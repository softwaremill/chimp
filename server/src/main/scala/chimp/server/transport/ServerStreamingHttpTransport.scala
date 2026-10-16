package chimp.server.transport

import chimp.protocol.ProgressToken
import chimp.server.*
import io.circe.Json
import sttp.capabilities.Streams
import sttp.model.Header
import sttp.monad.MonadError
import sttp.monad.syntax.*
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint

/** Abstract base for bidirectional MCP server using Streamable HTTP. Responds to JSON-RPC messages from an MCP client with a
  * Server-Sent-Event stream. Messages in the stream are interleaved with the final response on that stream.
  *
  * The extra type parameter `Caps` carries the streaming capability evidence required by the Tapir [[sttp.tapir.server.ServerEndpoint]] to
  * produce asynchronous stream of Server-Sent Events as response. An instance also serves as the streaming machinery which
  * [[SecuredServerStreamingHttpTransport]] needs from the same effect backend - its `path` plays no part in that, so the same instance, or
  * another one for the same `F`/`Caps`, can back both a plain [[StreamingMcpServer]] and a [[SecuredStreamingMcpServer]].
  *
  * @param path
  *   The MCP endpoint path.
  */
abstract class ServerStreamingHttpTransport[F[_], Caps](path: List[String]) extends StreamingServerTransport[F, ServerEndpoint[Caps, F]]:
  val streams: Streams[Caps]
  type EventStream
  def sseBody: StreamBodyIO[streams.BinaryStream, EventStream, Caps]
  def emptyStream: EventStream
  def eventStream(handle: OutboundSink[F] => F[Option[Json]]): F[EventStream]

  final def serve(server: StreamingMcpServer[F]): ServerEndpoint[Caps, F] =
    val handler = new McpHandler[F, StreamingServerContext[F]](server)
    val endpoint = infallibleEndpoint.post
      .in(path.foldLeft(emptyInput)((inputSoFar, pathComponent) => inputSoFar / pathComponent))
      .in(extractFromRequest(_.headers))
      .in(jsonBody[Json])
      .out(statusCode)
      .out(sseBody)

    ServerEndpoint.public(
      endpoint,
      me => { (input: (Seq[Header], Json)) =>
        val (headers, json) = input
        given MonadError[F] = me
        respondWithEventStream(server.originCheck, headers, this) { sink =>
          val makeContext: Option[ProgressToken] => StreamingServerContext[F] =
            token => SinkStreamingServerContext(sink, token)
          handler.handleJsonRpc(json, headers, makeContext).map(_.body)
        }.map(Right(_))
      }
    )
