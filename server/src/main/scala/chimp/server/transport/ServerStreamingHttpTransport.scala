package chimp.server.transport

import chimp.server.*
import io.circe.Json
import sttp.capabilities.Streams
import sttp.model.{Header, StatusCode}
import sttp.monad.MonadError
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

  /** Runs `handle`, which sends its messages to the sink and ends with the response. If it ends with an error which has its own HTTP status
    * (see [[plainJsonError]]) before it sends any message, that error is returned on the left, to be sent as plain JSON. Otherwise the
    * messages and then the response's body go on a 200 event stream.
    */
  def eventStream(handle: OutboundSink[F] => F[McpResponse]): F[Either[(StatusCode, Json), EventStream]]

  /** The stream of an accepted `subscriptions/listen` request. It opens the subscription when the stream starts, and cancels it when the
    * client closes the stream. The server ends it by closing the subscription's output.
    */
  private[server] def subscriptionStream(listen: ListenRequest): EventStream

  final def serve(server: StreamingMcpServer[F]): ServerEndpoint[Caps, F] =
    val handler = new McpHandler[F, StreamingServerContext[F]](server, canListen = true)
    val endpoint = sttp.tapir.endpoint.post
      .in(path.foldLeft(emptyInput)((inputSoFar, pathComponent) => inputSoFar / pathComponent))
      .in(extractFromRequest(_.headers))
      .in(jsonBody[Json])
      .errorOut(jsonRejectionOutput)
      .out(statusCode)
      .out(sseBody)

    ServerEndpoint.public(
      endpoint,
      me => { (input: (Seq[Header], Json)) =>
        val (headers, json) = input
        given MonadError[F] = me
        respondWithEventStream(
          server.originCheck,
          headers,
          this,
          handler.validateRequest(json, headers),
          handler.listenRequest(json, headers)
        ) { sink =>
          val makeContext: RequestInfo => StreamingServerContext[F] =
            info => SinkStreamingServerContext(sink, info)
          handler.handleJsonRpc(json, headers, makeContext)
        }
      }
    )
