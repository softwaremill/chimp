package chimp.server.transport

import chimp.protocol.ProgressToken
import chimp.server.*
import io.circe.Json
import sttp.capabilities.Streams
import sttp.model.{Header, HeaderNames, StatusCode}
import sttp.monad.MonadError
import sttp.monad.syntax.*
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint

/** Implementation of bidirectional MCP server using Streamable HTTP.
  * Responds to JSON-RPC messages from an MCP client with a Server-Sent-Event stream.
  * Messages in the stream are interleaved with the final response on that stream.
  *
  * @param path
  *   The MCP endpoint path.
  **/
abstract class StreamingHttpServerTransport[F[_], S](path: List[String]) extends StreamingServerTransport[F, ServerEndpoint[S, F]]:
  val streams: Streams[S]
  type EventStream
  def sseBody: StreamBodyIO[streams.BinaryStream, EventStream, S]
  def emptyStream: EventStream
  def eventStream(handle: OutboundSink[F] => F[Option[Json]]): F[EventStream]

  final def serve(server: StreamingMcpServer[F]): ServerEndpoint[S, F] =
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
        val host = headers.find(_.name.equalsIgnoreCase(HeaderNames.Host)).map(_.value)
        val origin = headers.find(_.name.equalsIgnoreCase(HeaderNames.Origin)).map(_.value)
        if !server.originCheck.validate(host, origin) then me.unit(Right((StatusCode.Forbidden, emptyStream)))
        else
          eventStream { sink =>
            val makeContext: Option[ProgressToken] => StreamingServerContext[F] =
              token => SinkStreamingServerContext(sink, token)
            handler.handleJsonRpc(json, headers, makeContext).map(_.body)
          }.map(events => Right((StatusCode.Ok, events)))
      }
    )
