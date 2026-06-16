package chimp.server

import chimp.protocol.ProgressToken
import io.circe.Json
import sttp.monad.MonadError
import sttp.monad.syntax.*
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint
import sttp.model.{Header, StatusCode}

/** Builds the SSE-capable endpoint: a POST that responds with `text/event-stream`. Progress and log notifications emitted by the tool logic
  * during the call are streamed as events, followed by the final JSON-RPC response event. A request rejected by the [[OriginCheck]] gets a
  * `403` with an empty stream.
  */
private[server] def buildStreamingEndpoint[F[_], S](
    server: StreamingMcpServer[F],
    streaming: McpStreaming[F, S],
    path: List[String]
): ServerEndpoint[S, F] =
  val handler = new McpHandler[F, StreamingServerContext[F]](server)
  val e = infallibleEndpoint.post
    .in(path.foldLeft(emptyInput)((inputSoFar, pathComponent) => inputSoFar / pathComponent))
    .in(extractFromRequest(_.headers))
    .in(jsonBody[Json])
    .out(statusCode)
    .out(streaming.sseBody)

  ServerEndpoint.public(
    e,
    me => { (input: (Seq[Header], Json)) =>
      val (headers, json) = input
      given MonadError[F] = me
      val host = headers.find(_.name.equalsIgnoreCase("Host")).map(_.value)
      val origin = headers.find(_.name.equalsIgnoreCase("Origin")).map(_.value)
      if !server.originCheck.validate(host, origin) then me.unit(Right((StatusCode.Forbidden, streaming.emptyEvents)))
      else
        streaming
          .eventStream { sink =>
            val makeContext: Option[ProgressToken] => StreamingServerContext[F] =
              token => SinkStreamingServerContext(sink, token)
            handler.handleJsonRpc(json, headers, makeContext).map(_.body)
          }
          .map(events => Right((StatusCode.Ok, events)))
    }
  )
