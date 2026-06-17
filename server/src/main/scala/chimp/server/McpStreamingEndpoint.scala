package chimp.server

import chimp.protocol.ProgressToken
import io.circe.Json
import sttp.model.{Header, HeaderNames, StatusCode}
import sttp.monad.MonadError
import sttp.monad.syntax.*
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint

private[server] def buildStreamingEndpoint[F[_], S](
    server: StreamingMcpServer[F],
    streaming: McpServerStreaming[F, S],
    path: List[String]
): ServerEndpoint[S, F] =
  val handler = new McpHandler[F, StreamingServerContext[F]](server)
  val endpoint = infallibleEndpoint.post
    .in(path.foldLeft(emptyInput)((inputSoFar, pathComponent) => inputSoFar / pathComponent))
    .in(extractFromRequest(_.headers))
    .in(jsonBody[Json])
    .out(statusCode)
    .out(streaming.sseBody)

  ServerEndpoint.public(
    endpoint,
    me => { (input: (Seq[Header], Json)) =>
      val (headers, json) = input
      given MonadError[F] = me
      val host = headers.find(_.name.equalsIgnoreCase(HeaderNames.Host)).map(_.value)
      val origin = headers.find(_.name.equalsIgnoreCase(HeaderNames.Origin)).map(_.value)
      if !server.originCheck.validate(host, origin) then me.unit(Right((StatusCode.Forbidden, streaming.emptyStream)))
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
