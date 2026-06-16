package chimp.server

import io.circe.Json
import sttp.model.{Header, HeaderNames, StatusCode}
import sttp.monad.MonadError
import sttp.monad.syntax.*
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint

private[server] def buildEndpoint[F[_]](server: McpServer[F], path: List[String]): ServerEndpoint[Any, F] =
  val mcpHandler = new McpHandler(server)
  val endpoint = infallibleEndpoint.post
    .in(path.foldLeft(emptyInput)((inputSoFar, pathComponent) => inputSoFar / pathComponent))
    .in(extractFromRequest(_.headers))
    .in(jsonBody[Json])
    .out(statusCode)
    .out(jsonBody[Option[Json]])

  ServerEndpoint.public(
    endpoint,
    me => { (input: (Seq[Header], Json)) =>
      val (headers, json) = input
      given MonadError[F] = me
      val host = headers.find(_.name.equalsIgnoreCase(HeaderNames.Host)).map(_.value)
      val origin = headers.find(_.name.equalsIgnoreCase(HeaderNames.Origin)).map(_.value)
      if !server.originCheck.validate(host, origin) then me.unit(Right((StatusCode.Forbidden, None)))
      else
        mcpHandler
          .handleJsonRpc(json, headers)
          .map(response => Right((response.statusCode, response.body)))
    }
  )
