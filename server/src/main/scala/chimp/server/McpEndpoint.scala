package chimp.server

import io.circe.Json
import sttp.monad.MonadError
import sttp.monad.syntax.*
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint
import sttp.model.Header

private[server] def buildEndpoint[F[_]](server: McpServer[F], path: List[String]): ServerEndpoint[Any, F] =
  val mcpHandler = new McpHandler(server)
  val e = infallibleEndpoint.post
    .in(path.foldLeft(emptyInput)((inputSoFar, pathComponent) => inputSoFar / pathComponent))
    .in(extractFromRequest(_.headers))
    .in(jsonBody[Json])
    .out(statusCode)
    .out(jsonBody[Option[Json]])

  ServerEndpoint.public(
    e,
    me => { (input: (Seq[Header], Json)) =>
      val (headers, json) = input
      given MonadError[F] = me
      mcpHandler
        .handleJsonRpc(json, headers)
        .map(response => Right((response.statusCode, response.body)))
    }
  )
