package chimp.server.transport

import chimp.server.*
import io.circe.Json
import sttp.model.{Header, HeaderNames, StatusCode}
import sttp.monad.MonadError
import sttp.monad.syntax.*
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint

/** Implementation of unidirectional MCP server using Streamable HTTP, protected by the security logic of a [[SecuredMcpServer]]. The
  * security logic runs before any MCP message is handled. If it gives a rejection, the server sends the error output and no tool logic
  * runs. If it gives a principal, the principal goes to the tool logic.
  *
  * @param path
  *   The MCP endpoint path.
  */
final case class SecuredServerHttpTransport[F[_], S, E, P](path: List[String]):
  def serve(server: SecuredMcpServer[F, S, E, P]): ServerEndpoint[Any, F] =
    val handler = new McpHandler(server)
    val mcpEndpoint = endpoint.post
      .securityIn(server.securityInput)
      .in(path.foldLeft(emptyInput)((inputSoFar, pathComponent) => inputSoFar / pathComponent))
      .in(extractFromRequest(_.headers))
      .in(jsonBody[Json])
      .errorOut(server.errorOutput)
      .out(statusCode)
      .out(jsonBody[Option[Json]])

    ServerEndpoint(
      mcpEndpoint,
      server.securityLogic,
      me => { (principal: P) => (input: (Seq[Header], Json)) =>
        val (headers, json) = input
        given MonadError[F] = me
        val host = headers.find(_.name.equalsIgnoreCase(HeaderNames.Host)).map(_.value)
        val origin = headers.find(_.name.equalsIgnoreCase(HeaderNames.Origin)).map(_.value)
        if !server.originCheck.validate(host, origin) then me.unit(Right((StatusCode.Forbidden, None)))
        else
          handler
            .handleJsonRpc(json, headers, _ => SecuredServerContext[F, P](principal))
            .map(response => Right((response.statusCode, response.body)))
      }
    )
