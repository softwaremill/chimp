package chimp.server.transport

import chimp.server.*
import io.circe.Json
import sttp.model.{Header, HeaderNames, StatusCode}
import sttp.monad.MonadError
import sttp.monad.syntax.*
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint

/** Implementation of unidirectional MCP server using Streamable HTTP. Responds to JSON-RPC messages from an MCP client with a single
  * JSON-RPC message response.
  *
  * @param path
  *   The MCP endpoint path.
  */
final case class HttpServerTransport[F[_]](path: List[String]) extends ServerTransport[F, ServerEndpoint[Any, F]]:
  def serve(server: McpServer[F]): ServerEndpoint[Any, F] =
    val handler = new McpHandler(server)
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
          handler
            .handleJsonRpc(json, headers)
            .map(response => Right((response.statusCode, response.body)))
      }
    )
