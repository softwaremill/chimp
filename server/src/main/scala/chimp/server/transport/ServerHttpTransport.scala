package chimp.server.transport

import chimp.server.*
import io.circe.Json
import sttp.model.Header
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
final case class ServerHttpTransport[F[_]](path: List[String]) extends ServerTransport[F, ServerEndpoint[Any, F]]:
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
        respondToJsonRpc(server.originCheck, headers)(handler.handleJsonRpc(json, headers)).map(Right(_))
      }
    )
