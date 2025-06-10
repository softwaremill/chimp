package chimp

import io.circe.Json
import org.slf4j.LoggerFactory
import sttp.monad.MonadError
import sttp.monad.syntax.*
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint

private val logger = LoggerFactory.getLogger(classOf[McpHandler[_]])

/** Creates a Tapir endpoint description, which will handle MCP HTTP server requests, using the provided tools.
  *
  * @param tools
  *   The list of tools to expose.
  * @param path
  *   The path components at which to expose the MCP server.
  *
  * @tparam F
  *   The effect type. Might be `Identity` for a endpoints with synchronous logic.
  */
def mcpEndpoint[F[_]](tools: List[ServerTool[?, F]], path: List[String]): ServerEndpoint[Any, F] =
  val mcpHandler = new McpHandler(tools)
  val e = infallibleEndpoint.post
    .in(path.foldLeft(emptyInput)((inputSoFar, pathComponent) => inputSoFar / pathComponent))
    .in(jsonBody[Json])
    .out(jsonBody[Json])

  ServerEndpoint.public(
    e,
    me =>
      json =>
        given MonadError[F] = me
        mcpHandler
          .handleJsonRpc(json)
          .map: responseJson =>
            Right(responseJson.deepDropNullValues)
  )
