package chimp

import io.circe.Json
import org.slf4j.LoggerFactory
import sttp.tapir.infallibleEndpoint
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint
import sttp.monad.MonadError
import sttp.monad.syntax.*

private val logger = LoggerFactory.getLogger(classOf[McpHandler[_]])

/** Creates a Tapir endpoint description, which will handle MCP HTTP server requests, using the provided tools.
  *
  * @tparam F
  *   The effect type. Might be `Identity` for a endpoints with synchronous logic.
  */
def mcpEndpoint[F[_]](tools: List[ServerTool[?, F]]): ServerEndpoint[Any, F] =
  val mcpHandler = new McpHandler(tools)
  val e = infallibleEndpoint.post
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
