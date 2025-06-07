package chimp

import io.circe.Json
import org.slf4j.LoggerFactory
import ox.tap
import sttp.shared.Identity
import sttp.tapir.endpoint
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint

private val logger = LoggerFactory.getLogger(classOf[McpHandler])

/** Creates a Tapir endpoint description, which will handle MCP HTTP server requests, using the provided tools. */
def mcpEndpoint(tools: List[ServerTool[?]]): ServerEndpoint[Any, Identity] =
  val mcpEndpoint = new McpHandler(tools)
  endpoint.post
    .in(jsonBody[Json])
    .out(jsonBody[Json])
    .handleSuccess: json =>
      mcpEndpoint
        .handleJsonRpc(json)
        .deepDropNullValues
        .tap: response =>
          logger.debug(s"For request: $json, returning response: $response")
