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
  * @param headerName
  *   The optional name of the header to read. If None, no header is read.
  *
  * @tparam F
  *   The effect type. Might be `Identity` for a endpoints with synchronous logic.
  */
def mcpEndpoint[F[_]](tools: List[ServerTool[?, F]], path: List[String], headerName: Option[String] = None): ServerEndpoint[Any, F] =
  val mcpHandler = new McpHandler(tools)
  val base = infallibleEndpoint.post
    .in(path.foldLeft(emptyInput)((inputSoFar, pathComponent) => inputSoFar / pathComponent))
    .in(jsonBody[Json])
    .out(jsonBody[Json])

  headerName match {
    case Some(name) =>
      val endpoint = base.prependIn(header[Option[String]](name))
      ServerEndpoint.public(
        endpoint,
        me => { (input: (Option[String], Json)) =>
          val (headerValue, json) = input
          given MonadError[F] = me
          mcpHandler
            .handleJsonRpc(json, headerValue)
            .map(responseJson => Right(responseJson.deepDropNullValues))
        }
      )
    case None =>
      ServerEndpoint.public(
        base,
        me => { (json: Json) =>
          given MonadError[F] = me
          mcpHandler
            .handleJsonRpc(json, None)
            .map(responseJson => Right(responseJson.deepDropNullValues))
        }
      )
  }
