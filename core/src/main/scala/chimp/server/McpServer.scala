package chimp.server

import chimp.ServerTool
import chimp.mcp.*
import io.circe.*
import io.circe.syntax.*
import sttp.apispec.circe.*
import sttp.tapir.*
import sttp.tapir.docs.apispec.schema.TapirSchemaToJsonSchema
import sttp.tapir.json.circe.*
import sttp.tapir.server.netty.sync.NettySyncServer
import org.slf4j.LoggerFactory

/** The MCP server handles JSON-RPC requests for tool listing, invocation, and initialization.
  *
  * @param tools
  *   The list of available server tools.
  * @param name
  *   The server name (for protocol reporting).
  * @param version
  *   The server version (for protocol reporting).
  */
class McpServer(tools: List[ServerTool[?]], name: String = "Chimp MCP server", version: String = "1.0.0"):
  /** SLF4J logger for this class. */
  private val logger = LoggerFactory.getLogger(classOf[McpServer])

  /** Converts a ServerTool to its protocol definition. */
  private def toolToDefinition(tool: ServerTool[?]): ToolDefinition =
    val jsonSchema = TapirSchemaToJsonSchema(tool.inputCodec.schema, markOptionsAsNullable = true)
    val json = jsonSchema.asJson
    ToolDefinition(
      name = tool.name,
      description = tool.description,
      inputSchema = json,
      annotations = tool.annotations
        .map(a => ToolAnnotations(a.title, a.readOnlyHint, a.destructiveHint, a.idempotentHint, a.openWorldHint))
    )

  private val toolDefs: List[ToolDefinition] = tools.map(toolToDefinition)

  private def protocolError(id: RequestId, code: Int, message: String): JSONRPCMessage.Error =
    logger.debug(s"Protocol error (id=$id, code=$code): $message")
    JSONRPCMessage.Error(
      id = id,
      error = JSONRPCErrorObject(
        code = code,
        message = message
      )
    )

  private def handleInitialize(id: RequestId): JSONRPCMessage.Response =
    val capabilities = ServerCapabilities(
      tools = Some(ServerToolsCapability(listChanged = Some(false)))
    )
    val result = InitializeResult(
      protocolVersion = "2025-03-26",
      capabilities = capabilities,
      serverInfo = Implementation(name, version)
    )
    JSONRPCMessage.Response(id = id, result = result.asJson)

  private def handleToolsList(id: RequestId): JSONRPCMessage.Response =
    val result = ListToolsResponse(toolDefs)
    JSONRPCMessage.Response(id = id, result = result.asJson)

  /** Handles the 'tools/call' JSON-RPC method. Attempts to decode the tool name and arguments, then dispatches to the tool logic. Provides
    * detailed error messages for decode failures.
    */
  private def handleToolsCall(params: Option[io.circe.Json], id: RequestId): JSONRPCMessage =
    val (toolNameOpt, argumentsOpt) = {
      var name: Option[String] = None
      var args: Option[io.circe.Json] = None
      params.foreach { p =>
        val cursor = p.asInstanceOf[io.circe.Json].hcursor
        name = cursor.downField("name").as[String].toOption
        args = cursor.downField("arguments").focus
      }
      (name, args)
    }
    toolNameOpt match
      case Some(toolName) =>
        tools.find(_.name == toolName) match
          case Some(tool) =>
            argumentsOpt match
              case Some(args) =>
                // this is not optimal, but otherwise we wouldn't be able to reuse tapir's decoding logic
                // (as we need to decode only part of the input)
                def inputSnippet = args.noSpaces.take(200) // for error reporting
                tool.inputCodec.decode(args.noSpaces) match
                  case sttp.tapir.DecodeResult.Value(decodedInput) =>
                    handleDecodedInput(tool, decodedInput, id)
                  case sttp.tapir.DecodeResult.Error(_, decodingError) =>
                    protocolError(
                      id,
                      JSONRPCErrorCodes.InvalidParams.code,
                      s"Invalid arguments: ${decodingError.getMessage}. Input: $inputSnippet"
                    )
                  case sttp.tapir.DecodeResult.Missing =>
                    protocolError(id, JSONRPCErrorCodes.InvalidParams.code, s"Missing arguments for tool: $toolName. Input: $inputSnippet")
                  case other =>
                    logger.debug(s"Unknown decode failure for tool: $toolName, $other. Input: $inputSnippet")
                    protocolError(
                      id,
                      JSONRPCErrorCodes.InvalidParams.code,
                      s"Unknown decode failure for tool: $toolName. Input: $inputSnippet"
                    )
              case None => protocolError(id, JSONRPCErrorCodes.InvalidParams.code, s"Missing arguments for tool: $toolName")
          case None => protocolError(id, JSONRPCErrorCodes.MethodNotFound.code, s"Unknown tool: $toolName")
      case None => protocolError(id, JSONRPCErrorCodes.InvalidParams.code, "Missing tool name")

  /** Handles a successfully decoded tool input, dispatching to the tool's logic. */
  private def handleDecodedInput[T](tool: ServerTool[T], decodedInput: T, id: RequestId): JSONRPCMessage =
    tool.logic(decodedInput) match
      case Right(result) =>
        val callResult = ToolCallResult(
          content = List(ToolContent.Text(text = result)),
          isError = false
        )
        JSONRPCMessage.Response(id = id, result = callResult.asJson)
      case Left(errorMsg) =>
        val callResult = ToolCallResult(
          content = List(ToolContent.Text(text = errorMsg)),
          isError = true
        )
        JSONRPCMessage.Response(id = id, result = callResult.asJson)

  /** Handles a JSON-RPC request, dispatching to the appropriate handler. Logs requests and responses.
    */
  def handleJsonRpc(request: Json): Json =
    logger.debug(s"Request: $request")
    val response: JSONRPCMessage = request.as[JSONRPCMessage] match
      case Left(err) => protocolError(RequestId("null"), JSONRPCErrorCodes.ParseError.code, s"Parse error: ${err.message}")
      case Right(JSONRPCMessage.Request(_, method, params: Option[io.circe.Json], id)) =>
        method match
          case "tools/list" => handleToolsList(id)
          case "tools/call" => handleToolsCall(params, id)
          case "initialize" => handleInitialize(id)
          case other        => protocolError(id, JSONRPCErrorCodes.MethodNotFound.code, s"Unknown method: $other")
      case Right(_) => protocolError(RequestId("null"), JSONRPCErrorCodes.InvalidRequest.code, "Invalid request type")
    logger.debug(s"Response: $response")
    response.asJson

object McpServer:
  def apply(tools: List[ServerTool[?]]): McpServer = new McpServer(tools)

  def main(args: Array[String]): Unit =
    // Example: pass an empty list or construct your tools elsewhere
    val server = McpServer(List())
    val jsonRpcEndpoint = endpoint.post
      .in("jsonrpc")
      .in(jsonBody[Json])
      .out(jsonBody[Json])

    val serverEndpoint = jsonRpcEndpoint.handleSuccess { json =>
      println("Request: " + json)
      val r = server.handleJsonRpc(json).deepDropNullValues
      println("Response: " + r)
      r
    }

    println("Starting MCP server on http://localhost:8080 ...")
    NettySyncServer()
      .port(8080)
      .addEndpoint(serverEndpoint)
      .startAndWait()
