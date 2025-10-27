package chimp

import chimp.protocol.*
import io.circe.*
import io.circe.syntax.*
import org.slf4j.LoggerFactory
import sttp.apispec.circe.*
import sttp.model.{Header, StatusCode}
import sttp.monad.MonadError
import sttp.monad.syntax.*
import sttp.tapir.docs.apispec.schema.TapirSchemaToJsonSchema

/** Represents different types of HTTP responses for JSON-RPC requests */
enum McpResponse:
  /** Response with JSON body (for requests and errors) */
  case JsonResponse(json: Json)

  /** Response with no body (for notifications) */
  case EmptyAcceptResponse

  def statusCode: StatusCode = this match
    case JsonResponse(_)     => StatusCode.Ok
    case EmptyAcceptResponse => StatusCode.Accepted

  def body: Option[Json] = this match
    case JsonResponse(json)  => Some(json)
    case EmptyAcceptResponse => None

/** The MCP server handles JSON-RPC requests for tool listing, invocation, and initialization.
  *
  * @param tools
  *   The list of available server tools.
  * @param name
  *   The server name (for protocol reporting).
  * @param version
  *   The server version (for protocol reporting).
  * @param showJsonSchemaMetadata
  *   Whether to include JSON Schema metadata (such as $schema) in the tool input schemas. Some agents do not recognize it, so it can be
  *   disabled.
  */
class McpHandler[F[_]](
    tools: List[ServerTool[?, F]],
    name: String,
    version: String,
    showJsonSchemaMetadata: Boolean
):
  private val logger = LoggerFactory.getLogger(classOf[McpHandler[_]])
  private val ProtocolVersion = "2025-03-26"
  private val toolsByName = tools.map(t => t.name -> t).toMap

  /** Converts a ServerTool to its protocol definition. */
  private def toolToDefinition(tool: ServerTool[?, F]): ToolDefinition =
    val jsonSchema =
      val base = TapirSchemaToJsonSchema(tool.inputSchema, markOptionsAsNullable = true)
      if showJsonSchemaMetadata then base
      else base.copy($schema = None)

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
    JSONRPCMessage.Error(id = id, error = JSONRPCErrorObject(code = code, message = message))

  private def handleInitialize(id: RequestId): JSONRPCMessage.Response =
    val capabilities = ServerCapabilities(tools = Some(ServerToolsCapability(listChanged = Some(false))))
    val result =
      InitializeResult(protocolVersion = ProtocolVersion, capabilities = capabilities, serverInfo = Implementation(name, version))
    JSONRPCMessage.Response(id = id, result = result.asJson)

  /** Handles the 'tools/list' JSON-RPC method, returning the list of available tools. */
  private def handleToolsList(id: RequestId): JSONRPCMessage.Response =
    JSONRPCMessage.Response(id = id, result = ListToolsResponse(toolDefs).asJson)

  /** Handles the 'tools/call' JSON-RPC method. Attempts to decode the tool name and arguments, then dispatches to the tool logic. Provides
    * detailed error messages for decode failures.
    */
  private def handleToolsCall(params: Option[io.circe.Json], id: RequestId, headers: Seq[Header])(using
      MonadError[F]
  ): F[JSONRPCMessage] =
    // Extract tool name and arguments in a functional, idiomatic way
    val toolNameOpt = params.flatMap(_.hcursor.downField("name").as[String].toOption)
    val argumentsOpt = params.flatMap(_.hcursor.downField("arguments").focus)
    (toolNameOpt, argumentsOpt) match
      case (Some(toolName), Some(args)) =>
        toolsByName.get(toolName) match
          case Some(tool) =>
            def inputSnippet = args.noSpaces.take(200) // for error reporting
            // Use Circe's Decoder for argument decoding
            tool.inputDecoder.decodeJson(args) match
              case Right(decodedInput) => handleDecodedInput(tool, decodedInput, id, headers)
              case Left(decodingError) =>
                protocolError(
                  id,
                  JSONRPCErrorCodes.InvalidParams.code,
                  s"Invalid arguments: ${decodingError.getMessage}. Input: $inputSnippet"
                ).unit
          case None => protocolError(id, JSONRPCErrorCodes.MethodNotFound.code, s"Unknown tool: $toolName").unit
      case (Some(toolName), None) =>
        protocolError(id, JSONRPCErrorCodes.InvalidParams.code, s"Missing arguments for tool: $toolName").unit
      case (None, _) =>
        protocolError(id, JSONRPCErrorCodes.InvalidParams.code, "Missing tool name").unit

  /** Handles a successfully decoded tool input, dispatching to the tool's logic. */
  private def handleDecodedInput[T](tool: ServerTool[T, F], decodedInput: T, id: RequestId, headers: Seq[Header])(using
      MonadError[F]
  ): F[JSONRPCMessage] =
    tool
      .logic(decodedInput, headers)
      .map:
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

  /** Handles a JSON-RPC request, dispatching to the appropriate handler. Logs requests and responses. */
  private def doHandleJsonRpc(request: Json, headers: Seq[Header])(using MonadError[F]): F[McpResponse] =
    request.as[JSONRPCMessage] match
      case Left(err) =>
        val errorResponse = protocolError(RequestId("null"), JSONRPCErrorCodes.ParseError.code, s"Parse error: ${err.message}")
        McpResponse.JsonResponse((errorResponse: JSONRPCMessage).asJson).unit
      case Right(JSONRPCMessage.Request(_, method, params: Option[io.circe.Json], id)) =>
        method match
          case "tools/list" =>
            val response = handleToolsList(id)
            McpResponse.JsonResponse((response: JSONRPCMessage).asJson).unit
          case "tools/call" =>
            handleToolsCall(params, id, headers).map { response =>
              McpResponse.JsonResponse((response: JSONRPCMessage).asJson)
            }
          case "initialize" =>
            val response = handleInitialize(id)
            McpResponse.JsonResponse((response: JSONRPCMessage).asJson).unit
          case other =>
            val errorResponse = protocolError(id, JSONRPCErrorCodes.MethodNotFound.code, s"Unknown method: $other")
            McpResponse.JsonResponse((errorResponse: JSONRPCMessage).asJson).unit
      case Right(JSONRPCMessage.BatchRequest(requests)) =>
        // For each sub-request, process as a single request using flatMap/fold (no .sequence)
        def processBatch(reqs: List[JSONRPCMessage], acc: List[JSONRPCMessage]): F[List[JSONRPCMessage]] =
          reqs match
            case Nil => acc.reverse.unit
            case head :: tail =>
              head match
                case JSONRPCMessage.Notification(_, _, _) =>
                  processBatch(tail, acc) // skip notifications
                case _ =>
                  doHandleJsonRpc((head: JSONRPCMessage).asJson, headers).flatMap { resp =>
                    resp match
                      case McpResponse.JsonResponse(json) =>
                        val msg = json
                          .as[JSONRPCMessage]
                          .getOrElse(
                            protocolError(RequestId("null"), JSONRPCErrorCodes.InternalError.code, "Failed to decode sub-response")
                          )
                        processBatch(tail, msg :: acc)
                      case McpResponse.EmptyAcceptResponse =>
                        processBatch(tail, acc) // skip notifications in batch
                  }
        processBatch(requests, Nil).map { responses =>
          // Per JSON-RPC spec, notifications (no id) should not be included in the response
          val filtered = responses.collect {
            case r @ JSONRPCMessage.Response(_, id, _) => r
            case e @ JSONRPCMessage.Error(_, id, _)    => e
          }
          val batchResponse = JSONRPCMessage.BatchResponse(filtered)
          McpResponse.JsonResponse((batchResponse: JSONRPCMessage).asJson)
        }
      case Right(notification: JSONRPCMessage.Notification) =>
        logger.debug(s"Received notification: ${notification.method}")
        // For notifications, return EmptyAcceptResponse to indicate no body should be sent
        McpResponse.EmptyAcceptResponse.unit
      case Right(_) =>
        val errorResponse = protocolError(RequestId("null"), JSONRPCErrorCodes.InvalidRequest.code, "Invalid request type")
        McpResponse.JsonResponse((errorResponse: JSONRPCMessage).asJson).unit
  end doHandleJsonRpc

  def handleJsonRpc(request: Json, headers: Seq[Header])(using MonadError[F]): F[McpResponse] =
    doHandleJsonRpc(request, headers).map: response =>
      logger.debug(s"Request: $request, response: ${response.statusCode}, body: ${response.body}")
      response
