package chimp.server

import chimp.protocol.*
import io.circe.*
import io.circe.syntax.*
import org.slf4j.LoggerFactory
import sttp.apispec.circe.*
import sttp.model.{Header, StatusCode}
import sttp.monad.MonadError
import sttp.monad.syntax.*
import sttp.tapir.docs.apispec.schema.TapirSchemaToJsonSchema

enum McpResponse:
  case JsonResponse(json: Json)
  case EmptyAcceptResponse

  def statusCode: StatusCode = this match
    case JsonResponse(_)     => StatusCode.Ok
    case EmptyAcceptResponse => StatusCode.Accepted

  def body: Option[Json] = this match
    case JsonResponse(json)  => Some(json)
    case EmptyAcceptResponse => None

  def withNullsDroppedDeep: McpResponse = this match
    case JsonResponse(json)  => JsonResponse(json.deepDropNullValues)
    case EmptyAcceptResponse => this

class McpHandler[F[_], C <: ServerContext[F]](server: McpServerDef[F, C]):
  private val logger = LoggerFactory.getLogger(classOf[McpHandler[?, ?]])
  private val toolsByName = server.tools.map(t => t.name -> t).toMap
  private val promptsByName = server.prompts.map(p => p.definition.name -> p).toMap
  private val resourcesByUri = server.resources.map(r => r.definition.uri -> r).toMap
  private val hasResources = server.resources.nonEmpty || server.resourceTemplates.nonEmpty
  private val toolDefinitions = server.tools.map(toolToDefinition)

  private def toolToDefinition(tool: ServerTool[?, F, C]): ToolDefinition =
    val jsonSchema = tool.inputSchema match
      case ToolSchema.Derived(schema) =>
        val base = TapirSchemaToJsonSchema(schema, markOptionsAsNullable = false)
        (if server.showJsonSchemaMetadata then base else base.copy($schema = None)).asJson
      case ToolSchema.Raw(json) => json
    ToolDefinition(
      name = tool.name,
      description = tool.description,
      inputSchema = jsonSchema,
      annotations = tool.annotations
        .map(a => ToolAnnotations(a.title, a.readOnlyHint, a.destructiveHint, a.idempotentHint, a.openWorldHint))
    )

  private def protocolError(id: RequestId, code: Int, message: String, data: Option[Json] = None): JSONRPCMessage.Error =
    logger.debug(s"Protocol error (id=$id, code=$code): $message")
    JSONRPCMessage.Error(id = id, error = JSONRPCErrorObject(code = code, message = message, data = data))

  private def jsonResponse(message: JSONRPCMessage): McpResponse = McpResponse.JsonResponse(message.asJson)

  private def emptyResult(id: RequestId): JSONRPCMessage = JSONRPCMessage.Response(id = id, result = Json.obj())

  private def decodeParams[P: Decoder](params: Option[Json], id: RequestId)(f: P => F[JSONRPCMessage])(using
      MonadError[F]
  ): F[JSONRPCMessage] =
    params.flatMap(_.as[P].toOption) match
      case Some(p) => f(p)
      case None    => protocolError(id, JSONRPCErrorCodes.InvalidParams.code, "Invalid or missing params").unit

  private def handleInitialize(params: Option[Json], id: RequestId): JSONRPCMessage.Response =
    val requested = params.flatMap(_.hcursor.downField("protocolVersion").as[String].toOption)
    val negotiated = requested.map(ProtocolVersion.negotiate).getOrElse(ProtocolVersion.Latest)
    val capabilities = ServerCapabilities(
      logging = Option.when(server.setLevel.isDefined)(Json.obj()),
      completions = Option.when(server.completion.isDefined)(Json.obj()),
      prompts = Option.when(server.prompts.nonEmpty)(ServerPromptsCapability(listChanged = Some(false))),
      resources =
        Option.when(hasResources)(ServerResourcesCapability(subscribe = Some(server.subscriptions.isDefined), listChanged = Some(false))),
      tools = Option.when(server.tools.nonEmpty)(ServerToolsCapability(listChanged = Some(false)))
    )
    val result = InitializeResult(
      protocolVersion = negotiated.name,
      capabilities = capabilities,
      serverInfo = Implementation(server.name, server.version),
      instructions = server.instructions
    )
    JSONRPCMessage.Response(id = id, result = result.asJson)

  private def handleToolsCall(params: Option[Json], id: RequestId, headers: Seq[Header], makeContext: Option[ProgressToken] => C)(using
      MonadError[F]
  ): F[JSONRPCMessage] =
    val name = params.flatMap(_.hcursor.downField("name").as[String].toOption)
    val args = params.flatMap(_.hcursor.downField("arguments").focus).getOrElse(Json.obj())
    val progressToken = params.flatMap(_.hcursor.downField("_meta").downField("progressToken").as[ProgressToken].toOption)
    name match
      case Some(name) =>
        toolsByName.get(name) match
          case Some(tool) =>
            tool.inputDecoder.decodeJson(args) match
              case Right(input) =>
                val context = makeContext(progressToken)
                tool
                  .logic(input, context, headers)
                  .map: result =>
                    JSONRPCMessage.Response(
                      id = id,
                      result = CallToolResult(
                        content = result.content,
                        structuredContent = result.structuredContent,
                        isError = result.isError
                      ).asJson
                    )
              case Left(decodingError) =>
                val snippet = args.noSpaces.take(200)
                protocolError(
                  id,
                  JSONRPCErrorCodes.InvalidParams.code,
                  s"Invalid arguments: ${decodingError.getMessage}. Input: $snippet"
                ).unit
          case None => protocolError(id, JSONRPCErrorCodes.MethodNotFound.code, s"Unknown tool: $name").unit
      case None =>
        protocolError(id, JSONRPCErrorCodes.InvalidParams.code, "Missing tool name").unit

  private def handleResourcesRead(params: Option[Json], id: RequestId)(using MonadError[F]): F[JSONRPCMessage] =
    decodeParams[ReadResourceParams](params, id): params =>
      resourcesByUri.get(params.uri) match
        case Some(resource) => resource.read().map(readResponse(id, params.uri))
        case None           =>
          val templateMatch = server.resourceTemplates.iterator
            .map(template => (template, template.matcher.matchUri(params.uri)))
            .collectFirst { case (template, Some(vars)) => (template, vars) }
          templateMatch match
            case Some((template, vars)) => template.read(vars, params.uri).map(readResponse(id, params.uri))
            case None                   =>
              protocolError(
                id,
                JSONRPCErrorCodes.InvalidParams.code,
                s"Resource not found: ${params.uri}",
                Some(Json.obj("uri" -> Json.fromString(params.uri)))
              ).unit

  private def readResponse(id: RequestId, uri: String)(result: Either[ResourceError, List[ResourceContents]]): JSONRPCMessage =
    result match
      case Right(contents) => JSONRPCMessage.Response(id = id, result = ReadResourceResult(contents).asJson)
      case Left(error) =>
        protocolError(
          id,
          JSONRPCErrorCodes.InvalidParams.code,
          error.message,
          error.uri.orElse(Some(uri)).map(u => Json.obj("uri" -> Json.fromString(u)))
        )

  private def handleSubscribe(params: Option[Json], id: RequestId, subscribe: Boolean)(using MonadError[F]): F[JSONRPCMessage] =
    val subs = server.subscriptions.get
    if subscribe then decodeParams[SubscribeParams](params, id)(p => subs.onSubscribe(p).map(_ => emptyResult(id)))
    else decodeParams[UnsubscribeParams](params, id)(p => subs.onUnsubscribe(p).map(_ => emptyResult(id)))

  private def handlePromptsGet(params: Option[Json], id: RequestId, headers: Seq[Header])(using MonadError[F]): F[JSONRPCMessage] =
    decodeParams[GetPromptParams](params, id): p =>
      promptsByName.get(p.name) match
        case Some(prompt) =>
          prompt.logic(p.arguments.getOrElse(Map.empty), headers).map(result => JSONRPCMessage.Response(id = id, result = result.asJson))
        case None => protocolError(id, JSONRPCErrorCodes.InvalidParams.code, s"Unknown prompt: ${p.name}").unit

  private def handleComplete(params: Option[Json], id: RequestId)(using MonadError[F]): F[JSONRPCMessage] =
    val handler = server.completion.get
    decodeParams[CompleteParams](params, id): p =>
      handler(p.ref, p.argument, p.context).map(completion => JSONRPCMessage.Response(id = id, result = CompleteResult(completion).asJson))

  private def handleSetLevel(params: Option[Json], id: RequestId)(using MonadError[F]): F[JSONRPCMessage] =
    val handler = server.setLevel.get
    decodeParams[SetLevelParams](params, id)(p => handler(p.level).map(_ => emptyResult(id)))

  private def doHandleJsonRpc(request: Json, headers: Seq[Header], makeContext: Option[ProgressToken] => C)(using
      MonadError[F]
  ): F[McpResponse] =
    request.as[JSONRPCMessage] match
      case Left(err) =>
        val errorResponse = protocolError(RequestId("null"), JSONRPCErrorCodes.ParseError.code, s"Parse error: ${err.message}")
        jsonResponse(errorResponse).unit
      case Right(JSONRPCMessage.Request(_, method, params: Option[Json], id)) =>
        method match
          case "initialize" =>
            jsonResponse(handleInitialize(params, id)).unit
          case "ping" =>
            jsonResponse(JSONRPCMessage.Response(id = id, result = Json.obj())).unit
          case "tools/list" =>
            jsonResponse(JSONRPCMessage.Response(id = id, result = ListToolsResponse(toolDefinitions).asJson)).unit
          case "tools/call" =>
            handleToolsCall(params, id, headers, makeContext).map(jsonResponse)
          case "resources/list" if hasResources =>
            jsonResponse(JSONRPCMessage.Response(id = id, result = ListResourcesResult(server.resources.map(_.definition)).asJson)).unit
          case "resources/templates/list" if hasResources =>
            jsonResponse(
              JSONRPCMessage.Response(id = id, result = ListResourceTemplatesResult(server.resourceTemplates.map(_.definition)).asJson)
            ).unit
          case "resources/read" if hasResources =>
            handleResourcesRead(params, id).map(jsonResponse)
          case "resources/subscribe" if server.subscriptions.isDefined =>
            handleSubscribe(params, id, subscribe = true).map(jsonResponse)
          case "resources/unsubscribe" if server.subscriptions.isDefined =>
            handleSubscribe(params, id, subscribe = false).map(jsonResponse)
          case "prompts/list" if server.prompts.nonEmpty =>
            jsonResponse(JSONRPCMessage.Response(id = id, result = ListPromptsResult(server.prompts.map(_.definition)).asJson)).unit
          case "prompts/get" if server.prompts.nonEmpty =>
            handlePromptsGet(params, id, headers).map(jsonResponse)
          case "completion/complete" if server.completion.isDefined =>
            handleComplete(params, id).map(jsonResponse)
          case "logging/setLevel" if server.setLevel.isDefined =>
            handleSetLevel(params, id).map(jsonResponse)
          case other =>
            jsonResponse(protocolError(id, JSONRPCErrorCodes.MethodNotFound.code, s"Unknown method: $other")).unit
      case Right(notification: JSONRPCMessage.Notification) =>
        logger.debug(s"Received notification: ${notification.method}")
        McpResponse.EmptyAcceptResponse.unit
      case Right(_) =>
        jsonResponse(protocolError(RequestId("null"), JSONRPCErrorCodes.InvalidRequest.code, "Invalid request type")).unit
  end doHandleJsonRpc

  def handleJsonRpc(request: Json, headers: Seq[Header], makeContext: Option[ProgressToken] => C)(using MonadError[F]): F[McpResponse] =
    doHandleJsonRpc(request, headers, makeContext).map: response =>
      logger.debug(s"Request: $request, response: ${response.statusCode}, body: ${response.body}")
      response.withNullsDroppedDeep

  def handleJsonRpc(request: Json, headers: Seq[Header])(using m: MonadError[F], ev: ServerContext[F] <:< C): F[McpResponse] =
    handleJsonRpc(request, headers, _ => ev(ServerContext.noop[F]))
