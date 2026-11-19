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
  case JsonError(status: StatusCode, json: Json)
  case EmptyAcceptResponse

  def statusCode: StatusCode = this match
    case JsonResponse(_)      => StatusCode.Ok
    case JsonError(status, _) => status
    case EmptyAcceptResponse  => StatusCode.Accepted

  def body: Option[Json] = this match
    case JsonResponse(json)  => Some(json)
    case JsonError(_, json)  => Some(json)
    case EmptyAcceptResponse => None

  def withNullsDroppedDeep: McpResponse = this match
    case JsonResponse(json)      => JsonResponse(json.deepDropNullValues)
    case JsonError(status, json) => JsonError(status, json.deepDropNullValues)
    case EmptyAcceptResponse     => this

/** @param canListen
  *   Whether the transport can hold a `subscriptions/listen` stream open. Without it the server neither serves `subscriptions/listen` nor
  *   advertises the capabilities which need it.
  */
private[server] class McpHandler[F[_], C <: ServerContext[F]](server: McpServerDef[F, C], canListen: Boolean = false):
  private val logger = LoggerFactory.getLogger(classOf[McpHandler[?, ?]])
  private val listenPublisher = server.publisher.filter(_ => canListen)
  private val toolsByName = server.tools.map(tool => tool.name -> tool).toMap
  private val promptsByName = server.prompts.map(prompt => prompt.definition.name -> prompt).toMap
  private val resourcesByUri = server.resources.map(resource => resource.definition.uri -> resource).toMap
  private val hasResources = server.resources.nonEmpty || server.resourceTemplates.nonEmpty
  private val hasPrompts = server.prompts.nonEmpty
  private val toolDefinitions = server.tools.map(toolToDefinition)

  private def toJsonSchema(toolSchema: ToolSchema): Json = toolSchema match
    case ToolSchema.Derived(schema) =>
      val base = TapirSchemaToJsonSchema(schema, markOptionsAsNullable = false)
      (if server.showJsonSchemaMetadata then base else base.copy($schema = None)).asJson
    case ToolSchema.Raw(json) => json

  private def toolToDefinition(tool: ServerTool[?, ?, F, C]): ToolDefinition =
    ToolDefinition(
      name = tool.name,
      description = tool.description,
      inputSchema = toJsonSchema(tool.inputSchema),
      outputSchema = tool.outputSchema.map(toJsonSchema),
      annotations = tool.annotations
        .map(annotation =>
          ToolAnnotations(
            annotation.title,
            annotation.readOnlyHint,
            annotation.destructiveHint,
            annotation.idempotentHint,
            annotation.openWorldHint
          )
        )
    )

  def handleJsonRpc(request: Json, headers: Seq[Header], makeContext: RequestInfo => C)(using MonadError[F]): F[McpResponse] =
    doHandleJsonRpc(request, headers, makeContext).map: response =>
      logger.debug(s"Request: $request, response: ${response.statusCode}, body: ${response.body.getOrElse(Json.Null)}")
      response.withNullsDroppedDeep

  def handleJsonRpc(request: Json, headers: Seq[Header])(using m: MonadError[F], ev: ServerContext[F] <:< C): F[McpResponse] =
    handleJsonRpc(request, headers, info => ev(ServerContext[F](info)))

  private def doHandleJsonRpc(request: Json, headers: Seq[Header], makeContext: RequestInfo => C)(using
      MonadError[F]
  ): F[McpResponse] =
    request.as[JSONRPCMessage] match
      case Left(err) =>
        jsonResponse(protocolError(RequestId("null"), JSONRPCErrorCodes.ParseError.code, s"Parse error: ${err.message}")).unit
      case Right(JSONRPCMessage.Request(_, method, params: Option[Json], id)) =>
        requestRejection(method, params, headers, id) match
          case Some(rejection) => rejection.unit
          case None            =>
            val isModern = isModernRequest(params, headers)
            val info = requestInfo(params, headers, isModern)
            dispatch(method, params, id, headers, info, makeContext, isModern)
      case Right(notification: JSONRPCMessage.Notification) =>
        logger.debug(s"Received notification: ${notification.method}")
        McpResponse.EmptyAcceptResponse.unit
      case Right(_) =>
        jsonResponse(protocolError(RequestId("null"), JSONRPCErrorCodes.InvalidRequest.code, "Invalid request type")).unit
  end doHandleJsonRpc

  private def dispatch(
      method: String,
      params: Option[Json],
      id: RequestId,
      headers: Seq[Header],
      info: RequestInfo,
      makeContext: RequestInfo => C,
      isModern: Boolean
  )(using MonadError[F]): F[McpResponse] =
    def wrap(cacheHints: Option[CacheHints])(message: JSONRPCMessage): JSONRPCMessage = message match
      case JSONRPCMessage.Response(jsonrpc, rid, result) if isModern =>
        JSONRPCMessage.Response(jsonrpc, rid, modernResult(result, cacheHints))
      case other => other
    def reply(cacheHints: Option[CacheHints])(reply: Reply): McpResponse =
      reply.fold(identity, message => jsonResponse(wrap(cacheHints)(message)))
    val cacheHints = Some(CacheHints.Default)
    method match
      case "initialize" =>
        jsonResponse(handleInitialize(params, id)).unit
      case "server/discover" =>
        jsonResponse(handleDiscover(id)).unit
      case "ping" =>
        jsonResponse(wrap(None)(JSONRPCMessage.Response(id = id, result = Json.obj()))).unit
      case "tools/list" =>
        jsonResponse(wrap(cacheHints)(JSONRPCMessage.Response(id = id, result = ListToolsResponse(toolDefinitions).asJson))).unit
      case "tools/call" =>
        handleToolsCall(params, id, headers, info, makeContext, isModern).map(reply(None))
      case "resources/list" if hasResources =>
        jsonResponse(
          wrap(cacheHints)(JSONRPCMessage.Response(id = id, result = ListResourcesResult(server.resources.map(_.definition)).asJson))
        ).unit
      case "resources/templates/list" if hasResources =>
        jsonResponse(
          wrap(cacheHints)(
            JSONRPCMessage.Response(id = id, result = ListResourceTemplatesResult(server.resourceTemplates.map(_.definition)).asJson)
          )
        ).unit
      case "resources/read" if hasResources =>
        handleResourcesRead(params, id, headers, info, makeContext, isModern).map(reply(cacheHints))
      case "resources/subscribe" if server.subscriptions.isDefined =>
        handleSubscribe(params, id, subscribe = true).map(jsonResponse)
      case "resources/unsubscribe" if server.subscriptions.isDefined =>
        handleSubscribe(params, id, subscribe = false).map(jsonResponse)
      case "prompts/list" if hasPrompts =>
        jsonResponse(
          wrap(cacheHints)(JSONRPCMessage.Response(id = id, result = ListPromptsResult(server.prompts.map(_.definition)).asJson))
        ).unit
      case "prompts/get" if hasPrompts =>
        handlePromptsGet(params, id, headers, info, makeContext, isModern).map(reply(None))
      case "completion/complete" if server.completion.isDefined =>
        handleComplete(params, id).map(wrap(None)).map(jsonResponse)
      case "logging/setLevel" if server.loggingLevel.isDefined =>
        handleSetLoggingLevel(params, id).map(jsonResponse)
      case "subscriptions/listen" if listenPublisher.isDefined =>
        jsonResponse(protocolError(id, JSONRPCErrorCodes.InvalidParams.code, "Invalid or missing params")).unit
      case other =>
        jsonResponse(protocolError(id, JSONRPCErrorCodes.MethodNotFound.code, s"Unknown method: $other")).unit

  /** Adds the modern envelope fields to a result. An input-required result already names its type and is never cached. */
  private def modernResult(result: Json, cacheHints: Option[CacheHints]): Json =
    val meta = result.hcursor.downField("_meta").focus.getOrElse(Json.obj()).deepMerge(serverInfoMeta)
    if result.hcursor.get[ResultType]("resultType").contains(ResultType.InputRequired) then result.deepMerge(Json.obj("_meta" -> meta))
    else
      val enriched = result.deepMerge(Json.obj("resultType" -> ResultType.Complete.asJson, "_meta" -> meta))
      cacheHints.fold(enriched)(hints => enriched.deepMerge(hints.asJson))

  private def protocolError(id: RequestId, code: Int, message: String, data: Option[Json] = None): JSONRPCMessage.Error =
    logger.debug(s"Protocol error (id=$id, code=$code): $message")
    JSONRPCMessage.Error(id = id, error = JSONRPCErrorObject(code = code, message = message, data = data))

  private def jsonResponse(message: JSONRPCMessage): McpResponse = McpResponse.JsonResponse(message.asJson)

  /** The reply of a handler which may end with [[InputRequired]]: a JSON-RPC message, or a response with its own HTTP status. */
  private type Reply = Either[McpResponse, JSONRPCMessage]

  /** Applies the server-enforced input-required rules: only a modern request can get it, and only for the input requests which the client
    * declared the capabilities for (otherwise a `-32021` with HTTP 400).
    */
  private def inputRequiredReply(id: RequestId, outcome: InputRequired, info: RequestInfo, isModern: Boolean): Reply =
    if !isModern then
      Right(
        protocolError(
          id,
          JSONRPCErrorCodes.InternalError.code,
          s"The handler asked for client input, which needs protocol version ${ProtocolVersion.V2026_07_28.name} or later"
        )
      )
    else
      val declared = info.clientCapabilities.getOrElse(ClientCapabilities())
      val undeclared = outcome.requests.values.filterNot(InputRequired.isDeclared(_, declared))
      if undeclared.nonEmpty then
        val required = InputRequired.requiredCapabilities(undeclared)
        logger.debug(s"Input required rejected (id=$id): missing client capabilities ${required.asJson.deepDropNullValues.noSpaces}")
        val envelope: JSONRPCMessage = JSONRPCMessage.Error(id = id, error = ProtocolMeta.missingClientCapabilityError(required))
        Left(McpResponse.JsonError(StatusCode.BadRequest, envelope.asJson))
      else
        val result = InputRequiredResult(Option.when(outcome.requests.nonEmpty)(outcome.requests), outcome.requestState)
        Right(JSONRPCMessage.Response(id = id, result = result.asJson))

  // request methods a 2026-07-28 client may call
  private val modernMethods: Set[String] =
    Set(
      "server/discover",
      "tools/list",
      "tools/call",
      "resources/list",
      "resources/templates/list",
      "resources/read",
      "prompts/list",
      "prompts/get",
      "completion/complete"
    ) ++ listenPublisher.map(_ => "subscriptions/listen")

  private def jsonError(id: RequestId, status: StatusCode, code: Int, message: String, data: Option[Json] = None): McpResponse =
    logger.debug(s"Modern request rejected (id=$id, http=${status.code}, code=$code): $message")
    val envelope: JSONRPCMessage = JSONRPCMessage.Error(id = id, error = JSONRPCErrorObject(code = code, message = message, data = data))
    McpResponse.JsonError(status, envelope.asJson)

  /** Whether the request follows the modern (2026-07-28) rules: it declares a modern version in `_meta` or via the `MCP-Protocol-Version`
    * header. A known legacy version in `_meta` keeps the legacy rules; an unknown one is treated as modern, so it gets the modern `-32022`
    * with HTTP 400. A `server/discover` that declares no version stays legacy, so a probing client can still discover the server.
    */
  private def isModernRequest(params: Option[Json], headers: Seq[Header]): Boolean =
    val metaVersion = params.flatMap(_.hcursor.downField("_meta").downField(ProtocolMeta.ProtocolVersionKey).as[String].toOption)
    val headerVersion = headers.collectFirst { case h if h.name.equalsIgnoreCase(ProtocolMeta.ProtocolVersionHeader) => h.value.trim }
    val metaIsModern = metaVersion.exists(version => ProtocolVersion.from(version).forall(_.isModern))
    metaIsModern || headerVersion.flatMap(ProtocolVersion.from).exists(_.isModern)

  /** The facts a handler can read about the request; see [[RequestInfo]]. */
  private def requestInfo(params: Option[Json], headers: Seq[Header], isModern: Boolean): RequestInfo =
    val meta = params.flatMap(_.hcursor.downField("_meta").focus).map(_.hcursor)
    def fromMeta[A: Decoder](key: String): Option[A] = meta.flatMap(_.get[A](key).toOption)
    val headerVersion = headers.collectFirst { case h if h.name.equalsIgnoreCase(ProtocolMeta.ProtocolVersionHeader) => h.value.trim }
    val progressToken = fromMeta[ProgressToken]("progressToken")
    val protocolVersion = fromMeta[String](ProtocolMeta.ProtocolVersionKey).orElse(headerVersion).flatMap(ProtocolVersion.from)
    if isModern then
      RequestInfo(
        protocolVersion = protocolVersion,
        clientCapabilities = fromMeta[ClientCapabilities](ProtocolMeta.ClientCapabilities),
        clientInfo = fromMeta[Implementation](ProtocolMeta.ClientInfo),
        logLevel = fromMeta[LoggingLevel](ProtocolMeta.LogLevel),
        progressToken = progressToken,
        inputResponses = params.flatMap(_.hcursor.get[Map[String, InputResponse]]("inputResponses").toOption).getOrElse(Map.empty),
        requestState = params.flatMap(_.hcursor.get[String]("requestState").toOption)
      )
    else RequestInfo(protocolVersion = protocolVersion, progressToken = progressToken)

  /** The rejection to send for an invalid request, or `None` if it may be dispatched. Runs no tool logic, so a transport can call it before
    * deciding how to respond (a modern validation error is a plain-JSON HTTP error, not an SSE stream).
    */
  def requestRejection(method: String, params: Option[Json], headers: Seq[Header], id: RequestId): Option[McpResponse] =
    val meta = params.flatMap(_.hcursor.downField("_meta").focus)
    val metaVersion = meta.flatMap(_.hcursor.get[String](ProtocolMeta.ProtocolVersionKey).toOption)
    val headerVersion = headers.collectFirst { case h if h.name.equalsIgnoreCase(ProtocolMeta.ProtocolVersionHeader) => h.value.trim }
    if isModernRequest(params, headers) then modernRejection(method, params, meta, metaVersion, headerVersion, headers, id)
    else
      metaVersion match
        case Some(version) if ProtocolVersion.from(version).isEmpty =>
          Some(
            jsonResponse(
              JSONRPCMessage.Error(id = id, error = ProtocolMeta.unsupportedVersionError(version, ProtocolVersion.supported.map(_.name)))
            )
          )
        case _ => None

  /** Parses `request` and returns the rejection for an invalid (modern) request, or `None`; runs no tool logic. */
  def validateRequest(request: Json, headers: Seq[Header]): Option[McpResponse] =
    request.as[JSONRPCMessage] match
      case Right(JSONRPCMessage.Request(_, method, params: Option[Json], id)) => requestRejection(method, params, headers, id)
      case _                                                                  => None

  /** Validates a modern (2026-07-28) request; `Some` is the rejection to send, `None` means it may be dispatched. */
  private def modernRejection(
      method: String,
      params: Option[Json],
      meta: Option[Json],
      metaVersion: Option[String],
      headerVersion: Option[String],
      headers: Seq[Header],
      id: RequestId
  ): Option[McpResponse] =
    val declared = metaVersion.orElse(headerVersion)
    val overHttp = headers.exists(_.name.equalsIgnoreCase("Host"))
    if overHttp && headerVersion.isEmpty then
      Some(jsonError(id, StatusCode.BadRequest, JSONRPCErrorCodes.HeaderMismatch.code, "Missing MCP-Protocol-Version header"))
    else if headerVersion.isDefined && metaVersion.isDefined && headerVersion != metaVersion then
      Some(jsonError(id, StatusCode.BadRequest, JSONRPCErrorCodes.HeaderMismatch.code, "MCP-Protocol-Version header does not match _meta"))
    else if declared.exists(version => ProtocolVersion.from(version).isEmpty) then
      val envelope: JSONRPCMessage =
        JSONRPCMessage.Error(id = id, error = ProtocolMeta.unsupportedVersionError(declared.get, ProtocolVersion.supported.map(_.name)))
      Some(McpResponse.JsonError(StatusCode.BadRequest, envelope.asJson))
    else if metaVersion.isEmpty then
      Some(
        jsonError(id, StatusCode.BadRequest, JSONRPCErrorCodes.InvalidParams.code, s"Missing ${ProtocolMeta.ProtocolVersionKey} in _meta")
      )
    else if meta.flatMap(_.hcursor.downField(ProtocolMeta.ClientCapabilities).focus).isEmpty then
      Some(
        jsonError(id, StatusCode.BadRequest, JSONRPCErrorCodes.InvalidParams.code, s"Missing ${ProtocolMeta.ClientCapabilities} in _meta")
      )
    else if !modernMethods.contains(method) then
      Some(jsonError(id, StatusCode.NotFound, JSONRPCErrorCodes.MethodNotFound.code, s"Method not found: $method"))
    else
      // SEP-2243 standard request headers only apply over HTTP (identified by the Host header); stdio carries no such headers
      val headerError = Option.when(overHttp)(headerValidationError(method, params, headers, id))
      headerError.flatten.orElse(missingCapabilityError(method, params, meta, id)).orElse(inputResponsesError(method, params, id))

  // the requests which can end with an input-required result and so carry `inputResponses` and `requestState` on a retry
  private val inputRequiredMethods: Set[String] = Set("tools/call", "prompts/get", "resources/read")

  /** A `-32602` with HTTP 400 when a retry's `inputResponses` is not an object of input responses, or its `requestState` not a string. */
  private def inputResponsesError(method: String, params: Option[Json], id: RequestId): Option[McpResponse] =
    def invalid(message: String) = Some(jsonError(id, StatusCode.BadRequest, JSONRPCErrorCodes.InvalidParams.code, message))
    val cursor = params.filter(_ => inputRequiredMethods.contains(method)).map(_.hcursor)
    val responses = cursor.flatMap(_.downField("inputResponses").focus).map(_.as[Map[String, InputResponse]])
    val state = cursor.flatMap(_.downField("requestState").focus)
    responses match
      case Some(Left(failure))                       => invalid(s"Invalid inputResponses: ${failure.getMessage}")
      case _ if state.exists(json => !json.isString) => invalid("Invalid requestState: expected a string")
      case _                                         => None

  /** A `-32021` with HTTP 400 when `tools/call` names a tool whose required client capabilities the request's `_meta` does not declare. */
  private def missingCapabilityError(method: String, params: Option[Json], meta: Option[Json], id: RequestId): Option[McpResponse] =
    val declared = meta.flatMap(_.hcursor.downField(ProtocolMeta.ClientCapabilities).focus).getOrElse(Json.obj())
    val required = Option
      .when(method == "tools/call")(params.flatMap(_.hcursor.get[String]("name").toOption))
      .flatten
      .flatMap(toolsByName.get)
      .flatMap(_.requiredClientCapabilities)
    required
      .filterNot(capabilities => declares(declared, capabilities.asJson.deepDropNullValues))
      .map: capabilities =>
        val envelope: JSONRPCMessage = JSONRPCMessage.Error(id = id, error = ProtocolMeta.missingClientCapabilityError(capabilities))
        logger.debug(s"Modern request rejected (id=$id): missing client capabilities ${capabilities.asJson.noSpaces}")
        McpResponse.JsonError(StatusCode.BadRequest, envelope.asJson)

  /** Whether `declared` has every key of `required`, recursively for nested objects. */
  private def declares(declared: Json, required: Json): Boolean =
    required.asObject.forall(_.toList.forall { (key, value) =>
      declared.asObject.flatMap(_(key)).exists(declaredValue => value.asObject.forall(_ => declares(declaredValue, value)))
    })

  // SEP-2243: the body field that the `Mcp-Name` header must mirror, per method
  private val nameFields: Map[String, String] = Map("tools/call" -> "name", "prompts/get" -> "name", "resources/read" -> "uri")

  /** SEP-2243 standard request headers: `Mcp-Method` must equal the body method, and `Mcp-Name` must equal the method's name-shaped body
    * field ([[nameFields]]); values are case-sensitive, surrounding whitespace is ignored. A missing or mismatched header is a `-32020`
    * HeaderMismatch with HTTP 400.
    */
  private def headerValidationError(method: String, params: Option[Json], headers: Seq[Header], id: RequestId): Option[McpResponse] =
    def header(name: String): Option[String] = headers.collectFirst { case h if h.name.equalsIgnoreCase(name) => h.value.trim }
    def mismatch(message: String) = Some(jsonError(id, StatusCode.BadRequest, JSONRPCErrorCodes.HeaderMismatch.code, message))
    val nameField = nameFields.get(method)
    val bodyName = nameField.flatMap(field => params.flatMap(_.hcursor.get[String](field).toOption))
    if !header("Mcp-Method").contains(method) then mismatch("Mcp-Method header missing or does not match the request method")
    else if bodyName.exists(name => !header("Mcp-Name").contains(name)) then
      mismatch(s"Mcp-Name header missing or does not match params.${nameField.getOrElse("name")}")
    else None

  private val serverImplementation: Implementation = Implementation(server.name, server.version)
  private val serverInfoJson: Json = serverImplementation.asJson
  private val serverInfoMeta: Json = Json.obj(ProtocolMeta.ServerInfo -> serverInfoJson)

  /** A legacy client gets no list changes (there is no stream for them) and subscribes with `resources/subscribe`. A modern client gets
    * them on a `subscriptions/listen` stream, which also replaces `resources/subscribe`.
    */
  private def serverCapabilities(modern: Boolean): ServerCapabilities =
    val listChanged = Some(modern && listenPublisher.isDefined)
    ServerCapabilities(
      logging = Option.when(server.loggingLevel.isDefined)(Json.obj()),
      completions = Option.when(server.completion.isDefined)(Json.obj()),
      prompts = Option.when(hasPrompts)(ServerPromptsCapability(listChanged = listChanged)),
      resources = Option.when(hasResources)(
        ServerResourcesCapability(
          subscribe = if modern then listChanged else Some(server.subscriptions.isDefined),
          listChanged = listChanged
        )
      ),
      tools = Option.when(server.tools.nonEmpty)(ServerToolsCapability(listChanged = listChanged))
    )

  /** The `subscriptions/listen` request which this server serves on a stream, with the subset of its filter that the server honours; `None`
    * for any other request, and for a request which [[validateRequest]] rejects.
    */
  def listenRequest(request: Json, headers: Seq[Header]): Option[ListenRequest] =
    for
      publisher <- listenPublisher
      case JSONRPCMessage.Request(_, "subscriptions/listen", params, id) <- request.as[JSONRPCMessage].toOption
      if isModernRequest(params, headers) && requestRejection("subscriptions/listen", params, headers, id).isEmpty
      listen <- params.flatMap(_.as[SubscriptionsListenParams].toOption)
    yield ListenRequest(id, honoured(listen.notifications), publisher)

  private def honoured(requested: SubscriptionFilter): SubscriptionFilter =
    def flag(value: Option[Boolean], supported: Boolean) = Option.when(supported && value.contains(true))(true)
    SubscriptionFilter(
      promptsListChanged = flag(requested.promptsListChanged, hasPrompts),
      resourceSubscriptions = requested.resourceSubscriptions.filter(_ => hasResources),
      resourcesListChanged = flag(requested.resourcesListChanged, hasResources),
      toolsListChanged = flag(requested.toolsListChanged, server.tools.nonEmpty)
    )

  private val discoverResult: DiscoverResult = DiscoverResult(
    supportedVersions = ProtocolVersion.supported.map(_.name),
    capabilities = serverCapabilities(modern = true),
    ttlMs = CacheHints.Default.ttlMs,
    cacheScope = CacheHints.Default.cacheScope,
    instructions = server.instructions,
    _meta = Some(Map(ProtocolMeta.ServerInfo -> serverInfoJson))
  )

  private def handleInitialize(params: Option[Json], id: RequestId): JSONRPCMessage.Response =
    val requested = params.flatMap(_.hcursor.downField("protocolVersion").as[String].toOption)
    val negotiated = requested.map(ProtocolVersion.negotiate).getOrElse(ProtocolVersion.LatestLegacy)
    val result = InitializeResult(
      protocolVersion = negotiated.name,
      capabilities = serverCapabilities(modern = false),
      serverInfo = serverImplementation,
      instructions = server.instructions
    )
    JSONRPCMessage.Response(id = id, result = result.asJson)

  private def handleDiscover(id: RequestId): JSONRPCMessage.Response =
    JSONRPCMessage.Response(id = id, result = discoverResult.asJson)

  private def handleToolsCall(
      params: Option[Json],
      id: RequestId,
      headers: Seq[Header],
      info: RequestInfo,
      makeContext: RequestInfo => C,
      isModern: Boolean
  )(using MonadError[F]): F[Reply] =
    val name = params.flatMap(_.hcursor.downField("name").as[String].toOption)
    val arguments = params.flatMap(_.hcursor.downField("arguments").focus).getOrElse(Json.obj())
    name match
      case Some(name) =>
        toolsByName.get(name) match
          case Some(tool) =>
            tool.inputDecoder.decodeJson(arguments) match
              case Right(input) =>
                withContext(id, "tools/call", name, arguments, info, makeContext): (context, sign) =>
                  tool
                    .logic(input, context, headers)
                    .map:
                      case outcome: InputRequired => inputRequiredReply(id, sign(outcome), context.request, isModern)
                      case result: ToolResult[?]  => Right(toolCallResponse(id, result))
              case Left(decodingError) =>
                val snippet = arguments.noSpaces.take(200)
                failed(
                  protocolError(
                    id,
                    JSONRPCErrorCodes.InvalidParams.code,
                    s"Invalid arguments: ${decodingError.getMessage}. Input: $snippet"
                  )
                )
          case None => failed(protocolError(id, JSONRPCErrorCodes.MethodNotFound.code, s"Unknown tool: $name"))
      case None =>
        failed(protocolError(id, JSONRPCErrorCodes.InvalidParams.code, "Missing tool name"))

  private def failed(error: JSONRPCMessage)(using MonadError[F]): F[Reply] = (Right(error): Reply).unit

  /** Runs `run` with the handler's context, and with how to sign the request state of its outcome. With a [[RequestStateSigner]], the
    * request state of a retry is verified first, and the handler sees the original state; a state which fails verification is a `-32602`
    * with HTTP 400, and the handler does not run.
    */
  private def withContext(id: RequestId, method: String, target: String, arguments: Json, info: RequestInfo, makeContext: RequestInfo => C)(
      run: (C, InputRequired => InputRequired) => F[Reply]
  )(using MonadError[F]): F[Reply] =
    server.requestStateSigner match
      case None         => run(makeContext(info), identity)
      case Some(signer) =>
        val context = makeContext(info)
        val owner = server.requestStateOwner(context)
        val request = Json.arr(method.asJson, target.asJson, RequestStateSigner.digest(arguments).asJson, owner.asJson)
        val sign: InputRequired => InputRequired = _.mapRequestState(signer.sign(_, request))
        info.requestState.map(signer.verify(_, request)) match
          case None                => run(context, sign)
          case Some(Right(state))  => run(makeContext(info.copy(requestState = Some(state))), sign)
          case Some(Left(problem)) =>
            (Left(
              jsonError(id, StatusCode.BadRequest, JSONRPCErrorCodes.InvalidParams.code, s"Invalid requestState: $problem")
            ): Reply).unit

  private def toolCallResponse(id: RequestId, result: ToolResult[?]): JSONRPCMessage =
    // for backwards compatibility, structured output is serialized into a text block, unless the tool returned content of its own
    val content = result.structuredContent match
      case Some(json) if result.content.isEmpty => List(ToolContent.Text(text = json.noSpaces))
      case _                                    => result.content
    JSONRPCMessage.Response(
      id = id,
      result = CallToolResult(
        content = content,
        structuredContent = result.structuredContent,
        isError = result.isError
      ).asJson
    )

  private def handleResourcesRead(
      params: Option[Json],
      id: RequestId,
      headers: Seq[Header],
      info: RequestInfo,
      makeContext: RequestInfo => C,
      isModern: Boolean
  )(using MonadError[F]): F[Reply] =
    decodeReplyParams[ReadResourceParams](params, id): params =>
      withContext(id, "resources/read", params.uri, Json.obj(), info, makeContext): (context, sign) =>
        val respond = resourceReadReply(id, params.uri, context.request, isModern, sign)
        resourcesByUri.get(params.uri) match
          case Some(resource) => resource.read(context, headers).map(respond)
          case None           =>
            server.resourceTemplates.iterator
              .map(template => (template, template.matcher.matchUri(params.uri)))
              .collectFirst { case (template, Some(vars)) => template.read(vars, params.uri, context, headers) } match
              case Some(result) => result.map(respond)
              case None         =>
                failed(
                  protocolError(
                    id,
                    JSONRPCErrorCodes.ResourceNotFound.code,
                    s"Resource not found: ${params.uri}",
                    Some(Json.obj("uri" -> Json.fromString(params.uri)))
                  )
                )

  private def decodeReplyParams[P: Decoder](params: Option[Json], id: RequestId)(f: P => F[Reply])(using MonadError[F]): F[Reply] =
    params.flatMap(_.as[P].toOption) match
      case Some(params) => f(params)
      case None         => failed(protocolError(id, JSONRPCErrorCodes.InvalidParams.code, "Invalid or missing params"))

  private def decodeParams[P: Decoder](params: Option[Json], id: RequestId)(f: P => F[JSONRPCMessage])(using
      MonadError[F]
  ): F[JSONRPCMessage] =
    params.flatMap(_.as[P].toOption) match
      case Some(params) => f(params)
      case None         => protocolError(id, JSONRPCErrorCodes.InvalidParams.code, "Invalid or missing params").unit

  private def resourceReadReply(id: RequestId, uri: String, info: RequestInfo, isModern: Boolean, sign: InputRequired => InputRequired)(
      outcome: Either[ResourceError, List[ResourceContents]] | InputRequired
  ): Reply =
    outcome match
      case required: InputRequired => inputRequiredReply(id, sign(required), info, isModern)
      case Right(contents)         => Right(JSONRPCMessage.Response(id = id, result = ReadResourceResult(contents).asJson))
      case Left(error)             =>
        Right(
          protocolError(
            id,
            JSONRPCErrorCodes.ResourceNotFound.code,
            error.message,
            error.uri.orElse(Some(uri)).map(uri => Json.obj("uri" -> Json.fromString(uri)))
          )
        )

  private def handleSubscribe(params: Option[Json], id: RequestId, subscribe: Boolean)(using MonadError[F]): F[JSONRPCMessage] =
    val subs = server.subscriptions.get
    if subscribe then decodeParams[SubscribeParams](params, id)(params => subs.onSubscribe(params).map(_ => emptyResult(id)))
    else decodeParams[UnsubscribeParams](params, id)(params => subs.onUnsubscribe(params).map(_ => emptyResult(id)))

  private def emptyResult(id: RequestId): JSONRPCMessage = JSONRPCMessage.Response(id = id, result = Json.obj())

  private def handlePromptsGet(
      params: Option[Json],
      id: RequestId,
      headers: Seq[Header],
      info: RequestInfo,
      makeContext: RequestInfo => C,
      isModern: Boolean
  )(using MonadError[F]): F[Reply] =
    decodeReplyParams[GetPromptParams](params, id): params =>
      promptsByName.get(params.name) match
        case Some(prompt) =>
          val arguments = params.arguments.getOrElse(Map.empty)
          withContext(id, "prompts/get", params.name, arguments.asJson, info, makeContext): (context, sign) =>
            prompt
              .logic(arguments, context, headers)
              .map:
                case outcome: InputRequired  => inputRequiredReply(id, sign(outcome), context.request, isModern)
                case result: GetPromptResult => Right(JSONRPCMessage.Response(id = id, result = result.asJson))
        case None => failed(protocolError(id, JSONRPCErrorCodes.InvalidParams.code, s"Unknown prompt: ${params.name}"))

  private def handleComplete(params: Option[Json], id: RequestId)(using MonadError[F]): F[JSONRPCMessage] =
    val handler = server.completion.get
    decodeParams[CompleteParams](params, id): params =>
      handler(params.ref, params.argument, params.context)
        .map(completion => JSONRPCMessage.Response(id = id, result = CompleteResult(completion).asJson))

  private def handleSetLoggingLevel(params: Option[Json], id: RequestId)(using MonadError[F]): F[JSONRPCMessage] =
    val handler = server.loggingLevel.get
    decodeParams[SetLevelParams](params, id)(params => handler(params.level).map(_ => emptyResult(id)))
