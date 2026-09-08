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

private[server] class McpHandler[F[_], C <: ServerContext[F]](server: McpServerDef[F, C]):
  private val logger = LoggerFactory.getLogger(classOf[McpHandler[?, ?]])
  private val toolsByName = server.tools.map(tool => tool.name -> tool).toMap
  private val taskToolsByName = server.taskTools.map(tool => tool.name -> tool).toMap
  private val inputCoordinator = TaskInputCoordinator()
  private val promptsByName = server.prompts.map(prompt => prompt.definition.name -> prompt).toMap
  private val resourcesByUri = server.resources.map(resource => resource.definition.uri -> resource).toMap
  private val hasResources = server.resources.nonEmpty || server.resourceTemplates.nonEmpty
  private val toolDefinitions = server.tools.map(toolToDefinition) ++ server.taskTools.map(toolToDefinition)

  private def toJsonSchema(toolSchema: ToolSchema): Json = toolSchema match
    case ToolSchema.Derived(schema) =>
      val base = TapirSchemaToJsonSchema(schema, markOptionsAsNullable = false)
      (if server.showJsonSchemaMetadata then base else base.copy($schema = None)).asJson
    case ToolSchema.Raw(json) => json

  private def toolToDefinition(tool: ServerTool[?, ?, F, ?]): ToolDefinition =
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

  def handleJsonRpc(request: Json, headers: Seq[Header], makeContext: Option[ProgressToken] => C)(using MonadError[F]): F[McpResponse] =
    doHandleJsonRpc(request, headers, makeContext).map: response =>
      logger.debug(s"Request: $request, response: ${response.statusCode}, body: ${response.body.getOrElse(Json.Null)}")
      response.withNullsDroppedDeep

  def handleJsonRpc(request: Json, headers: Seq[Header])(using m: MonadError[F], ev: ServerContext[F] <:< C): F[McpResponse] =
    handleJsonRpc(request, headers, _ => ev(ServerContext.noop[F]))

  private def doHandleJsonRpc(request: Json, headers: Seq[Header], makeContext: Option[ProgressToken] => C)(using
      MonadError[F]
  ): F[McpResponse] =
    request.as[JSONRPCMessage] match
      case Left(err) =>
        jsonResponse(protocolError(RequestId("null"), JSONRPCErrorCodes.ParseError.code, s"Parse error: ${err.message}")).unit
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
          case "tasks/get" if server.tasks.isDefined =>
            handleTasksGet(params, id).map(jsonResponse)
          case "tasks/cancel" if server.tasks.isDefined =>
            handleTasksCancel(params, id).map(jsonResponse)
          case "tasks/update" if server.tasks.isDefined =>
            handleTasksUpdate(params, id).map(jsonResponse)
          case "resources/list" if hasResources =>
            jsonResponse(JSONRPCMessage.Response(id = id, result = ListResourcesResult(server.resources.map(_.definition)).asJson)).unit
          case "resources/templates/list" if hasResources =>
            jsonResponse(
              JSONRPCMessage.Response(id = id, result = ListResourceTemplatesResult(server.resourceTemplates.map(_.definition)).asJson)
            ).unit
          case "resources/read" if hasResources =>
            handleResourcesRead(params, id, headers).map(jsonResponse)
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
          case "logging/setLevel" if server.loggingLevel.isDefined =>
            handleSetLoggingLevel(params, id).map(jsonResponse)
          case other =>
            jsonResponse(protocolError(id, JSONRPCErrorCodes.MethodNotFound.code, s"Unknown method: $other")).unit
      case Right(notification: JSONRPCMessage.Notification) =>
        logger.debug(s"Received notification: ${notification.method}")
        McpResponse.EmptyAcceptResponse.unit
      case Right(_) =>
        jsonResponse(protocolError(RequestId("null"), JSONRPCErrorCodes.InvalidRequest.code, "Invalid request type")).unit
  end doHandleJsonRpc

  private def protocolError(id: RequestId, code: Int, message: String, data: Option[Json] = None): JSONRPCMessage.Error =
    logger.debug(s"Protocol error (id=$id, code=$code): $message")
    JSONRPCMessage.Error(id = id, error = JSONRPCErrorObject(code = code, message = message, data = data))

  private def jsonResponse(message: JSONRPCMessage): McpResponse = McpResponse.JsonResponse(message.asJson)

  private def handleInitialize(params: Option[Json], id: RequestId): JSONRPCMessage.Response =
    val requested = params.flatMap(_.hcursor.downField("protocolVersion").as[String].toOption)
    val negotiated = requested.map(ProtocolVersion.negotiate).getOrElse(ProtocolVersion.Latest)
    val capabilities = ServerCapabilities(
      logging = Option.when(server.loggingLevel.isDefined)(Json.obj()),
      completions = Option.when(server.completion.isDefined)(Json.obj()),
      prompts = Option.when(server.prompts.nonEmpty)(ServerPromptsCapability(listChanged = Some(false))),
      resources =
        Option.when(hasResources)(ServerResourcesCapability(subscribe = Some(server.subscriptions.isDefined), listChanged = Some(false))),
      tools = Option.when(server.tools.nonEmpty)(ServerToolsCapability(listChanged = Some(false))),
      extensions = server.tasks.map(_ => Map(TasksExtension.Id -> Json.obj()))
    )
    val result = InitializeResult(
      protocolVersion = negotiated.name,
      capabilities = capabilities,
      serverInfo = Implementation(server.name, server.version),
      instructions = server.instructions
    )
    JSONRPCMessage.Response(id = id, result = result.asJson)

  private def handleToolsCall(params: Option[Json], id: RequestId, headers: Seq[Header], makeContext: Option[ProgressToken] => C)(using
      m: MonadError[F]
  ): F[JSONRPCMessage] =
    val name = params.flatMap(_.hcursor.downField("name").as[String].toOption)
    val arguments = params.flatMap(_.hcursor.downField("arguments").focus).getOrElse(Json.obj())
    val progressToken = params.flatMap(_.hcursor.downField("_meta").downField("progressToken").as[ProgressToken].toOption)
    val requestMeta = params.flatMap(_.hcursor.downField("_meta").as[Map[String, Json]].toOption)
    val clientSupportsTasks = TasksExtension.declaredIn(requestMeta)

    def invalidArguments(error: DecodingFailure): F[JSONRPCMessage] =
      protocolError(
        id,
        JSONRPCErrorCodes.InvalidParams.code,
        s"Invalid arguments: ${error.getMessage}. Input: ${arguments.noSpaces.take(200)}"
      ).unit

    def missingTaskCapability(toolName: String): F[JSONRPCMessage] =
      protocolError(
        id,
        JSONRPCErrorCodes.MissingRequiredClientCapability.code,
        s"Tool '$toolName' requires the ${TasksExtension.Id} client capability"
      ).unit

    name match
      case Some(name) =>
        taskToolsByName.get(name) match
          // a task tool always runs as a task and needs both server task support and the client capability
          case Some(taskTool) =>
            server.tasks match
              case None =>
                protocolError(
                  id,
                  JSONRPCErrorCodes.InternalError.code,
                  s"Tool '$name' runs as a task, but task support is not configured"
                ).unit
              case Some(_) if !clientSupportsTasks => missingTaskCapability(name)
              case Some(support)                   =>
                taskTool.inputDecoder.decodeJson(arguments) match
                  case Right(input) => runTask(support, id)(taskId => taskTool.logic(input, makeTaskContext(support, taskId), headers))
                  case Left(error)  => invalidArguments(error)
          case None =>
            toolsByName.get(name) match
              case Some(tool) =>
                tool.inputDecoder.decodeJson(arguments) match
                  case Right(input) =>
                    val context = makeContext(progressToken)
                    server.tasks match
                      case Some(support) if support.requireTask(name) && !clientSupportsTasks => missingTaskCapability(name)
                      case Some(support) if clientSupportsTasks && support.useTask(name)      =>
                        runTask(support, id)(_ => tool.logic(input, context, headers))
                      case _ =>
                        tool.logic(input, context, headers).map(result => toolCallResponse(id, result))
                  case Left(error) => invalidArguments(error)
              case None => protocolError(id, JSONRPCErrorCodes.MethodNotFound.code, s"Unknown tool: $name").unit
      case None =>
        protocolError(id, JSONRPCErrorCodes.InvalidParams.code, "Missing tool name").unit

  private def toCallToolResult(result: ToolResult[?]): CallToolResult =
    // for backwards compatibility, structured output is serialized into a text block, unless the tool returned content of its own
    val content = result.structuredContent match
      case Some(json) if result.content.isEmpty => List(ToolContent.Text(text = json.noSpaces))
      case _                                    => result.content
    CallToolResult(content = content, structuredContent = result.structuredContent, isError = result.isError)

  private def toolCallResponse(id: RequestId, result: ToolResult[?]): JSONRPCMessage =
    JSONRPCMessage.Response(id = id, result = toCallToolResult(result).asJson)

  private def runTask[O](support: TaskSupport[F], id: RequestId)(compute: TaskId => F[ToolResult[O]])(using
      m: MonadError[F]
  ): F[JSONRPCMessage] =
    val taskId = TaskId(java.util.UUID.randomUUID().toString)
    val now = java.time.Instant.now()
    val initial = GetTaskResult(
      taskId = taskId,
      outcome = TaskOutcome.Working,
      createdAt = Some(now),
      lastUpdatedAt = Some(now),
      ttlMs = support.ttl,
      pollIntervalMs = support.pollInterval,
      resultType = Some("complete")
    )
    // start and handleError both take their body by-name, so a synchronous (Identity) tool that throws is caught here too
    support.store
      .create(initial)
      .flatMap { _ =>
        support.executor.start(taskId)(
          m.handleError(
            m.flatMap(compute(taskId))(result => finishTask(support, taskId, TaskOutcome.Completed(toCallToolResult(result).asJson)))
          ) { case t =>
            finishTask(
              support,
              taskId,
              TaskOutcome.Failed(JSONRPCErrorObject(JSONRPCErrorCodes.InternalError.code, Option(t.getMessage).getOrElse("Task failed")))
            )
          }
        )
      }
      .map { _ =>
        JSONRPCMessage.Response(
          id = id,
          result = CreateTaskResult(
            taskId = taskId,
            status = TaskStatus.Working,
            createdAt = Some(now),
            lastUpdatedAt = Some(now),
            ttlMs = support.ttl,
            pollIntervalMs = support.pollInterval
          ).asJson
        )
      }

  private def makeTaskContext(support: TaskSupport[F], taskId: TaskId)(using m: MonadError[F]): TaskContext[F] =
    new TaskContext[F]:
      def requestInput(key: String, request: Json): F[Json] =
        // register the waiter before advertising input_required, so a fast tasks/update is never lost
        m.flatMap(m.eval(inputCoordinator.register(taskId, key))) { waiter =>
          m.flatMap(setInputRequired(support, taskId, key, request)) { _ =>
            // waiter.get blocks the worker until tasks/update delivers the answer; fine on the virtual-thread executor
            m.flatMap(m.eval(waiter.get()))(response => m.map(resolveInput(support, taskId, key))(_ => response))
          }
        }

  private def setInputRequired(support: TaskSupport[F], taskId: TaskId, key: String, request: Json)(using MonadError[F]): F[Unit] =
    support.store
      .update(taskId): current =>
        val outstanding = current.outcome match
          case TaskOutcome.InputRequired(requests) => requests
          case _                                   => Map.empty[String, Json]
        current.copy(outcome = TaskOutcome.InputRequired(outstanding + (key -> request)), lastUpdatedAt = Some(java.time.Instant.now()))
      .map(_ => ())

  private def resolveInput(support: TaskSupport[F], taskId: TaskId, key: String)(using MonadError[F]): F[Unit] =
    support.store
      .update(taskId): current =>
        current.outcome match
          case TaskOutcome.InputRequired(requests) =>
            val remaining = requests - key
            val outcome = if remaining.isEmpty then TaskOutcome.Working else TaskOutcome.InputRequired(remaining)
            current.copy(outcome = outcome, lastUpdatedAt = Some(java.time.Instant.now()))
          case _ => current
      .map(_ => ())

  // only transition a task that is still working, so a cancellation is not overwritten by a late completion
  private def finishTask(support: TaskSupport[F], taskId: TaskId, outcome: TaskOutcome)(using MonadError[F]): F[Unit] =
    support.store
      .update(taskId): current =>
        if current.status == TaskStatus.Working then current.copy(outcome = outcome, lastUpdatedAt = Some(java.time.Instant.now()))
        else current
      .map(_ => ())

  private def handleTasksGet(params: Option[Json], id: RequestId)(using MonadError[F]): F[JSONRPCMessage] =
    decodeParams[GetTaskParams](params, id): p =>
      server.tasks.get.store
        .get(p.taskId)
        .map:
          case Some(task) => JSONRPCMessage.Response(id = id, result = task.asJson)
          case None       => protocolError(id, JSONRPCErrorCodes.InvalidParams.code, s"Unknown task: ${p.taskId}")

  private def handleTasksCancel(params: Option[Json], id: RequestId)(using MonadError[F]): F[JSONRPCMessage] =
    decodeParams[CancelTaskParams](params, id): p =>
      val support = server.tasks.get
      support.store
        .update(p.taskId): current =>
          if TaskStatus.isTerminal(current.status) then current
          else current.copy(outcome = TaskOutcome.Cancelled, lastUpdatedAt = Some(java.time.Instant.now()))
        .flatMap:
          case Some(_) =>
            inputCoordinator.cancel(p.taskId)
            support.executor.cancel(p.taskId).map(_ => taskAck(id, p.taskId, TaskStatus.Cancelled))
          case None => protocolError(id, JSONRPCErrorCodes.InvalidParams.code, s"Unknown task: ${p.taskId}").unit

  private def handleTasksUpdate(params: Option[Json], id: RequestId)(using MonadError[F]): F[JSONRPCMessage] =
    decodeParams[UpdateTaskParams](params, id): p =>
      server.tasks.get.store
        .get(p.taskId)
        .map:
          case Some(task) =>
            // hand each response to the waiting task tool; it transitions the task back to working itself
            p.inputResponses.foreach((key, response) => inputCoordinator.deliverInput(p.taskId, key, response))
            taskAck(id, task.taskId, task.status)
          case None => protocolError(id, JSONRPCErrorCodes.InvalidParams.code, s"Unknown task: ${p.taskId}")

  private def taskAck(id: RequestId, taskId: TaskId, status: TaskStatus): JSONRPCMessage =
    JSONRPCMessage.Response(id = id, result = TaskAck(taskId = Some(taskId), status = Some(status)).asJson)

  private def handleResourcesRead(params: Option[Json], id: RequestId, headers: Seq[Header])(using MonadError[F]): F[JSONRPCMessage] =
    decodeParams[ReadResourceParams](params, id): params =>
      resourcesByUri.get(params.uri) match
        case Some(resource) => resource.read(headers).map(resourceReadResponse(id, params.uri))
        case None           =>
          val templateMatch = server.resourceTemplates.iterator
            .map(template => (template, template.matcher.matchUri(params.uri)))
            .collectFirst { case (template, Some(vars)) => (template, vars) }
          templateMatch match
            case Some((template, vars)) => template.read(vars, params.uri, headers).map(resourceReadResponse(id, params.uri))
            case None                   =>
              protocolError(
                id,
                JSONRPCErrorCodes.ResourceNotFound.code,
                s"Resource not found: ${params.uri}",
                Some(Json.obj("uri" -> Json.fromString(params.uri)))
              ).unit

  private def decodeParams[P: Decoder](params: Option[Json], id: RequestId)(f: P => F[JSONRPCMessage])(using
      MonadError[F]
  ): F[JSONRPCMessage] =
    params.flatMap(_.as[P].toOption) match
      case Some(params) => f(params)
      case None         => protocolError(id, JSONRPCErrorCodes.InvalidParams.code, "Invalid or missing params").unit

  private def resourceReadResponse(id: RequestId, uri: String)(result: Either[ResourceError, List[ResourceContents]]): JSONRPCMessage =
    result match
      case Right(contents) => JSONRPCMessage.Response(id = id, result = ReadResourceResult(contents).asJson)
      case Left(error)     =>
        protocolError(
          id,
          JSONRPCErrorCodes.ResourceNotFound.code,
          error.message,
          error.uri.orElse(Some(uri)).map(uri => Json.obj("uri" -> Json.fromString(uri)))
        )

  private def handleSubscribe(params: Option[Json], id: RequestId, subscribe: Boolean)(using MonadError[F]): F[JSONRPCMessage] =
    val subs = server.subscriptions.get
    if subscribe then decodeParams[SubscribeParams](params, id)(params => subs.onSubscribe(params).map(_ => emptyResult(id)))
    else decodeParams[UnsubscribeParams](params, id)(params => subs.onUnsubscribe(params).map(_ => emptyResult(id)))

  private def emptyResult(id: RequestId): JSONRPCMessage = JSONRPCMessage.Response(id = id, result = Json.obj())

  private def handlePromptsGet(params: Option[Json], id: RequestId, headers: Seq[Header])(using MonadError[F]): F[JSONRPCMessage] =
    decodeParams[GetPromptParams](params, id): params =>
      promptsByName.get(params.name) match
        case Some(prompt) =>
          prompt
            .logic(params.arguments.getOrElse(Map.empty), headers)
            .map(result => JSONRPCMessage.Response(id = id, result = result.asJson))
        case None => protocolError(id, JSONRPCErrorCodes.InvalidParams.code, s"Unknown prompt: ${params.name}").unit

  private def handleComplete(params: Option[Json], id: RequestId)(using MonadError[F]): F[JSONRPCMessage] =
    val handler = server.completion.get
    decodeParams[CompleteParams](params, id): params =>
      handler(params.ref, params.argument, params.context)
        .map(completion => JSONRPCMessage.Response(id = id, result = CompleteResult(completion).asJson))

  private def handleSetLoggingLevel(params: Option[Json], id: RequestId)(using MonadError[F]): F[JSONRPCMessage] =
    val handler = server.loggingLevel.get
    decodeParams[SetLevelParams](params, id)(params => handler(params.level).map(_ => emptyResult(id)))
