package chimp.server

import chimp.protocol.{CompleteArgument, CompleteContext, CompleteRef, Completion, LoggingLevel, SubscribeParams, UnsubscribeParams}
import sttp.tapir.server.ServerEndpoint

/** Logic producing completion suggestions for an argument of a prompt or resource-template reference. */
type CompletionHandler[F[_]] = (CompleteRef, CompleteArgument, Option[CompleteContext]) => F[Completion]

/** Logic invoked when the client sets the minimum logging level via `logging/setLevel`. */
type SetLevelHandler[F[_]] = LoggingLevel => F[Unit]

/** Logic invoked when the client subscribes to / unsubscribes from updates for a resource URI. */
case class ResourceSubscriptions[F[_]](
    onSubscribe: SubscribeParams => F[Unit],
    onUnsubscribe: UnsubscribeParams => F[Unit]
)

/** The features and metadata of an MCP server, shared by the request/response [[McpServer]] (tools needing only a base [[ServerContext]]) and
  * the [[StreamingMcpServer]] (tools needing a [[StreamingServerContext]]). `C` is the context the registered tools require.
  */
sealed trait McpServerDef[F[_], C <: ServerContext[F]]:
  def name: String
  def version: String
  def instructions: Option[String]
  def showJsonSchemaMetadata: Boolean
  def originCheck: OriginCheck
  def tools: List[ServerTool[?, F, C]]
  def prompts: List[ServerPrompt[F]]
  def resources: List[ServerResource[F]]
  def resourceTemplates: List[ServerResourceTemplate[F]]
  def completion: Option[CompletionHandler[F]]
  def setLevel: Option[SetLevelHandler[F]]
  def subscriptions: Option[ResourceSubscriptions[F]]

/** An MCP server definition: the features it exposes and the metadata it reports. Build one up with the `add*`/`with*` methods, then turn it
  * into a Tapir endpoint with [[endpoint]]. Server capabilities are derived from which features are registered.
  */
case class McpServer[F[_]](
    name: String = "Chimp MCP server",
    version: String = "1.0.0",
    instructions: Option[String] = None,
    showJsonSchemaMetadata: Boolean = true,
    originCheck: OriginCheck = OriginCheck.localhostOnly,
    tools: List[ServerTool[?, F, ServerContext[F]]] = Nil,
    prompts: List[ServerPrompt[F]] = Nil,
    resources: List[ServerResource[F]] = Nil,
    resourceTemplates: List[ServerResourceTemplate[F]] = Nil,
    completion: Option[CompletionHandler[F]] = None,
    setLevel: Option[SetLevelHandler[F]] = None,
    subscriptions: Option[ResourceSubscriptions[F]] = None
) extends McpServerDef[F, ServerContext[F]]:
  def name(value: String): McpServer[F] = copy(name = value)
  def version(value: String): McpServer[F] = copy(version = value)
  def instructions(value: String): McpServer[F] = copy(instructions = Some(value))
  def withJsonSchemaMetadata(value: Boolean): McpServer[F] = copy(showJsonSchemaMetadata = value)
  def withOriginCheck(value: OriginCheck): McpServer[F] = copy(originCheck = value)

  def addTool(t: ServerTool[?, F, ServerContext[F]]): McpServer[F] = copy(tools = tools :+ t)
  def addTools(ts: ServerTool[?, F, ServerContext[F]]*): McpServer[F] = copy(tools = tools ++ ts)
  def addPrompt(p: ServerPrompt[F]): McpServer[F] = copy(prompts = prompts :+ p)
  def addPrompts(ps: ServerPrompt[F]*): McpServer[F] = copy(prompts = prompts ++ ps)
  def addResource(r: ServerResource[F]): McpServer[F] = copy(resources = resources :+ r)
  def addResources(rs: ServerResource[F]*): McpServer[F] = copy(resources = resources ++ rs)
  def addResourceTemplate(rt: ServerResourceTemplate[F]): McpServer[F] = copy(resourceTemplates = resourceTemplates :+ rt)
  def addResourceTemplates(rts: ServerResourceTemplate[F]*): McpServer[F] = copy(resourceTemplates = resourceTemplates ++ rts)
  def withCompletion(handler: CompletionHandler[F]): McpServer[F] = copy(completion = Some(handler))
  def withLogging(handler: SetLevelHandler[F]): McpServer[F] = copy(setLevel = Some(handler))
  def withSubscriptions(handler: ResourceSubscriptions[F]): McpServer[F] = copy(subscriptions = Some(handler))

  /** Build the request/response Tapir endpoint serving this MCP server at the given path. */
  def endpoint(path: List[String]): ServerEndpoint[Any, F] = buildEndpoint(this, path)

  /** Promote this server to a [[StreamingMcpServer]], on which streaming-only tools can additionally be registered and which serves SSE
    * responses. The already-registered tools carry over (a base-context tool runs unchanged on the streaming endpoint).
    */
  def streaming: StreamingMcpServer[F] =
    StreamingMcpServer(name, version, instructions, showJsonSchemaMetadata, originCheck, tools, prompts, resources, resourceTemplates,
      completion, setLevel, subscriptions)

/** An MCP server that serves Server-Sent-Event responses and accepts streaming-only tools (those using a [[StreamingServerContext]] to
  * report progress, log, or — later — sample/elicit). Materialized into a Tapir endpoint via [[streamingEndpoint]] using a per-effect
  * [[McpStreaming]] implementation.
  */
case class StreamingMcpServer[F[_]](
    name: String = "Chimp MCP server",
    version: String = "1.0.0",
    instructions: Option[String] = None,
    showJsonSchemaMetadata: Boolean = true,
    originCheck: OriginCheck = OriginCheck.localhostOnly,
    tools: List[ServerTool[?, F, StreamingServerContext[F]]] = Nil,
    prompts: List[ServerPrompt[F]] = Nil,
    resources: List[ServerResource[F]] = Nil,
    resourceTemplates: List[ServerResourceTemplate[F]] = Nil,
    completion: Option[CompletionHandler[F]] = None,
    setLevel: Option[SetLevelHandler[F]] = None,
    subscriptions: Option[ResourceSubscriptions[F]] = None
) extends McpServerDef[F, StreamingServerContext[F]]:
  def name(value: String): StreamingMcpServer[F] = copy(name = value)
  def version(value: String): StreamingMcpServer[F] = copy(version = value)
  def instructions(value: String): StreamingMcpServer[F] = copy(instructions = Some(value))
  def withJsonSchemaMetadata(value: Boolean): StreamingMcpServer[F] = copy(showJsonSchemaMetadata = value)
  def withOriginCheck(value: OriginCheck): StreamingMcpServer[F] = copy(originCheck = value)

  /** Add a base-context tool (it runs unchanged, without using streaming features). */
  def addTool(t: ServerTool[?, F, ServerContext[F]]): StreamingMcpServer[F] = copy(tools = tools :+ t)
  def addTools(ts: ServerTool[?, F, ServerContext[F]]*): StreamingMcpServer[F] = copy(tools = tools ++ ts)

  /** Add a streaming-only tool (one whose logic uses the [[StreamingServerContext]]). */
  def addStreamingTool(t: ServerTool[?, F, StreamingServerContext[F]]): StreamingMcpServer[F] = copy(tools = tools :+ t)
  def addStreamingTools(ts: ServerTool[?, F, StreamingServerContext[F]]*): StreamingMcpServer[F] = copy(tools = tools ++ ts)

  def addPrompt(p: ServerPrompt[F]): StreamingMcpServer[F] = copy(prompts = prompts :+ p)
  def addPrompts(ps: ServerPrompt[F]*): StreamingMcpServer[F] = copy(prompts = prompts ++ ps)
  def addResource(r: ServerResource[F]): StreamingMcpServer[F] = copy(resources = resources :+ r)
  def addResources(rs: ServerResource[F]*): StreamingMcpServer[F] = copy(resources = resources ++ rs)
  def addResourceTemplate(rt: ServerResourceTemplate[F]): StreamingMcpServer[F] = copy(resourceTemplates = resourceTemplates :+ rt)
  def addResourceTemplates(rts: ServerResourceTemplate[F]*): StreamingMcpServer[F] = copy(resourceTemplates = resourceTemplates ++ rts)
  def withCompletion(handler: CompletionHandler[F]): StreamingMcpServer[F] = copy(completion = Some(handler))
  def withLogging(handler: SetLevelHandler[F]): StreamingMcpServer[F] = copy(setLevel = Some(handler))
  def withSubscriptions(handler: ResourceSubscriptions[F]): StreamingMcpServer[F] = copy(subscriptions = Some(handler))

  /** Build the SSE-capable Tapir endpoint serving this MCP server at the given path, using the given per-effect streaming implementation. */
  def streamingEndpoint[S](path: List[String], streaming: McpStreaming[F, S]): ServerEndpoint[S, F] =
    buildStreamingEndpoint(this, streaming, path)
