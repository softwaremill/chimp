package chimp.server

import chimp.protocol.*
import chimp.server.transport.ServerHttpTransport
import sttp.tapir.server.ServerEndpoint

type CompletionHandler[F[_]] = (CompleteRef, CompleteArgument, Option[CompleteContext]) => F[Completion]

type SetLoggingLevelHandler[F[_]] = LoggingLevel => F[Unit]

case class ResourceSubscriptions[F[_]](
    onSubscribe: SubscribeParams => F[Unit],
    onUnsubscribe: UnsubscribeParams => F[Unit]
)

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
  def loggingLevel: Option[SetLoggingLevelHandler[F]]
  def subscriptions: Option[ResourceSubscriptions[F]]

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
    loggingLevel: Option[SetLoggingLevelHandler[F]] = None,
    subscriptions: Option[ResourceSubscriptions[F]] = None
) extends McpServerDef[F, ServerContext[F]]:
  def name(value: String): McpServer[F] =
    copy(name = value)

  def version(value: String): McpServer[F] =
    copy(version = value)

  def instructions(value: String): McpServer[F] =
    copy(instructions = Some(value))

  def withJsonSchemaMetadata(value: Boolean): McpServer[F] =
    copy(showJsonSchemaMetadata = value)

  def withOriginCheck(value: OriginCheck): McpServer[F] =
    copy(originCheck = value)

  def addTool(tool: ServerTool[?, F, ServerContext[F]]): McpServer[F] =
    copy(tools = tools :+ tool)

  def addTools(tools: ServerTool[?, F, ServerContext[F]]*): McpServer[F] =
    copy(tools = this.tools ++ tools)

  def addPrompt(prompt: ServerPrompt[F]): McpServer[F] =
    copy(prompts = prompts :+ prompt)

  def addPrompts(prompts: ServerPrompt[F]*): McpServer[F] =
    copy(prompts = this.prompts ++ prompts)

  def addResource(resource: ServerResource[F]): McpServer[F] =
    copy(resources = resources :+ resource)

  def addResources(resources: ServerResource[F]*): McpServer[F] =
    copy(resources = this.resources ++ resources)

  def addResourceTemplate(resourceTemplate: ServerResourceTemplate[F]): McpServer[F] =
    copy(resourceTemplates = resourceTemplates :+ resourceTemplate)

  def addResourceTemplates(resourceTemplates: ServerResourceTemplate[F]*): McpServer[F] =
    copy(resourceTemplates = this.resourceTemplates ++ resourceTemplates)

  def withCompletion(handler: CompletionHandler[F]): McpServer[F] =
    copy(completion = Some(handler))

  def withLoggingLevel(handler: SetLoggingLevelHandler[F]): McpServer[F] =
    copy(loggingLevel = Some(handler))

  def withSubscriptions(handler: ResourceSubscriptions[F]): McpServer[F] =
    copy(subscriptions = Some(handler))

  def endpoint(path: List[String]): ServerEndpoint[Any, F] = ServerHttpTransport(path).serve(this)

  def streaming: StreamingMcpServer[F] =
    StreamingMcpServer(
      name,
      version,
      instructions,
      showJsonSchemaMetadata,
      originCheck,
      tools,
      prompts,
      resources,
      resourceTemplates,
      completion,
      loggingLevel,
      subscriptions
    )

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
    loggingLevel: Option[SetLoggingLevelHandler[F]] = None,
    subscriptions: Option[ResourceSubscriptions[F]] = None
) extends McpServerDef[F, StreamingServerContext[F]]:
  def name(value: String): StreamingMcpServer[F] =
    copy(name = value)

  def version(value: String): StreamingMcpServer[F] =
    copy(version = value)

  def instructions(value: String): StreamingMcpServer[F] =
    copy(instructions = Some(value))

  def withJsonSchemaMetadata(value: Boolean): StreamingMcpServer[F] =
    copy(showJsonSchemaMetadata = value)

  def withOriginCheck(value: OriginCheck): StreamingMcpServer[F] =
    copy(originCheck = value)

  def addTool(tool: ServerTool[?, F, ServerContext[F]]): StreamingMcpServer[F] =
    copy(tools = tools :+ tool)

  def addTools(tools: ServerTool[?, F, ServerContext[F]]*): StreamingMcpServer[F] =
    copy(tools = this.tools ++ tools)

  def addStreamingTool(tool: ServerTool[?, F, StreamingServerContext[F]]): StreamingMcpServer[F] =
    copy(tools = tools :+ tool)

  def addStreamingTools(tools: ServerTool[?, F, StreamingServerContext[F]]*): StreamingMcpServer[F] =
    copy(tools = this.tools ++ tools)

  def addPrompt(prompt: ServerPrompt[F]): StreamingMcpServer[F] =
    copy(prompts = prompts :+ prompt)

  def addPrompts(prompts: ServerPrompt[F]*): StreamingMcpServer[F] =
    copy(prompts = this.prompts ++ prompts)

  def addResource(resource: ServerResource[F]): StreamingMcpServer[F] =
    copy(resources = resources :+ resource)

  def addResources(resources: ServerResource[F]*): StreamingMcpServer[F] =
    copy(resources = this.resources ++ resources)

  def addResourceTemplate(resourceTemplate: ServerResourceTemplate[F]): StreamingMcpServer[F] =
    copy(resourceTemplates = resourceTemplates :+ resourceTemplate)

  def addResourceTemplates(resourceTemplates: ServerResourceTemplate[F]*): StreamingMcpServer[F] =
    copy(resourceTemplates = this.resourceTemplates ++ resourceTemplates)

  def withCompletion(handler: CompletionHandler[F]): StreamingMcpServer[F] =
    copy(completion = Some(handler))

  def withLoggingLevel(handler: SetLoggingLevelHandler[F]): StreamingMcpServer[F] =
    copy(loggingLevel = Some(handler))

  def withSubscriptions(handler: ResourceSubscriptions[F]): StreamingMcpServer[F] =
    copy(subscriptions = Some(handler))
