package chimp.server

import chimp.protocol.*
import chimp.server.transport.{SecuredServerHttpTransport, ServerHttpTransport}
import sttp.monad.MonadError
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.{EndpointInput, EndpointOutput}

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
  def tools: List[ServerTool[?, ?, F, C]]
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
    tools: List[ServerTool[?, ?, F, ServerContext[F]]] = Nil,
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

  def addTool(tool: ServerTool[?, ?, F, ServerContext[F]]): McpServer[F] =
    copy(tools = tools :+ tool)

  def addTools(tools: ServerTool[?, ?, F, ServerContext[F]]*): McpServer[F] =
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

  /** Adds the security input, the error output which describes a rejection, and the logic which validates the security input and makes the
    * principal. The principal is given to the logic of the tools which are added to the returned server.
    */
  def serverSecurityLogic[S, E, P](securityInput: EndpointInput[S], errorOutput: EndpointOutput[E])(
      logic: S => F[Either[E, P]]
  ): SecuredMcpServer[F, S, E, P] =
    SecuredMcpServer(this, securityInput, errorOutput, _ => logic)

  /** The same as [[serverSecurityLogic]], but for security logic which needs no effect. */
  def serverSecurityLogicPure[S, E, P](securityInput: EndpointInput[S], errorOutput: EndpointOutput[E])(
      logic: S => Either[E, P]
  ): SecuredMcpServer[F, S, E, P] =
    SecuredMcpServer(this, securityInput, errorOutput, monad => input => monad.unit(logic(input)))

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
    tools: List[ServerTool[?, ?, F, StreamingServerContext[F]]] = Nil,
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

  def addTool(tool: ServerTool[?, ?, F, ServerContext[F]]): StreamingMcpServer[F] =
    copy(tools = tools :+ tool)

  def addTools(tools: ServerTool[?, ?, F, ServerContext[F]]*): StreamingMcpServer[F] =
    copy(tools = this.tools ++ tools)

  def addStreamingTool(tool: ServerTool[?, ?, F, StreamingServerContext[F]]): StreamingMcpServer[F] =
    copy(tools = tools :+ tool)

  def addStreamingTools(tools: ServerTool[?, ?, F, StreamingServerContext[F]]*): StreamingMcpServer[F] =
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

/** An [[McpServer]] with security logic, which runs before the server handles an MCP message. The result of the security logic, the
  * principal, is given to the logic of the tools which are added to this server. Tools of the initial server, which do not need the
  * principal, are kept.
  *
  * @tparam S
  *   The type of the security input, for example a bearer token.
  * @tparam E
  *   The type of the error output, which the server sends if the security logic gives a rejection.
  * @tparam P
  *   The type of the principal, which the security logic makes from the security input.
  */
case class SecuredMcpServer[F[_], S, E, P](
    server: McpServer[F],
    securityInput: EndpointInput[S],
    errorOutput: EndpointOutput[E],
    securityLogic: MonadError[F] => S => F[Either[E, P]],
    securedTools: List[ServerTool[?, ?, F, SecuredServerContext[F, P]]] = Nil
) extends McpServerDef[F, SecuredServerContext[F, P]]:
  def name: String = server.name
  def version: String = server.version
  def instructions: Option[String] = server.instructions
  def showJsonSchemaMetadata: Boolean = server.showJsonSchemaMetadata
  def originCheck: OriginCheck = server.originCheck
  def prompts: List[ServerPrompt[F]] = server.prompts
  def resources: List[ServerResource[F]] = server.resources
  def resourceTemplates: List[ServerResourceTemplate[F]] = server.resourceTemplates
  def completion: Option[CompletionHandler[F]] = server.completion
  def loggingLevel: Option[SetLoggingLevelHandler[F]] = server.loggingLevel
  def subscriptions: Option[ResourceSubscriptions[F]] = server.subscriptions

  def tools: List[ServerTool[?, ?, F, SecuredServerContext[F, P]]] = server.tools ++ securedTools

  def name(value: String): SecuredMcpServer[F, S, E, P] =
    copy(server = server.name(value))

  def version(value: String): SecuredMcpServer[F, S, E, P] =
    copy(server = server.version(value))

  def instructions(value: String): SecuredMcpServer[F, S, E, P] =
    copy(server = server.instructions(value))

  def withJsonSchemaMetadata(value: Boolean): SecuredMcpServer[F, S, E, P] =
    copy(server = server.withJsonSchemaMetadata(value))

  def withOriginCheck(value: OriginCheck): SecuredMcpServer[F, S, E, P] =
    copy(server = server.withOriginCheck(value))

  def addTool(tool: ServerTool[?, ?, F, SecuredServerContext[F, P]]): SecuredMcpServer[F, S, E, P] =
    copy(securedTools = securedTools :+ tool)

  def addTools(tools: ServerTool[?, ?, F, SecuredServerContext[F, P]]*): SecuredMcpServer[F, S, E, P] =
    copy(securedTools = this.securedTools ++ tools)

  def addPrompt(prompt: ServerPrompt[F]): SecuredMcpServer[F, S, E, P] =
    copy(server = server.addPrompt(prompt))

  def addPrompts(prompts: ServerPrompt[F]*): SecuredMcpServer[F, S, E, P] =
    copy(server = server.addPrompts(prompts*))

  def addResource(resource: ServerResource[F]): SecuredMcpServer[F, S, E, P] =
    copy(server = server.addResource(resource))

  def addResources(resources: ServerResource[F]*): SecuredMcpServer[F, S, E, P] =
    copy(server = server.addResources(resources*))

  def addResourceTemplate(resourceTemplate: ServerResourceTemplate[F]): SecuredMcpServer[F, S, E, P] =
    copy(server = server.addResourceTemplate(resourceTemplate))

  def addResourceTemplates(resourceTemplates: ServerResourceTemplate[F]*): SecuredMcpServer[F, S, E, P] =
    copy(server = server.addResourceTemplates(resourceTemplates*))

  def withCompletion(handler: CompletionHandler[F]): SecuredMcpServer[F, S, E, P] =
    copy(server = server.withCompletion(handler))

  def withLoggingLevel(handler: SetLoggingLevelHandler[F]): SecuredMcpServer[F, S, E, P] =
    copy(server = server.withLoggingLevel(handler))

  def withSubscriptions(handler: ResourceSubscriptions[F]): SecuredMcpServer[F, S, E, P] =
    copy(server = server.withSubscriptions(handler))

  def endpoint(path: List[String]): ServerEndpoint[Any, F] = SecuredServerHttpTransport[F, S, E, P](path).serve(this)

  def streaming: SecuredStreamingMcpServer[F, S, E, P] = SecuredStreamingMcpServer(this)

/** A [[SecuredMcpServer]] which also accepts streaming tools, which are given a [[SecuredStreamingServerContext]] combining the
  * principal with the [[StreamingServerContext]]. Tools of the initial secured server, which need only the principal, are kept.
  */
case class SecuredStreamingMcpServer[F[_], S, E, P](
    server: SecuredMcpServer[F, S, E, P],
    streamingTools: List[ServerTool[?, ?, F, SecuredStreamingServerContext[F, P]]] = Nil
) extends McpServerDef[F, SecuredStreamingServerContext[F, P]]:
  def name: String = server.name
  def version: String = server.version
  def instructions: Option[String] = server.instructions
  def showJsonSchemaMetadata: Boolean = server.showJsonSchemaMetadata
  def originCheck: OriginCheck = server.originCheck
  def prompts: List[ServerPrompt[F]] = server.prompts
  def resources: List[ServerResource[F]] = server.resources
  def resourceTemplates: List[ServerResourceTemplate[F]] = server.resourceTemplates
  def completion: Option[CompletionHandler[F]] = server.completion
  def loggingLevel: Option[SetLoggingLevelHandler[F]] = server.loggingLevel
  def subscriptions: Option[ResourceSubscriptions[F]] = server.subscriptions

  def securityInput: EndpointInput[S] = server.securityInput
  def errorOutput: EndpointOutput[E] = server.errorOutput
  def securityLogic: MonadError[F] => S => F[Either[E, P]] = server.securityLogic

  def tools: List[ServerTool[?, ?, F, SecuredStreamingServerContext[F, P]]] = server.tools ++ streamingTools

  def name(value: String): SecuredStreamingMcpServer[F, S, E, P] =
    copy(server = server.name(value))

  def version(value: String): SecuredStreamingMcpServer[F, S, E, P] =
    copy(server = server.version(value))

  def instructions(value: String): SecuredStreamingMcpServer[F, S, E, P] =
    copy(server = server.instructions(value))

  def withJsonSchemaMetadata(value: Boolean): SecuredStreamingMcpServer[F, S, E, P] =
    copy(server = server.withJsonSchemaMetadata(value))

  def withOriginCheck(value: OriginCheck): SecuredStreamingMcpServer[F, S, E, P] =
    copy(server = server.withOriginCheck(value))

  def addTool(tool: ServerTool[?, ?, F, SecuredServerContext[F, P]]): SecuredStreamingMcpServer[F, S, E, P] =
    copy(server = server.addTool(tool))

  def addTools(tools: ServerTool[?, ?, F, SecuredServerContext[F, P]]*): SecuredStreamingMcpServer[F, S, E, P] =
    copy(server = server.addTools(tools*))

  def addStreamingTool(tool: ServerTool[?, ?, F, SecuredStreamingServerContext[F, P]]): SecuredStreamingMcpServer[F, S, E, P] =
    copy(streamingTools = streamingTools :+ tool)

  def addStreamingTools(tools: ServerTool[?, ?, F, SecuredStreamingServerContext[F, P]]*): SecuredStreamingMcpServer[F, S, E, P] =
    copy(streamingTools = this.streamingTools ++ tools)

  def addPrompt(prompt: ServerPrompt[F]): SecuredStreamingMcpServer[F, S, E, P] =
    copy(server = server.addPrompt(prompt))

  def addPrompts(prompts: ServerPrompt[F]*): SecuredStreamingMcpServer[F, S, E, P] =
    copy(server = server.addPrompts(prompts*))

  def addResource(resource: ServerResource[F]): SecuredStreamingMcpServer[F, S, E, P] =
    copy(server = server.addResource(resource))

  def addResources(resources: ServerResource[F]*): SecuredStreamingMcpServer[F, S, E, P] =
    copy(server = server.addResources(resources*))

  def addResourceTemplate(resourceTemplate: ServerResourceTemplate[F]): SecuredStreamingMcpServer[F, S, E, P] =
    copy(server = server.addResourceTemplate(resourceTemplate))

  def addResourceTemplates(resourceTemplates: ServerResourceTemplate[F]*): SecuredStreamingMcpServer[F, S, E, P] =
    copy(server = server.addResourceTemplates(resourceTemplates*))

  def withCompletion(handler: CompletionHandler[F]): SecuredStreamingMcpServer[F, S, E, P] =
    copy(server = server.withCompletion(handler))

  def withLoggingLevel(handler: SetLoggingLevelHandler[F]): SecuredStreamingMcpServer[F, S, E, P] =
    copy(server = server.withLoggingLevel(handler))

  def withSubscriptions(handler: ResourceSubscriptions[F]): SecuredStreamingMcpServer[F, S, E, P] =
    copy(server = server.withSubscriptions(handler))
