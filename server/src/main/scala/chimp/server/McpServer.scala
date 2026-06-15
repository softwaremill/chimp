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

/** An MCP server definition: the features it exposes and the metadata it reports. Build one up with the `add*`/`with*` methods, then turn
  * it into a Tapir endpoint with [[endpoint]]. Server capabilities are derived from which features are registered.
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
):
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

  /** Build the Tapir endpoint serving this MCP server at the given path. */
  def endpoint(path: List[String]): ServerEndpoint[Any, F] = buildEndpoint(this, path)
