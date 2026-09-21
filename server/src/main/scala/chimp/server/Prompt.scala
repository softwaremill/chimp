package chimp.server

import chimp.protocol.{GetPromptResult, Prompt, PromptArgument}
import sttp.model.Header
import sttp.shared.Identity

/** Starts defining a prompt with the given name. */
def prompt(name: String): PartialPrompt = PartialPrompt(name)

/** A prompt being defined, before its logic is attached. */
case class PartialPrompt(
    name: String,
    title: Option[String] = None,
    description: Option[String] = None,
    arguments: List[PromptArgument] = Nil
):
  def title(value: String): PartialPrompt =
    copy(title = Some(value))

  def description(value: String): PartialPrompt =
    copy(description = Some(value))

  /** Declares a single argument the prompt accepts. */
  def argument(name: String, description: Option[String] = None, required: Boolean = false): PartialPrompt =
    copy(arguments = arguments :+ PromptArgument(name, description, required = Some(required)))

  /** Declares multiple arguments the prompt accepts. */
  def arguments(args: PromptArgument*): PartialPrompt =
    copy(arguments = arguments ++ args)

  /** Attaches effectful logic, with access to the request headers, producing the prompt's messages. */
  def serverLogic[F[_]](
      logic: (Map[String, String], Seq[Header]) => F[GetPromptResult]
  ): ServerPrompt[F, ServerContext[F]] =
    ServerPrompt(definition, (args, _, headers) => logic(args, headers))

  /** Attaches effectful logic, with access to the principal; usable only on a [[SecuredMcpServer]]. */
  def securedServerLogic[F[_], P](
      logic: (Map[String, String], P, Seq[Header]) => F[GetPromptResult]
  ): ServerPrompt[F, SecuredServerContext[F, P]] =
    ServerPrompt(definition, (args, context, headers) => logic(args, context.principal, headers))

  /** Attaches synchronous logic that also receives the request headers. */
  def handleWithHeaders(
      logic: (Map[String, String], Seq[Header]) => GetPromptResult
  ): ServerPrompt[Identity, ServerContext[Identity]] =
    serverLogic[Identity](logic)

  /** Attaches synchronous logic over just the supplied argument values. */
  def handle(logic: Map[String, String] => GetPromptResult): ServerPrompt[Identity, ServerContext[Identity]] =
    handleWithHeaders((args, _) => logic(args))

  /** Attaches synchronous logic over the supplied argument values and the principal; usable only on a [[SecuredMcpServer]]. */
  def handleSecured[P](
      logic: (Map[String, String], P) => GetPromptResult
  ): ServerPrompt[Identity, SecuredServerContext[Identity, P]] =
    securedServerLogic[Identity, P]((args, principal, _) => logic(args, principal))

  private def definition: Prompt =
    Prompt(name, title, description, Option.when(arguments.nonEmpty)(arguments))

end PartialPrompt

/** A fully-defined prompt: its metadata plus the logic producing its messages. */
case class ServerPrompt[F[_], -C <: ServerContext[F]](
    definition: Prompt,
    logic: (Map[String, String], C, Seq[Header]) => F[GetPromptResult]
)
