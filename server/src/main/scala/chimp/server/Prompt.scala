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
  def serverLogic[F[_]](logic: (Map[String, String], Seq[Header]) => F[GetPromptResult]): ServerPrompt[F] =
    ServerPrompt(definition, logic)

  /** Attaches synchronous logic that also receives the request headers. */
  def handleWithHeaders(logic: (Map[String, String], Seq[Header]) => GetPromptResult): ServerPrompt[Identity] =
    ServerPrompt(definition, logic)

  /** Attaches synchronous logic over just the supplied argument values. */
  def handle(logic: Map[String, String] => GetPromptResult): ServerPrompt[Identity] =
    handleWithHeaders((args, _) => logic(args))

  private def definition: Prompt =
    Prompt(name, title, description, Option.when(arguments.nonEmpty)(arguments))

end PartialPrompt

/** A fully-defined prompt: its metadata plus the logic producing its messages. */
case class ServerPrompt[F[_]](definition: Prompt, logic: (Map[String, String], Seq[Header]) => F[GetPromptResult])
