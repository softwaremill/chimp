package chimp.server

import chimp.protocol.{GetPromptResult, Prompt, PromptArgument}
import sttp.model.Header
import sttp.shared.Identity

/** Creates a new MCP prompt description with the given name. */
def prompt(name: String): PartialPrompt = PartialPrompt(name)

/** Describes a prompt before its logic is specified. */
case class PartialPrompt(
    name: String,
    title: Option[String] = None,
    description: Option[String] = None,
    arguments: List[PromptArgument] = Nil
):
  def title(value: String): PartialPrompt = copy(title = Some(value))
  def description(value: String): PartialPrompt = copy(description = Some(value))
  def argument(name: String, description: Option[String] = None, required: Boolean = false): PartialPrompt =
    copy(arguments = arguments :+ PromptArgument(name, description, required = Some(required)))
  def arguments(args: PromptArgument*): PartialPrompt = copy(arguments = arguments ++ args)

  /** Combine the prompt description with logic that, given the resolved arguments, produces the prompt messages in the F-effect. */
  def get[F[_]](logic: Map[String, String] => F[GetPromptResult]): ServerPrompt[F] =
    ServerPrompt(definition, (args, _) => logic(args))

  /** Same as [[get]], but with synchronous logic. */
  def handle(logic: Map[String, String] => GetPromptResult): ServerPrompt[Identity] =
    ServerPrompt(definition, (args, _) => logic(args))

  private def definition: Prompt = Prompt(name, title, description, Option.when(arguments.nonEmpty)(arguments))

/** A prompt that can be retrieved from the MCP server. */
case class ServerPrompt[F[_]](definition: Prompt, logic: (Map[String, String], Seq[Header]) => F[GetPromptResult])
