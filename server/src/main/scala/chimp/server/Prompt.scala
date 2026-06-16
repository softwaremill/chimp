package chimp.server

import chimp.protocol.{GetPromptResult, Prompt, PromptArgument}
import sttp.model.Header

def prompt(name: String): PartialPrompt = PartialPrompt(name)

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

  def argument(name: String, description: Option[String] = None, required: Boolean = false): PartialPrompt =
    copy(arguments = arguments :+ PromptArgument(name, description, required = Some(required)))

  def arguments(args: PromptArgument*): PartialPrompt =
    copy(arguments = arguments ++ args)

  def serverLogic[F[_]](logic: Map[String, String] => F[GetPromptResult]): ServerPrompt[F] =
    ServerPrompt(definition, (args, _) => logic(args))

  private def definition: Prompt =
    Prompt(name, title, description, Option.when(arguments.nonEmpty)(arguments))

end PartialPrompt

case class ServerPrompt[F[_]](definition: Prompt, logic: (Map[String, String], Seq[Header]) => F[GetPromptResult])
