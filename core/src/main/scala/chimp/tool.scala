package chimp

import sttp.tapir.Codec.JsonCodec

case class ToolAnnotations(
    title: Option[String] = None,
    readOnlyHint: Option[Boolean] = None,
    destructiveHint: Option[Boolean] = None,
    idempotentHint: Option[Boolean] = None,
    openWorldHint: Option[Boolean] = None
)

/** Describes a tool before the input is specified. */
case class PartialTool(
    name: String,
    description: Option[String] = None,
    annotations: Option[ToolAnnotations] = None
):
  def description(desc: String): PartialTool = copy(description = Some(desc))
  def withAnnotations(ann: ToolAnnotations): PartialTool = copy(annotations = Some(ann))
  def input[I: JsonCodec]: Tool[I] = Tool[I](name, description, summon[JsonCodec[I]], annotations)

def tool(name: String): PartialTool = PartialTool(name)

//

/** Describes a tool after the input is specified. */
case class Tool[I](
    name: String,
    description: Option[String],
    inputCodec: JsonCodec[I],
    annotations: Option[ToolAnnotations]
):
  /** Given the input, returns either a tool execution error (`Left`), or a successful textual result (`Right`). */
  def handle(logic: I => Either[String, String]): ServerTool[I] =
    ServerTool(name, description, inputCodec, annotations, logic)

//

case class ServerTool[I](
    name: String,
    description: Option[String],
    inputCodec: JsonCodec[I],
    annotations: Option[ToolAnnotations],
    logic: I => Either[String, String]
)
