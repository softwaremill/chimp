package chimp

import sttp.tapir.Schema

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
  def input[I: Schema]: Tool[I] = Tool[I](name, description, summon[Schema[I]], annotations)

def tool(name: String): PartialTool = PartialTool(name)

//

/** Describes a tool after the input is specified. */
case class Tool[I](
    name: String,
    description: Option[String],
    inputSchema: Schema[I],
    annotations: Option[ToolAnnotations]
):
  /** Given the input, returns either a tool execution error (`Left`), or a successful textual result (`Right`). */
  def handle(logic: I => Either[String, String]): ServerTool[I] =
    ServerTool(name, description, inputSchema, annotations, Some(logic))

//

case class ServerTool[I](
    name: String,
    description: Option[String],
    inputSchema: Schema[I],
    annotations: Option[ToolAnnotations],
    logic: Option[I => Either[String, String]]
)
