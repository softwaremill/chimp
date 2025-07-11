package chimp

import sttp.tapir.Schema
import io.circe.Decoder
import sttp.shared.Identity

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

  /** Specify the input type for the tool, providing both a Tapir Schema and a Circe Decoder. */
  def input[I: Schema: Decoder]: Tool[I] = Tool[I](name, description, summon[Schema[I]], summon[Decoder[I]], annotations)

/** Creates a new MCP tool description with the given name. */
def tool(name: String): PartialTool = PartialTool(name)

//

/** Describes a tool after the input is specified. */
case class Tool[I](
    name: String,
    description: Option[String],
    inputSchema: Schema[I],
    inputDecoder: Decoder[I],
    annotations: Option[ToolAnnotations]
):
  /** Combine the tool description with the server logic, that should be executed when the tool is invoked. The logic, given the input,
    * should return either a tool execution error (`Left`), or a successful textual result (`Right`), using the F-effect.
    */
  def serverLogic[F[_]](logic: (I, Option[String]) => F[Either[String, String]]): ServerTool[I, F] =
    ServerTool(name, description, inputSchema, inputDecoder, annotations, logic)

  /** Combine the tool description with the server logic, that should be executed when the tool is invoked. The logic, given the input,
    * should return either a tool execution error (`Left`), or a successful textual result (`Right`).
    *
    * Same as [[serverLogic]], but using the identity "effect".
    */
  def handleWithHeader(logic: (I, Option[String]) => Either[String, String]): ServerTool[I, Identity] =
    ServerTool(name, description, inputSchema, inputDecoder, annotations, (i, t) => logic(i, t))

  def handle(logic: I => Either[String, String]): ServerTool[I, Identity] =
    handleWithHeader((i, _) => logic(i))
    
//

/** A tool that can be executed by the MCP server. */
case class ServerTool[I, F[_]](
    name: String,
    description: Option[String],
    inputSchema: Schema[I],
    inputDecoder: Decoder[I],
    annotations: Option[ToolAnnotations],
    logic: (I, Option[String]) => F[Either[String, String]]
)
