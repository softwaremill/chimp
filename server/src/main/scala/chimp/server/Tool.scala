package chimp.server

import chimp.protocol.{ResourceContents, ToolContent}
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}
import sttp.model.Header
import sttp.shared.Identity
import sttp.tapir.Schema

case class ToolAnnotations(
    title: Option[String] = None,
    readOnlyHint: Option[Boolean] = None,
    destructiveHint: Option[Boolean] = None,
    idempotentHint: Option[Boolean] = None,
    openWorldHint: Option[Boolean] = None
)

case class ToolResult(
    content: List[ToolContent],
    structuredContent: Option[Json] = None,
    isError: Boolean = false
):
  def asError: ToolResult = copy(isError = true)
  def withStructured(json: Json): ToolResult = copy(structuredContent = Some(json))

object ToolResult:
  def text(text: String): ToolResult = ToolResult(List(ToolContent.Text(text = text)))
  def error(message: String): ToolResult = ToolResult(List(ToolContent.Text(text = message)), isError = true)
  def image(data: String, mimeType: String): ToolResult = ToolResult(List(ToolContent.Image(data = data, mimeType = mimeType)))
  def audio(data: String, mimeType: String): ToolResult = ToolResult(List(ToolContent.Audio(data = data, mimeType = mimeType)))
  def embedded(resource: ResourceContents): ToolResult = ToolResult(List(ToolContent.ResourceContent(resource = resource)))
  def content(content: ToolContent*): ToolResult = ToolResult(content.toList)
  def structured[A: Encoder](value: A): ToolResult = ToolResult(Nil, structuredContent = Some(value.asJson))
  def fromEither(result: Either[String, String]): ToolResult = result.fold(error, text)

enum ToolSchema:
  case Derived(schema: Schema[?])
  case Raw(json: Json)

private val ToolNameRegex = "^[A-Za-z0-9_./-]+$".r

def tool(name: String): PartialTool =
  require(name.nonEmpty && name.length <= 64, s"Tool name must be 1..64 characters long, got ${name.length}: $name")
  require(ToolNameRegex.matches(name), s"Tool name must match ${ToolNameRegex.regex}, got: $name")
  PartialTool(name)

case class PartialTool(
    name: String,
    description: Option[String] = None,
    annotations: Option[ToolAnnotations] = None
):
  def description(desc: String): PartialTool =
    copy(description = Some(desc))

  def withAnnotations(ann: ToolAnnotations): PartialTool =
    copy(annotations = Some(ann))

  def input[I: Schema: Decoder]: Tool[I] =
    Tool[I](name, description, ToolSchema.Derived(summon[Schema[I]]), summon[Decoder[I]], annotations)

  def inputJson(schema: Json): Tool[Json] = Tool[Json](name, description, ToolSchema.Raw(schema), summon[Decoder[Json]], annotations)

case class Tool[I](
    name: String,
    description: Option[String],
    inputSchema: ToolSchema,
    inputDecoder: Decoder[I],
    annotations: Option[ToolAnnotations]
):
  def serverLogic[F[_]](logic: (I, ServerContext[F], Seq[Header]) => F[ToolResult]): ServerTool[I, F, ServerContext[F]] =
    ServerTool(name, description, inputSchema, inputDecoder, annotations, logic)

  def streamingServerLogic[F[_]](
      logic: (I, StreamingServerContext[F], Seq[Header]) => F[ToolResult]
  ): ServerTool[I, F, StreamingServerContext[F]] =
    ServerTool(name, description, inputSchema, inputDecoder, annotations, logic)

  def handleWithHeaders(logic: (I, Seq[Header]) => ToolResult): ServerTool[I, Identity, ServerContext[Identity]] =
    ServerTool(name, description, inputSchema, inputDecoder, annotations, (i, _, headers) => logic(i, headers))

  def handle(logic: I => ToolResult): ServerTool[I, Identity, ServerContext[Identity]] =
    handleWithHeaders((i, _) => logic(i))

case class ServerTool[I, F[_], -C <: ServerContext[F]](
    name: String,
    description: Option[String],
    inputSchema: ToolSchema,
    inputDecoder: Decoder[I],
    annotations: Option[ToolAnnotations],
    logic: (I, C, Seq[Header]) => F[ToolResult]
)
