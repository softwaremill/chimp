package chimp.server

import chimp.protocol.{ResourceContents, ToolContent}
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}
import sttp.model.Header
import sttp.shared.Identity
import sttp.tapir.Schema

/** Optional behavioral hints about a tool, surfaced to clients. */
case class ToolAnnotations(
    title: Option[String] = None,
    readOnlyHint: Option[Boolean] = None,
    destructiveHint: Option[Boolean] = None,
    idempotentHint: Option[Boolean] = None,
    openWorldHint: Option[Boolean] = None
)

/** The result of a tool call: content blocks, optional structured output, and whether the call failed. */
case class ToolResult(
    content: List[ToolContent],
    structuredContent: Option[Json] = None,
    isError: Boolean = false
):
  def asError: ToolResult = copy(isError = true)
  def withStructured(json: Json): ToolResult = copy(structuredContent = Some(json))

/** Constructors for the common [[ToolResult]] shapes. */
object ToolResult:
  def text(text: String): ToolResult = ToolResult(List(ToolContent.Text(text = text)))
  def error(message: String): ToolResult = ToolResult(List(ToolContent.Text(text = message)), isError = true)
  def image(data: String, mimeType: String): ToolResult = ToolResult(List(ToolContent.Image(data = data, mimeType = mimeType)))
  def audio(data: String, mimeType: String): ToolResult = ToolResult(List(ToolContent.Audio(data = data, mimeType = mimeType)))
  def embedded(resource: ResourceContents): ToolResult = ToolResult(List(ToolContent.ResourceContent(resource = resource)))
  def content(content: ToolContent*): ToolResult = ToolResult(content.toList)
  def structured[A: Encoder](value: A): ToolResult = ToolResult(Nil, structuredContent = Some(value.asJson))
  def fromEither(result: Either[String, String]): ToolResult = result.fold(error, text)

/** A tool's input schema: either derived from a Scala type or supplied as raw JSON Schema. */
enum ToolSchema:
  case Derived(schema: Schema[?])
  case Raw(json: Json)

/** https://modelcontextprotocol.io/seps/986-specify-format-for-tool-names */
private val ToolNameRegex = "^[A-Za-z0-9_./-]+$".r

/** Starts defining a tool with the given unique name */
def tool(name: String): PartialTool =
  require(name.nonEmpty && name.length <= 64, s"Tool name must be 1..64 characters long, got ${name.length}: $name")
  require(ToolNameRegex.matches(name), s"Tool name must match ${ToolNameRegex.regex}, got: $name")
  PartialTool(name)

/** A tool being defined, before its input type is fixed. */
case class PartialTool(
    name: String,
    description: Option[String] = None,
    annotations: Option[ToolAnnotations] = None
):
  def description(desc: String): PartialTool =
    copy(description = Some(desc))

  def withAnnotations(ann: ToolAnnotations): PartialTool =
    copy(annotations = Some(ann))

  /** Fixes the input type, deriving its JSON Schema and decoder from the given instances. */
  def input[I: Schema: Decoder]: Tool[I] =
    Tool[I](name, description, ToolSchema.Derived(summon[Schema[I]]), summon[Decoder[I]], annotations)

  /** Fixes the input as raw JSON, validated against the given JSON Schema. */
  def inputJson(schema: Json): Tool[Json] = Tool[Json](name, description, ToolSchema.Raw(schema), summon[Decoder[Json]], annotations)

/** A tool with a known input type `I`, ready to be given its handling logic. */
case class Tool[I](
    name: String,
    description: Option[String],
    inputSchema: ToolSchema,
    inputDecoder: Decoder[I],
    annotations: Option[ToolAnnotations]
):
  /** Attaches effectful logic with access to the base [[ServerContext]]. */
  def serverLogic[F[_]](logic: (I, ServerContext[F], Seq[Header]) => F[ToolResult]): ServerTool[I, F, ServerContext[F]] =
    ServerTool(name, description, inputSchema, inputDecoder, annotations, logic)

  /** Attaches effectful logic with access to the [[StreamingServerContext]]; usable only on a streaming server. */
  def streamingServerLogic[F[_]](
      logic: (I, StreamingServerContext[F], Seq[Header]) => F[ToolResult]
  ): ServerTool[I, F, StreamingServerContext[F]] =
    ServerTool(name, description, inputSchema, inputDecoder, annotations, logic)

  /** Attaches synchronous logic that also receives the request headers. */
  def handleWithHeaders(logic: (I, Seq[Header]) => ToolResult): ServerTool[I, Identity, ServerContext[Identity]] =
    ServerTool(name, description, inputSchema, inputDecoder, annotations, (i, _, headers) => logic(i, headers))

  /** Attaches synchronous logic over just the decoded input. */
  def handle(logic: I => ToolResult): ServerTool[I, Identity, ServerContext[Identity]] =
    handleWithHeaders((i, _) => logic(i))

/** A fully-defined tool: its metadata plus the logic handling a call, in effect `F` with context `C`. */
case class ServerTool[I, F[_], -C <: ServerContext[F]](
    name: String,
    description: Option[String],
    inputSchema: ToolSchema,
    inputDecoder: Decoder[I],
    annotations: Option[ToolAnnotations],
    logic: (I, C, Seq[Header]) => F[ToolResult]
)
