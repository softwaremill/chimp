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

/** The structured output type of a tool that returns none. A distinct type rather than `Unit`, which value discarding would let any
  * structured result conform to.
  */
sealed trait NoStructuredOutput

/** The result of a tool call: content blocks, optional structured output, and whether the call failed. `O` is the type of the structured
  * output, [[NoStructuredOutput]] when there is none.
  */
case class ToolResult[+O](
    content: List[ToolContent],
    structuredContent: Option[Json] = None,
    isError: Boolean = false
):
  def asError: ToolResult[O] = copy(isError = true)

  /** Adds structured output, which fixes the result's output type. */
  def withStructured[O2: Encoder](value: O2): ToolResult[O2] = ToolResult(content, Some(value.asJson), isError)

/** Constructors for the common [[ToolResult]] shapes. An error is a valid result whatever the output type, hence `ToolResult[Nothing]`. */
object ToolResult:
  def text(text: String): ToolResult[NoStructuredOutput] = ToolResult(List(ToolContent.Text(text = text)))
  def error(message: String): ToolResult[Nothing] = ToolResult(List(ToolContent.Text(text = message)), isError = true)
  def image(data: String, mimeType: String): ToolResult[NoStructuredOutput] =
    ToolResult(List(ToolContent.Image(data = data, mimeType = mimeType)))
  def audio(data: String, mimeType: String): ToolResult[NoStructuredOutput] =
    ToolResult(List(ToolContent.Audio(data = data, mimeType = mimeType)))
  def embedded(resource: ResourceContents): ToolResult[NoStructuredOutput] =
    ToolResult(List(ToolContent.ResourceContent(resource = resource)))
  def content(content: ToolContent*): ToolResult[NoStructuredOutput] = ToolResult(content.toList)
  def structured[O: Encoder](value: O): ToolResult[O] = ToolResult(Nil, structuredContent = Some(value.asJson))
  def fromEither(result: Either[String, String]): ToolResult[NoStructuredOutput] = result.fold(error, text)

/** A tool's input or output schema: either derived from a Scala type or supplied as raw JSON Schema. */
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
  def input[I: Schema: Decoder]: Tool[I, NoStructuredOutput] =
    Tool[I, NoStructuredOutput](name, description, ToolSchema.Derived(summon[Schema[I]]), summon[Decoder[I]], None, annotations)

  /** Fixes the input as raw JSON, validated against the given JSON Schema. */
  def inputJson(schema: Json): Tool[Json, NoStructuredOutput] =
    Tool[Json, NoStructuredOutput](name, description, ToolSchema.Raw(schema), summon[Decoder[Json]], None, annotations)

/** A tool with a known input type `I` and structured output type `O`, ready to be given its handling logic. */
case class Tool[I, O](
    name: String,
    description: Option[String],
    inputSchema: ToolSchema,
    inputDecoder: Decoder[I],
    outputSchema: Option[ToolSchema],
    annotations: Option[ToolAnnotations]
):
  /** Fixes the structured output type, deriving its JSON Schema from the given instance. */
  def output[O2: Schema]: Tool[I, O2] =
    Tool[I, O2](name, description, inputSchema, inputDecoder, Some(ToolSchema.Derived(summon[Schema[O2]])), annotations)

  /** Fixes the structured output as raw JSON, described by the given JSON Schema. */
  def outputJson(schema: Json): Tool[I, Json] =
    Tool[I, Json](name, description, inputSchema, inputDecoder, Some(ToolSchema.Raw(schema)), annotations)

  /** Attaches effectful logic, with access to the request headers. */
  def serverLogic[F[_]](logic: (I, Seq[Header]) => F[ToolResult[O]]): ServerTool[I, O, F, ServerContext[F]] =
    ServerTool(name, description, inputSchema, inputDecoder, outputSchema, annotations, (input, _, headers) => logic(input, headers))

  /** Attaches effectful logic with access to the [[StreamingServerContext]]; usable only on a streaming server. */
  def streamingServerLogic[F[_]](
      logic: (I, StreamingServerContext[F], Seq[Header]) => F[ToolResult[O]]
  ): ServerTool[I, O, F, StreamingServerContext[F]] =
    ServerTool(name, description, inputSchema, inputDecoder, outputSchema, annotations, logic)

  /** Attaches effectful logic with access to a [[TaskContext]], so the tool can request input from the client while running as a task
    * (Tasks extension). Register it with `addTaskTool`; such a tool is always answered with a task.
    */
  def taskLogic[F[_]](logic: (I, TaskContext[F], Seq[Header]) => F[ToolResult[O]]): ServerTool[I, O, F, TaskContext[F]] =
    ServerTool(name, description, inputSchema, inputDecoder, outputSchema, annotations, logic)

  /** Attaches synchronous logic that also receives the request headers. */
  def handleWithHeaders(logic: (I, Seq[Header]) => ToolResult[O]): ServerTool[I, O, Identity, ServerContext[Identity]] =
    ServerTool(name, description, inputSchema, inputDecoder, outputSchema, annotations, (i, _, headers) => logic(i, headers))

  /** Attaches synchronous logic over just the decoded input. */
  def handle(logic: I => ToolResult[O]): ServerTool[I, O, Identity, ServerContext[Identity]] =
    handleWithHeaders((i, _) => logic(i))

/** A fully-defined tool: its metadata plus the logic handling a call, in effect `F` with context `C`. */
case class ServerTool[I, O, F[_], -C <: ServerContext[F]](
    name: String,
    description: Option[String],
    inputSchema: ToolSchema,
    inputDecoder: Decoder[I],
    outputSchema: Option[ToolSchema],
    annotations: Option[ToolAnnotations],
    logic: (I, C, Seq[Header]) => F[ToolResult[O]]
)
