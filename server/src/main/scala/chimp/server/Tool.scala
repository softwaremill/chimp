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

/** The result of a tool invocation: a list of content items (text, image, audio, embedded resource, …), an optional structured payload, and
  * a flag marking the result as an error.
  */
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

/** A tool's input schema, either derived from a Tapir [[Schema]] or supplied directly as raw JSON Schema (for dialects Tapir cannot express,
  * e.g. JSON Schema 2020-12 with `additionalProperties: false`).
  */
enum ToolSchema:
  case Derived(schema: Schema[?])
  case Raw(json: Json)

private val ToolNameRegex = "^[A-Za-z0-9_./-]+$".r

/** Creates a new MCP tool description with the given name. The name must match `^[A-Za-z0-9_./-]+$` and be 1–64 characters long. */
def tool(name: String): PartialTool =
  require(name.length >= 1 && name.length <= 64, s"Tool name must be 1..64 characters long, got ${name.length}: $name")
  require(ToolNameRegex.matches(name), s"Tool name must match ${ToolNameRegex.regex}, got: $name")
  PartialTool(name)

/** Describes a tool before the input is specified. */
case class PartialTool(
    name: String,
    description: Option[String] = None,
    annotations: Option[ToolAnnotations] = None
):
  def description(desc: String): PartialTool = copy(description = Some(desc))
  def withAnnotations(ann: ToolAnnotations): PartialTool = copy(annotations = Some(ann))

  /** Specify the input type for the tool, providing both a Tapir Schema and a Circe Decoder. */
  def input[I: Schema: Decoder]: Tool[I] = Tool[I](name, description, ToolSchema.Derived(summon[Schema[I]]), summon[Decoder[I]], annotations)

  /** Specify the tool's input schema directly as raw JSON Schema. The tool receives its arguments as raw [[Json]]. */
  def inputJson(schema: Json): Tool[Json] = Tool[Json](name, description, ToolSchema.Raw(schema), summon[Decoder[Json]], annotations)

/** Describes a tool after the input is specified. */
case class Tool[I](
    name: String,
    description: Option[String],
    inputSchema: ToolSchema,
    inputDecoder: Decoder[I],
    annotations: Option[ToolAnnotations]
):
  /** Combine the tool description with the server logic, executed when the tool is invoked. The logic, given the input, a request-scoped
    * [[ServerContext]], and the request headers, returns a [[ToolResult]] in the F-effect.
    */
  def serverLogic[F[_]](logic: (I, ServerContext[F], Seq[Header]) => F[ToolResult]): ServerTool[I, F, ServerContext[F]] =
    ServerTool(name, description, inputSchema, inputDecoder, annotations, logic)

  /** Like [[serverLogic]], but the logic receives a [[StreamingServerContext]], so it may report progress, log, and issue sampling and
    * elicitation requests. Tools defined this way are accepted only by the streaming endpoint.
    */
  def streamingServerLogic[F[_]](
      logic: (I, StreamingServerContext[F], Seq[Header]) => F[ToolResult]
  ): ServerTool[I, F, StreamingServerContext[F]] =
    ServerTool(name, description, inputSchema, inputDecoder, annotations, logic)

  /** Combine the tool description with synchronous server logic that also receives the request headers. */
  def handleWithHeaders(logic: (I, Seq[Header]) => ToolResult): ServerTool[I, Identity, ServerContext[Identity]] =
    ServerTool(name, description, inputSchema, inputDecoder, annotations, (i, _, headers) => logic(i, headers))

  /** Combine the tool description with synchronous server logic. Same as [[handleWithHeaders]], but without access to the headers. */
  def handle(logic: I => ToolResult): ServerTool[I, Identity, ServerContext[Identity]] =
    handleWithHeaders((i, _) => logic(i))

/** A tool that can be executed by the MCP server. The context type `C` records which server capabilities the logic requires: a tool needing
  * only the base [[ServerContext]] runs on any server, while one needing a [[StreamingServerContext]] is rejected by the request/response
  * endpoint at compile time.
  */
case class ServerTool[I, F[_], -C <: ServerContext[F]](
    name: String,
    description: Option[String],
    inputSchema: ToolSchema,
    inputDecoder: Decoder[I],
    annotations: Option[ToolAnnotations],
    logic: (I, C, Seq[Header]) => F[ToolResult]
)
