package chimp.protocol

import com.networknt.schema.{InputFormat, SchemaRegistry, SpecificationVersion}
import io.circe.syntax.*
import io.circe.{Decoder, Encoder}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters.*

/** Shared harness that validates encoded protocol datatypes against the official MCP JSON schema of a given version. One concrete spec per
  * version supplies [[schemaResourcePath]] and its own cases.
  */
trait SchemaConformance extends AnyFlatSpec with Matchers:

  /** Classpath resource path of the version's `schema.json`, e.g. `/schema/2025-11-25/schema.json`. */
  def schemaResourcePath: String

  private val registry = SchemaRegistry.withDefaultDialect(SpecificationVersion.DRAFT_2020_12)

  private lazy val defsText: String =
    val stream = getClass.getResourceAsStream(schemaResourcePath)
    require(stream != null, s"MCP schema not found on the classpath at $schemaResourcePath")
    val text = String(stream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
    stream.close()
    io.circe.parser
      .parse(text)
      .getOrElse(throw RuntimeException(s"Could not parse the bundled MCP schema at $schemaResourcePath"))
      .hcursor
      .downField("$defs")
      .focus
      .getOrElse(throw RuntimeException("Schema root is missing $defs object"))
      .noSpaces

  protected def validate[T: Encoder: Decoder](defName: String, value: T): Unit =
    val encodedJson = value.asJson.deepDropNullValues
    val encodedStr = encodedJson.noSpaces
    val wrapper =
      s"""{"$$schema":"https://json-schema.org/draft/2020-12/schema","$$ref":"#/$$defs/$defName","$$defs":$defsText}"""
    val schema = registry.getSchema(wrapper, InputFormat.JSON)
    val errors = schema.validate(encodedStr, InputFormat.JSON).asScala.toList
    withClue(s"Encoded JSON ($defName):\n$encodedStr\nViolations:\n${errors.mkString("\n")}\n"):
      errors shouldBe empty
    val _ = encodedJson.as[T] match
      case Right(decoded) =>
        withClue(s"Round-trip mismatch ($defName):\nencoded: $encodedStr\n"):
          decoded shouldBe value
      case Left(err) =>
        fail(s"Decode round-trip failed for $defName:\nencoded: $encodedStr\nerror: ${err.getMessage}")

  // Shared metadata types (icons, annotations, _meta, title) are byte-identical across the 2025-11-25 and 2026-07-28 schemas, so both
  // concrete specs validate them. A field regression that breaks the older revision must not be invisible (AGENTS.md rule 2).
  private val sampleIcon =
    Icon(src = "https://example.com/icon.png", mimeType = Some("image/png"), sizes = Some(List("48x48")), theme = Some(IconTheme.Dark))

  it should "produce an Icon that matches the spec schema" in:
    validate("Icon", sampleIcon)

  it should "produce Annotations that match the spec schema" in:
    validate(
      "Annotations",
      Annotations(
        audience = Some(List(Role.User, Role.Assistant)),
        lastModified = Some("2026-01-12T15:00:58Z"),
        priority = Some(Priority(0.5))
      )
    )

  it should "produce an Implementation with icons, websiteUrl and description that matches the spec schema" in:
    validate(
      "Implementation",
      Implementation(
        "chimp",
        "1.0",
        title = Some("Chimp"),
        icons = Some(List(sampleIcon)),
        websiteUrl = Some("https://example.com"),
        description = Some("test")
      )
    )

  it should "produce a Tool with icons that matches the spec schema" in:
    validate(
      "Tool",
      ToolDefinition(name = "add", inputSchema = io.circe.Json.obj("type" -> "object".asJson), icons = Some(List(sampleIcon)))
    )

  it should "produce a Resource with icons and annotations that matches the spec schema" in:
    validate(
      "Resource",
      Resource(
        uri = "file:///x",
        name = "x",
        icons = Some(List(sampleIcon)),
        annotations = Some(Annotations(priority = Some(Priority(0.3))))
      )
    )

  it should "produce a Prompt with icons that matches the spec schema" in:
    validate("Prompt", Prompt(name = "greet", icons = Some(List(sampleIcon))))

  it should "produce a PromptReference with title that matches the spec schema" in:
    validate("PromptReference", PromptReference(name = "greet", title = Some("Greet")))

  it should "produce a ResourceLink with title, size, icons and annotations that matches the spec schema" in:
    val link: ToolContent = ToolContent.ResourceLink(
      uri = "file:///x",
      name = Some("x"),
      title = Some("X"),
      description = Some("a link"),
      mimeType = Some("text/plain"),
      size = Some(10L),
      icons = Some(List(sampleIcon)),
      annotations = Some(Annotations(priority = Some(Priority(0.1))))
    )
    validate("ResourceLink", link)

  it should "produce TextContent with annotations that matches the spec schema" in:
    val text: ToolContent = ToolContent.Text(text = "hi", annotations = Some(Annotations(audience = Some(List(Role.Assistant)))))
    validate("TextContent", text)
