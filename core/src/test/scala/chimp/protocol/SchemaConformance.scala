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
