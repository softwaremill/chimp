package chimp.protocol

import com.networknt.schema.{InputFormat, SchemaRegistry, SpecificationVersion}
import io.circe.syntax.*
import io.circe.{Decoder, Encoder}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/** Validates the 2026-07-28 (modern) protocol datatypes against the official MCP JSON schema, mirroring [[SchemaConformanceSpec]]. */
class Schema2026ConformanceSpec extends AnyFlatSpec with Matchers:

  private val registry = SchemaRegistry.withDefaultDialect(SpecificationVersion.DRAFT_2020_12)

  private val defsText: String =
    val stream = getClass.getResourceAsStream("/schema/2026-07-28/schema.json")
    require(stream != null, "MCP schema not found on the classpath at /schema/2026-07-28/schema.json")
    val text = String(stream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
    stream.close()
    io.circe.parser
      .parse(text)
      .getOrElse(throw RuntimeException("Could not parse the bundled MCP 2026-07-28 schema as JSON"))
      .hcursor
      .downField("$defs")
      .focus
      .getOrElse(throw RuntimeException("Schema root is missing $defs object"))
      .noSpaces

  private def validate[T: Encoder: Decoder](defName: String, value: T): Unit =
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

  it should "produce a DiscoverResult that matches the spec schema" in:
    validate(
      "DiscoverResult",
      DiscoverResult(
        supportedVersions = List("2026-07-28", "2025-11-25"),
        capabilities = ServerCapabilities(tools = Some(ServerToolsCapability(listChanged = Some(false)))),
        ttlMs = 0.millis,
        cacheScope = CacheScope.Private,
        instructions = Some("welcome"),
        _meta = Some(Map(ProtocolMeta.ServerInfo -> Implementation(name = "chimp", version = "1.0").asJson.deepDropNullValues))
      )
    )

  it should "produce an UnsupportedProtocolVersionError envelope that matches the spec schema" in:
    val msg: JSONRPCMessage =
      JSONRPCMessage.Error(id = RequestId(1), error = ProtocolMeta.unsupportedVersionError("1900-01-01", List("2026-07-28", "2025-11-25")))
    validate("UnsupportedProtocolVersionError", msg)
