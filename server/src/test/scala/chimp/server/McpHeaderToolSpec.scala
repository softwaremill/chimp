package chimp.server

import chimp.protocol.{JSONRPCMessage, RequestId}
import io.circe.parser.parse
import io.circe.syntax.*
import io.circe.{Codec, Json}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.monad.{IdentityMonad, MonadError}
import sttp.shared.Identity
import sttp.tapir.Schema

class McpHeaderToolSpec extends AnyFlatSpec with Matchers:
  case class Inner(zone: String) derives Schema, Codec
  case class Query(region: String, limit: Int, dryRun: Boolean, ratio: Double, tags: List[String], inner: Inner) derives Schema, Codec

  private def json(text: String): Json = parse(text).fold(throw _, identity)

  private def rawTool(properties: String, extra: String = "") =
    tool("raw").inputJson(json(s"""{"type": "object", "properties": $properties $extra}"""))

  private given MonadError[Identity] = IdentityMonad

  private def inputSchema(tool: Tool[?, NoStructuredOutput]): Json =
    val handler = McpHandler(McpServer(tools = List(tool.handle(_ => ToolResult.text("ok")))))
    val request: JSONRPCMessage = JSONRPCMessage.Request(method = "tools/list", id = RequestId("1"))
    val tools = handler.handleJsonRpc(request.asJson, Nil).body.flatMap(_.hcursor.downField("result").downField("tools").focus)
    tools.flatMap(_.hcursor.downArray.downField("inputSchema").focus).getOrElse(fail("no input schema"))

  "withMcpHeader" should "annotate a property of a schema derived from a case class" in:
    val annotated = tool("derived").input[Query].withMcpHeader("region", "Region").withMcpHeader("limit", "Limit")
    val schema = inputSchema(annotated)
    schema.hcursor.downField("properties").downField("region").get[String]("x-mcp-header") shouldBe Right("Region")
    schema.hcursor.downField("properties").downField("limit").get[String]("x-mcp-header") shouldBe Right("Limit")
    McpHeaders.params(schema) should contain theSameElementsAs List(McpHeader(List("region"), "Region"), McpHeader(List("limit"), "Limit"))

  it should "annotate a nested property of a raw JSON schema" in:
    val annotated =
      rawTool("""{"location": {"type": "object", "properties": {"region": {"type": "string"}}}}""")
        .withMcpHeader(List("location", "region"), "Region")
    val schema = inputSchema(annotated)
    schema.hcursor
      .downField("properties")
      .downField("location")
      .downField("properties")
      .downField("region")
      .get[String](
        "x-mcp-header"
      ) shouldBe Right("Region")

  it should "keep an annotation already present in a raw JSON schema" in:
    val schema = inputSchema(rawTool("""{"region": {"type": ["string", "null"], "x-mcp-header": "Region"}}"""))
    McpHeaders.params(schema) shouldBe List(McpHeader(List("region"), "Region"))

  it should "reject a header name which breaks the spec constraints" in:
    val base = tool("derived").input[Query]
    for name <- List("", "Bad Name", "Bad:Name", "Line\nBreak", "Ctrl\u0001", "Région") do
      withClue(name):
        an[IllegalArgumentException] should be thrownBy base.withMcpHeader("region", name)

  it should "reject a header name used twice in one tool, regardless of case" in:
    an[IllegalArgumentException] should be thrownBy
      tool("derived").input[Query].withMcpHeader("region", "Region").withMcpHeader("limit", "REGION")

  it should "reject a property whose type is not string, integer or boolean" in:
    for property <- List("ratio", "tags", "inner") do
      withClue(property):
        an[IllegalArgumentException] should be thrownBy tool("derived").input[Query].withMcpHeader(property, "Header")
    tool("derived").input[Query].withMcpHeader("dryRun", "Dry-Run").mcpHeaders shouldBe List(McpHeader(List("dryRun"), "Dry-Run"))

  it should "reject a property which does not exist or is not reachable through properties keys only" in:
    an[IllegalArgumentException] should be thrownBy tool("derived").input[Query].withMcpHeader("missing", "Header")
    an[IllegalArgumentException] should be thrownBy tool("derived").input[Query].withMcpHeader(List("inner", "zone"), "Zone")
    an[IllegalArgumentException] should be thrownBy tool("derived").input[Query].withMcpHeader(Nil, "Header")

  "inputJson" should "reject an annotation which is not reachable through properties keys only" in:
    an[IllegalArgumentException] should be thrownBy
      rawTool("""{"tags": {"type": "array", "items": {"type": "string", "x-mcp-header": "Tag"}}}""")
    an[IllegalArgumentException] should be thrownBy
      rawTool("""{"a": {"type": "string"}}""", """, "anyOf": [{"properties": {"a": {"type": "string", "x-mcp-header": "A"}}}]""")
    an[IllegalArgumentException] should be thrownBy
      rawTool("""{"a": {"$ref": "#/$defs/A"}}""", """, "$defs": {"A": {"type": "string", "x-mcp-header": "A"}}""")

  it should "reject an annotation with an invalid value or on an invalid type" in:
    an[IllegalArgumentException] should be thrownBy rawTool("""{"a": {"type": "string", "x-mcp-header": 1}}""")
    an[IllegalArgumentException] should be thrownBy rawTool("""{"a": {"type": "number", "x-mcp-header": "A"}}""")
    an[IllegalArgumentException] should be thrownBy rawTool("""{"a": {"x-mcp-header": "A"}}""")
    an[IllegalArgumentException] should be thrownBy
      rawTool("""{"a": {"type": "string", "x-mcp-header": "A"}, "b": {"type": "integer", "x-mcp-header": "a"}}""")

  it should "not treat a property named x-mcp-header or a default value as an annotation" in:
    noException should be thrownBy rawTool("""{"x-mcp-header": {"type": "object", "default": {"x-mcp-header": 1}}}""")

  "McpHeaders.decode" should "decode the Base64 sentinel form and keep any other value literal" in:
    McpHeaders.decode("=?base64?SGVsbG8=?=") shouldBe Right("Hello")
    McpHeaders.decode("=?base64?SGVsbG8sIOS4lueVjA==?=") shouldBe Right("Hello, 世界")
    McpHeaders.decode("=?base64??=") shouldBe Right("")
    McpHeaders.decode("SGVsbG8=") shouldBe Right("SGVsbG8=")
    McpHeaders.decode("=?base64?SGVsbG8=") shouldBe Right("=?base64?SGVsbG8=")
    McpHeaders.decode("SGVsbG8=?=") shouldBe Right("SGVsbG8=?=")

  it should "reject a malformed sentinel or invalid characters" in:
    for value <- List("=?base64?SGVsbG8?=", "=?base64?SGVs!!!bG8=?=", "=?base64?S===?=", "=?base64?/w==?=", "café", "a\u0000b") do
      withClue(value)(McpHeaders.decode(value).isLeft shouldBe true)

  "McpHeaders.matches" should "compare numbers numerically and other values literally" in:
    McpHeaders.matches("42.0", Json.fromInt(42)) shouldBe true
    McpHeaders.matches("4.2e1", Json.fromInt(42)) shouldBe true
    McpHeaders.matches("43", Json.fromInt(42)) shouldBe false
    McpHeaders.matches("x", Json.fromInt(42)) shouldBe false
    McpHeaders.matches("true", Json.True) shouldBe true
    McpHeaders.matches("True", Json.True) shouldBe false
    McpHeaders.matches("us-west1", Json.fromString("us-west1")) shouldBe true
    McpHeaders.matches("US-WEST1", Json.fromString("us-west1")) shouldBe false
