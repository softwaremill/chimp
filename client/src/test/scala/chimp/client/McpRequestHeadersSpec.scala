package chimp.client

import chimp.client.transport.McpRequestHeaders
import chimp.client.transport.McpRequestHeaders.Param
import io.circe.{parser, Json}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class McpRequestHeadersSpec extends AnyFlatSpec with Matchers:

  private def schema(properties: String): Json =
    parser.parse(s"""{"type":"object","properties":{$properties}}""").fold(throw _, identity)

  "encode" should "keep printable ASCII as it is, internal spaces included" in:
    McpRequestHeaders.encode("us west 1") shouldBe "us west 1"
    McpRequestHeaders.encode("") shouldBe ""

  it should "use the Base64 sentinel form for non-ASCII, control characters, leading or trailing whitespace and sentinel-like values" in:
    McpRequestHeaders.encode("Hello, 世界") shouldBe "=?base64?SGVsbG8sIOS4lueVjA==?="
    McpRequestHeaders.encode(" padded ") shouldBe "=?base64?IHBhZGRlZCA=?="
    McpRequestHeaders.encode("us-west1 ") shouldBe "=?base64?dXMtd2VzdDEg?="
    McpRequestHeaders.encode("line1\nline2") shouldBe "=?base64?bGluZTEKbGluZTI=?="
    McpRequestHeaders.encode("\tindented") shouldBe "=?base64?CWluZGVudGVk?="
    McpRequestHeaders.encode("=?base64?abc?=") shouldBe "=?base64?PT9iYXNlNjQ/YWJjPz0=?="

  "argumentValue" should "write booleans and integers as text, and give no value for null, arrays and objects" in:
    McpRequestHeaders.argumentValue(Json.True) shouldBe Some("true")
    McpRequestHeaders.argumentValue(Json.False) shouldBe Some("false")
    McpRequestHeaders.argumentValue(Json.fromInt(42)) shouldBe Some("42")
    McpRequestHeaders.argumentValue(Json.fromString("é")) shouldBe Some("=?base64?w6k=?=")
    McpRequestHeaders.argumentValue(Json.Null) shouldBe None
    McpRequestHeaders.argumentValue(Json.arr()) shouldBe None
    McpRequestHeaders.argumentValue(Json.obj()) shouldBe None

  "params" should "find the annotated properties, nested ones through properties keys included" in:
    val nested = schema(
      """"region":{"type":"string","x-mcp-header":"Region"},
        |"limits":{"type":"object","properties":{"max":{"type":["integer","null"],"x-mcp-header":"Max"}}},
        |"query":{"type":"string"}""".stripMargin
    )
    McpRequestHeaders.params(nested) shouldBe Right(List(Param(List("region"), "Region"), Param(List("limits", "max"), "Max")))

  it should "reject empty, non-string, duplicate and non-token header names" in:
    val invalid = List(
      """"v":{"type":"string","x-mcp-header":""}""",
      """"v":{"type":"string","x-mcp-header":7}""",
      """"a":{"type":"string","x-mcp-header":"MyField"},"b":{"type":"string","x-mcp-header":"myfield"}""",
      """"v":{"type":"string","x-mcp-header":"My Region"}""",
      """"v":{"type":"string","x-mcp-header":"Region:Primary"}""",
      """"v":{"type":"string","x-mcp-header":"Région"}""",
      """"v":{"type":"string","x-mcp-header":"Region\t1"}"""
    )
    all(invalid.map(properties => McpRequestHeaders.params(schema(properties)).isLeft)) shouldBe true

  it should "reject annotations on non-primitive or number properties, and off a plain properties chain" in:
    val invalid = List(
      """"v":{"type":"object","x-mcp-header":"Data"}""",
      """"v":{"type":"array","items":{"type":"string"},"x-mcp-header":"Items"}""",
      """"v":{"type":"null","x-mcp-header":"Nil"}""",
      """"v":{"type":"number","x-mcp-header":"Ratio"}""",
      """"v":{"type":"array","items":{"type":"string","x-mcp-header":"Item"}}""",
      """"v":{"anyOf":[{"type":"string","x-mcp-header":"Choice"}]}"""
    )
    all(invalid.map(properties => McpRequestHeaders.params(schema(properties)).isLeft)) shouldBe true
