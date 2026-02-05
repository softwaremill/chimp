package chimp

import chimp.protocol.*
import chimp.protocol.JSONRPCMessage.given
import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.model.Header
import sttp.monad.{IdentityMonad, MonadError}
import sttp.shared.Identity
import sttp.tapir.Schema

class McpHandlerSpec extends AnyFlatSpec with Matchers:
  import JSONRPCMessage.*
  import chimp.protocol.JSONRPCErrorCodes.*

  // Simple test input types
  case class EchoInput(message: String) derives Schema, Codec
  case class AddInput(a: Int, b: Int) derives Schema, Codec

  // Test tools
  val echoTool = tool("echo")
    .description("Echoes the input message.")
    .input[EchoInput]
    .handle(in => Right(in.message))

  val addTool = tool("add")
    .description("Adds two numbers.")
    .input[AddInput]
    .handle(in => Right((in.a + in.b).toString))

  val errorTool = tool("fail")
    .description("Always fails.")
    .input[EchoInput]
    .handle(_ => Left("Intentional failure"))

  // Tool that echoes the header's value for testing
  case class HeaderEchoInput(dummy: String) derives Schema, Codec
  private val headerEchoTool = tool("headerEcho")
    .description("Echoes the header value if present.")
    .input[HeaderEchoInput]
    .handleWithHeaders { (_, headers) =>
      if headers.isEmpty then Right("no header")
      else
        Right(
          headers
            .map(header => s"header name: ${header.name}, header value: ${header.value}")
            .mkString(", ")
        )
    }

  val handler = McpHandler(List(echoTool, addTool, errorTool, headerEchoTool), "Chimp MCP server", "1.0.0", true)

  def parseJson(str: String): Json = parse(str).getOrElse(throw new RuntimeException("Invalid JSON"))

  given MonadError[Identity] = IdentityMonad

  // Helper function to extract JSON from McpResponse for testing
  private def extractJsonFromResponse(response: McpResponse): Json = response match
    case McpResponse.JsonResponse(json)  => json
    case McpResponse.EmptyAcceptResponse => fail("Expected JsonResponse but got EmptyAcceptResponse")

  "McpHandler" should "respond to initialize" in:
    // Given
    val req: JSONRPCMessage = Request(method = "initialize", id = RequestId("1"))
    val json = req.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    resp match
      case Response(_, _, result) =>
        val resultObj = result.as[InitializeResult].getOrElse(fail("Failed to decode result"))
        resultObj.protocolVersion shouldBe "2025-03-26"
        resultObj.serverInfo.name should include("Chimp MCP server")
      case _ => fail("Expected Response")

    // nulls should be dropped
    respJson.hcursor.downField("result").downField("instructions").focus shouldBe None

  it should "list available tools" in:
    // Given
    val req: JSONRPCMessage = Request(method = "tools/list", id = RequestId("2"))
    val json = req.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    resp match
      case Response(_, _, result) =>
        val resultObj = result.as[ListToolsResponse].getOrElse(fail("Failed to decode result"))
        resultObj.tools.map(_.name).toSet shouldBe Set("echo", "add", "fail", "headerEcho")
      case _ => fail("Expected Response")

  it should "call a tool successfully (echo)" in:
    // Given
    val params = Json.obj(
      "name" -> Json.fromString("echo"),
      "arguments" -> Json.obj("message" -> Json.fromString("hello"))
    )
    val req: JSONRPCMessage = Request(method = "tools/call", params = Some(params), id = RequestId("3"))
    val json = req.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    resp match
      case Response(_, _, result) =>
        val resultObj = result.as[ToolCallResult].getOrElse(fail("Failed to decode result"))
        resultObj.isError shouldBe false
        resultObj.content should have length 1
        resultObj.content.head shouldBe ToolContent.Text("text", "hello")
      case _ => fail("Expected Response")

  it should "call a tool successfully (add)" in:
    // Given
    val params = Json.obj(
      "name" -> Json.fromString("add"),
      "arguments" -> Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(3))
    )
    val req: JSONRPCMessage = Request(method = "tools/call", params = Some(params), id = RequestId("4"))
    val json = req.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    resp match
      case Response(_, _, result) =>
        val resultObj = result.as[ToolCallResult].getOrElse(fail("Failed to decode result"))
        resultObj.isError shouldBe false
        resultObj.content.head shouldBe ToolContent.Text("text", "5")
      case _ => fail("Expected Response")

  it should "accept notifications and return EmptyAcceptResponse" in:
    // Given
    val req: JSONRPCMessage = Notification(method = "notifications/initialized")
    val json = req.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq.empty)
    // Then
    // Notifications should return EmptyAcceptResponse to indicate no body should be sent
    response shouldBe McpResponse.EmptyAcceptResponse

  it should "accept different notification types and return EmptyAcceptResponse" in:
    // Given
    val req: JSONRPCMessage = Notification(method = "notifications/tools/list_changed")
    val json = req.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq.empty)
    // Then
    // All notifications should return EmptyAcceptResponse to indicate no body should be sent
    response shouldBe McpResponse.EmptyAcceptResponse

  it should "return an error for unknown tool" in:
    // Given
    val params = Json.obj(
      "name" -> Json.fromString("unknown"),
      "arguments" -> Json.obj("foo" -> Json.fromString("bar"))
    )
    val req: JSONRPCMessage = Request(method = "tools/call", params = Some(params), id = RequestId("5"))
    val json = req.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Expected error response"))
    // Then
    resp match
      case Error(_, _, error) =>
        error.code shouldBe MethodNotFound.code
        error.message should include("Unknown tool")
      case _ => fail("Expected Error")

  it should "return an error for invalid arguments" in:
    // Given
    val params = Json.obj(
      "name" -> Json.fromString("add"),
      "arguments" -> Json.obj("a" -> Json.fromString("notAnInt"), "b" -> Json.fromInt(3))
    )
    val req: JSONRPCMessage = Request(method = "tools/call", params = Some(params), id = RequestId("6"))
    val json = req.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Expected error response"))
    // Then
    resp match
      case Error(_, _, error) =>
        error.code shouldBe InvalidParams.code
        error.message should include("Invalid arguments")
      case _ => fail("Expected Error")

  it should "return an error for missing arguments" in:
    // Given
    val params = Json.obj(
      "name" -> Json.fromString("add")
      // missing 'arguments'
    )
    val req: JSONRPCMessage = Request(method = "tools/call", params = Some(params), id = RequestId("7"))
    val json = req.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Expected error response"))
    // Then
    resp match
      case Error(_, _, error) =>
        error.code shouldBe InvalidParams.code
        error.message should include("Missing arguments")
      case _ => fail("Expected Error")

  it should "return an error for missing tool name" in:
    // Given
    val params = Json.obj(
      // missing 'name'
      "arguments" -> Json.obj("message" -> Json.fromString("hello"))
    )
    val req: JSONRPCMessage = Request(method = "tools/call", params = Some(params), id = RequestId("8"))
    val json = req.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Expected error response"))
    // Then
    resp match
      case Error(_, _, error) =>
        error.code shouldBe InvalidParams.code
        error.message should include("Missing tool name")
      case _ => fail("Expected Error")

  it should "return an error for tool logic failure" in:
    // Given
    val params = Json.obj(
      "name" -> Json.fromString("fail"),
      "arguments" -> Json.obj("message" -> Json.fromString("test"))
    )
    val req: JSONRPCMessage = Request(method = "tools/call", params = Some(params), id = RequestId("9"))
    val json = req.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    resp match
      case Response(_, _, result) =>
        val resultObj = result.as[ToolCallResult].getOrElse(fail("Failed to decode result"))
        resultObj.isError shouldBe true
        resultObj.content.head shouldBe ToolContent.Text("text", "Intentional failure")
      case _ => fail("Expected Response")

  it should "return an error for unknown method" in:
    // Given
    val req: JSONRPCMessage = Request(method = "not/a/real/method", id = RequestId("10"))
    val json = req.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Expected error response"))
    // Then
    resp match
      case Error(_, _, error) =>
        error.code shouldBe MethodNotFound.code
        error.message should include("Unknown method")
      case _ => fail("Expected Error")

  it should "handle batch requests with mixed results" in:
    // Given
    val req1 = Request(
      method = "tools/call",
      params = Some(
        Json.obj(
          "name" -> Json.fromString("echo"),
          "arguments" -> Json.obj("message" -> Json.fromString("hi"))
        )
      ),
      id = RequestId("b1")
    )
    val req2 = Request(
      method = "tools/call",
      params = Some(
        Json.obj(
          "name" -> Json.fromString("add"),
          "arguments" -> Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2))
        )
      ),
      id = RequestId("b2")
    )
    val req3 = Request(
      method = "tools/call",
      params = Some(
        Json.obj(
          "name" -> Json.fromString("fail"),
          "arguments" -> Json.obj("message" -> Json.fromString("fail"))
        )
      ),
      id = RequestId("b3")
    )
    val req4 = Request(
      method = "tools/call",
      params = Some(
        Json.obj(
          "name" -> Json.fromString("unknown"),
          "arguments" -> Json.obj("foo" -> Json.fromString("bar"))
        )
      ),
      id = RequestId("b4")
    )
    val notification = Notification(method = "tools/list", params = None)
    val batch = BatchRequest(List(req1, req2, req3, req4, notification))
    val json = batch.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode batch response"))
    // Then
    resp match
      case BatchResponse(responses) =>
        // Should not include notification response
        responses.foreach {
          case Response(_, id, result) if id == RequestId("b1") =>
            val r = result.as[ToolCallResult].getOrElse(fail("Failed to decode result"))
            r.isError shouldBe false
            r.content.head shouldBe ToolContent.Text("text", "hi")
          case Response(_, id, result) if id == RequestId("b2") =>
            val r = result.as[ToolCallResult].getOrElse(fail("Failed to decode result"))
            r.isError shouldBe false
            r.content.head shouldBe ToolContent.Text("text", "3")
          case Response(_, id, result) if id == RequestId("b3") =>
            val r = result.as[ToolCallResult].getOrElse(fail("Failed to decode result"))
            r.isError shouldBe true
            r.content.head shouldBe ToolContent.Text("text", "Intentional failure")
          case Error(_, id, error) if id == RequestId("b4") =>
            error.code shouldBe MethodNotFound.code
            error.message should include("Unknown tool")
          case other => fail(s"Unexpected response: $other")
        }
        responses.exists {
          case Response(_, id, _) if id == RequestId("b1") => true
          case Response(_, id, _) if id == RequestId("b2") => true
          case Response(_, id, _) if id == RequestId("b3") => true
          case Error(_, id, _) if id == RequestId("b4")    => true
          case _                                           => false
        } shouldBe true
        responses.exists {
          case Notification(_, _, _) => true
          case _                     => false
        } shouldBe false
      case _ => fail("Expected BatchResponse")

  it should "call a tool with a header and receive the header's value in the response" in:
    // Given
    val params = Json.obj(
      "name" -> Json.fromString("headerEcho"),
      "arguments" -> Json.obj("dummy" -> Json.fromString("irrelevant"))
    )
    val req: JSONRPCMessage = Request(method = "tools/call", params = Some(params), id = RequestId("header1"))
    val json = req.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq(Header("header-name", "my-secret-header")))
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    resp match
      case Response(_, _, result) =>
        val resultObj = result.as[ToolCallResult].getOrElse(fail("Failed to decode result"))
        resultObj.isError shouldBe false
        resultObj.content.head shouldBe ToolContent.Text("text", "header name: header-name, header value: my-secret-header")
      case _ => fail("Expected Response")

  it should "call a tool with a header and receive multiple header's values in the response" in:
    // Given
    val params = Json.obj(
      "name" -> Json.fromString("headerEcho"),
      "arguments" -> Json.obj("dummy" -> Json.fromString("irrelevant"))
    )
    val req: JSONRPCMessage = Request(method = "tools/call", params = Some(params), id = RequestId("header1"))
    val json = req.asJson
    // When
    val response =
      handler.handleJsonRpc(json, Seq(Header("header-name", "my-secret-header"), Header("another-header-name", "another-secret-header")))
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    resp match
      case Response(_, _, result) =>
        val resultObj = result.as[ToolCallResult].getOrElse(fail("Failed to decode result"))
        resultObj.isError shouldBe false
        resultObj.content.head shouldBe ToolContent.Text(
          "text",
          "header name: header-name, header value: my-secret-header, header name: another-header-name, header value: another-secret-header"
        )
      case _ => fail("Expected Response")

  it should "call a tool without a header value and receive 'no header' in the response" in:
    // Given
    val params = Json.obj(
      "name" -> Json.fromString("headerEcho"),
      "arguments" -> Json.obj("dummy" -> Json.fromString("irrelevant"))
    )
    val req: JSONRPCMessage = Request(method = "tools/call", params = Some(params), id = RequestId("header2"))
    val json = req.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    resp match
      case Response(_, _, result) =>
        val resultObj = result.as[ToolCallResult].getOrElse(fail("Failed to decode result"))
        resultObj.isError shouldBe false
        resultObj.content.head shouldBe ToolContent.Text("text", "no header")
      case _ => fail("Expected Response")

  it should "not use type arrays for optional fields in JSON schema" in:
    // Given - a tool with optional fields
    case class OptionalFieldInput(requiredField: String, optionalField: Option[Long]) derives Schema, Codec
    val optionalTool = tool("optionalTest")
      .description("Test tool with optional fields.")
      .input[OptionalFieldInput]
      .handle(_ => Right("ok"))

    val handlerWithOptional = McpHandler(List(optionalTool), "Test", "1.0.0", true)

    val req: JSONRPCMessage = Request(method = "tools/list", id = RequestId("opt1"))
    val json = req.asJson
    // When
    val response = handlerWithOptional.handleJsonRpc(json, Seq.empty)
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))
    // Then
    resp match
      case Response(_, _, result) =>
        val resultObj = result.as[ListToolsResponse].getOrElse(fail("Failed to decode result"))
        val toolDef = resultObj.tools.find(_.name == "optionalTest").get
        val inputSchema = toolDef.inputSchema

        // Check that optionalField does NOT use ["integer", "null"] type array
        // Claude API rejects this format - it should just be "integer" with the field not in required
        val optionalFieldType = inputSchema.hcursor
          .downField("properties")
          .downField("optionalField")
          .downField("type")
          .focus

        optionalFieldType match
          case Some(typeValue) =>
            // Should be a simple string "integer", not an array ["integer", "null"]
            typeValue.isString shouldBe true
            typeValue.asString.get shouldBe "integer"
          case None =>
            fail("optionalField type not found in schema")

        // Verify requiredField is in required array but optionalField is not
        val requiredFields = inputSchema.hcursor.downField("required").as[List[String]].getOrElse(Nil)
        requiredFields should contain("requiredField")
        requiredFields should not contain "optionalField"
      case _ => fail("Expected Response")

  it should "handle batch requests with mixed headers" in:
    // Given
    val req1 = Request(
      method = "tools/call",
      params = Some(
        Json.obj(
          "name" -> Json.fromString("headerEcho"),
          "arguments" -> Json.obj("dummy" -> Json.fromString("hi"))
        )
      ),
      id = RequestId("bh1")
    )
    val req2 = Request(
      method = "tools/call",
      params = Some(
        Json.obj(
          "name" -> Json.fromString("headerEcho"),
          "arguments" -> Json.obj("dummy" -> Json.fromString("yo"))
        )
      ),
      id = RequestId("bh2")
    )
    val batch = BatchRequest(List(req1, req2))
    val json = batch.asJson
    // When
    val response = handler.handleJsonRpc(json, Seq(Header("header-name", "batch-header")))
    val respJson = extractJsonFromResponse(response)
    val resp = respJson.as[JSONRPCMessage].getOrElse(fail("Failed to decode batch response"))
    // Then
    resp match
      case BatchResponse(responses) =>
        responses.foreach {
          case Response(_, id, result) if id == RequestId("bh1") || id == RequestId("bh2") =>
            val r = result.as[ToolCallResult].getOrElse(fail("Failed to decode result"))
            r.isError shouldBe false
            r.content.head shouldBe ToolContent.Text("text", "header name: header-name, header value: batch-header")
          case other => fail(s"Unexpected response: $other")
        }
      case _ => fail("Expected BatchResponse")
