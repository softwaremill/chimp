package chimp.server

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
    .handle(in => ToolResult.text(in.message))

  val addTool = tool("add")
    .description("Adds two numbers.")
    .input[AddInput]
    .handle(in => ToolResult.text((in.a + in.b).toString))

  val errorTool = tool("fail")
    .description("Always fails.")
    .input[EchoInput]
    .handle(_ => ToolResult.error("Intentional failure"))

  // Tool that echoes the header's value for testing
  case class HeaderEchoInput(dummy: String) derives Schema, Codec
  private val headerEchoTool = tool("headerEcho")
    .description("Echoes the header value if present.")
    .input[HeaderEchoInput]
    .handleWithHeaders { (_, headers) =>
      if headers.isEmpty then ToolResult.text("no header")
      else
        ToolResult.text(
          headers
            .map(header => s"header name: ${header.name}, header value: ${header.value}")
            .mkString(", ")
        )
    }

  val handler = McpHandler(McpServer(name = "Chimp MCP server", tools = List(echoTool, addTool, errorTool, headerEchoTool)))

  // Feature fixtures (resources, prompts, completion, logging)
  private val textResource = resource("test://text")
    .name("text")
    .mimeType("text/plain")
    .handle(() => Right(List(ResourceContents.Text(uri = "test://text", text = "hello text", mimeType = Some("text/plain")))))

  private val itemTemplate = resourceTemplate("test://item/{id}")
    .name("item")
    .handle((vars, uri) => Right(List(ResourceContents.Text(uri = uri, text = s"item ${vars("id")}"))))

  private val greetPrompt = prompt("greet")
    .description("Greets by name")
    .argument("name", required = true)
    .handle(args =>
      GetPromptResult(messages = List(PromptMessage(Role.User, ToolContent.Text(text = s"Hello ${args.getOrElse("name", "?")}"))))
    )

  private val levelRef = new java.util.concurrent.atomic.AtomicReference(Option.empty[LoggingLevel])

  private val featuresServer = McpServer[Identity](name = "Features")
    .addResource(textResource)
    .addResourceTemplate(itemTemplate)
    .addPrompt(greetPrompt)
    .withCompletion((_, _, _) => Completion(values = List("Alice", "Bob")))
    .withLogging(level => levelRef.set(Some(level)))

  private val featuresHandler = McpHandler(featuresServer)

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
        resultObj.protocolVersion shouldBe "2025-11-25"
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
        val resultObj = result.as[CallToolResult].getOrElse(fail("Failed to decode result"))
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
        val resultObj = result.as[CallToolResult].getOrElse(fail("Failed to decode result"))
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

  it should "return an error when required fields are missing (no arguments object)" in:
    // Given
    val params = Json.obj(
      "name" -> Json.fromString("add")
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
        error.message should include("Invalid arguments")
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
        val resultObj = result.as[CallToolResult].getOrElse(fail("Failed to decode result"))
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
        val resultObj = result.as[CallToolResult].getOrElse(fail("Failed to decode result"))
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
        val resultObj = result.as[CallToolResult].getOrElse(fail("Failed to decode result"))
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
        val resultObj = result.as[CallToolResult].getOrElse(fail("Failed to decode result"))
        resultObj.isError shouldBe false
        resultObj.content.head shouldBe ToolContent.Text("text", "no header")
      case _ => fail("Expected Response")

  it should "not use type arrays for optional fields in JSON schema" in:
    // Given - a tool with optional fields
    case class OptionalFieldInput(requiredField: String, optionalField: Option[Long]) derives Schema, Codec
    val optionalTool = tool("optionalTest")
      .description("Test tool with optional fields.")
      .input[OptionalFieldInput]
      .handle(_ => ToolResult.text("ok"))

    val handlerWithOptional = McpHandler(McpServer(name = "Test", tools = List(optionalTool)))

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

  private def featureResult(method: String, params: Option[Json], id: String): JSONRPCMessage =
    val req: JSONRPCMessage = Request(method = method, params = params, id = RequestId(id))
    val response = featuresHandler.handleJsonRpc(req.asJson, Seq.empty)
    extractJsonFromResponse(response).as[JSONRPCMessage].getOrElse(fail("Failed to decode response"))

  it should "list resources" in:
    featureResult("resources/list", None, "r1") match
      case Response(_, _, result) =>
        result.as[ListResourcesResult].getOrElse(fail("result")).resources.map(_.uri) shouldBe List("test://text")
      case _ => fail("Expected Response")

  it should "read a static text resource" in:
    val params = Json.obj("uri" -> Json.fromString("test://text"))
    featureResult("resources/read", Some(params), "r2") match
      case Response(_, _, result) =>
        result.as[ReadResourceResult].getOrElse(fail("result")).contents shouldBe
          List(ResourceContents.Text(uri = "test://text", text = "hello text", mimeType = Some("text/plain")))
      case _ => fail("Expected Response")

  it should "read a templated resource, substituting variables" in:
    val params = Json.obj("uri" -> Json.fromString("test://item/42"))
    featureResult("resources/read", Some(params), "r3") match
      case Response(_, _, result) =>
        val contents = result.as[ReadResourceResult].getOrElse(fail("result")).contents
        contents.head match
          case ResourceContents.Text(uri, text, _, _) =>
            uri shouldBe "test://item/42"
            text should include("42")
          case _ => fail("Expected text contents")
      case _ => fail("Expected Response")

  it should "return a -32602 error with data.uri for an unknown resource (sep-2164)" in:
    val params = Json.obj("uri" -> Json.fromString("test://missing"))
    featureResult("resources/read", Some(params), "r4") match
      case Error(_, _, error) =>
        error.code shouldBe InvalidParams.code
        error.data.flatMap(_.hcursor.downField("uri").as[String].toOption) shouldBe Some("test://missing")
      case _ => fail("Expected Error")

  it should "list prompts" in:
    featureResult("prompts/list", None, "p1") match
      case Response(_, _, result) =>
        result.as[ListPromptsResult].getOrElse(fail("result")).prompts.map(_.name) shouldBe List("greet")
      case _ => fail("Expected Response")

  it should "get a prompt, substituting arguments" in:
    val params = Json.obj("name" -> Json.fromString("greet"), "arguments" -> Json.obj("name" -> Json.fromString("World")))
    featureResult("prompts/get", Some(params), "p2") match
      case Response(_, _, result) =>
        result.as[GetPromptResult].getOrElse(fail("result")).messages.head.content match
          case ToolContent.Text(_, text) => text should include("World")
          case _                         => fail("Expected text content")
      case _ => fail("Expected Response")

  it should "return completion values" in:
    val params = Json.obj(
      "ref" -> Json.obj("type" -> Json.fromString("ref/prompt"), "name" -> Json.fromString("greet")),
      "argument" -> Json.obj("name" -> Json.fromString("name"), "value" -> Json.fromString("A"))
    )
    featureResult("completion/complete", Some(params), "c1") match
      case Response(_, _, result) =>
        result.as[CompleteResult].getOrElse(fail("result")).completion.values shouldBe List("Alice", "Bob")
      case _ => fail("Expected Response")

  it should "set the logging level and return an empty result" in:
    val params = Json.obj("level" -> Json.fromString("info"))
    featureResult("logging/setLevel", Some(params), "l1") match
      case Response(_, _, result) => result shouldBe Json.obj()
      case _                      => fail("Expected Response")
    levelRef.get() shouldBe Some(LoggingLevel.Info)

  it should "advertise capabilities derived from registered features" in:
    featureResult("initialize", None, "i1") match
      case Response(_, _, result) =>
        val caps = result.as[InitializeResult].getOrElse(fail("result")).capabilities
        caps.resources.flatMap(_.subscribe) shouldBe Some(false)
        caps.prompts.isDefined shouldBe true
        caps.completions.isDefined shouldBe true
        caps.logging.isDefined shouldBe true
        caps.tools shouldBe None
      case _ => fail("Expected Response")

  it should "return MethodNotFound for a feature method that is not configured" in:
    // featuresServer has no subscriptions handler, so resources/subscribe is not available
    val params = Json.obj("uri" -> Json.fromString("test://text"))
    featureResult("resources/subscribe", Some(params), "n1") match
      case Error(_, _, error) => error.code shouldBe MethodNotFound.code
      case _                  => fail("Expected Error")
