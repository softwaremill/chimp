package chimp.protocol

import com.networknt.schema.{InputFormat, SchemaRegistry, SpecificationVersion}
import io.circe.Encoder
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters.*

class SchemaConformanceSpec extends AnyFlatSpec with Matchers:

  private val registry = SchemaRegistry.withDefaultDialect(SpecificationVersion.DRAFT_2020_12)

  private val rootSchemaText: String =
    val stream = getClass.getResourceAsStream("/schema/2025-11-25/schema.json")
    assert(stream != null, "MCP schema not found on the classpath at /schema/2025-11-25/schema.json")
    val text = String(stream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
    stream.close()
    text

  private val defsText: String =
    val rootJson = io.circe.parser.parse(rootSchemaText).getOrElse(
      throw RuntimeException("Could not parse the bundled MCP schema as JSON")
    )
    rootJson.hcursor.downField("$defs").focus
      .getOrElse(throw RuntimeException("Schema root is missing $defs object"))
      .noSpaces

  private def validate[T: Encoder](defName: String, value: T): Unit =
    val encoded = value.asJson.deepDropNullValues.noSpaces
    val wrapper =
      s"""{"$$schema":"https://json-schema.org/draft/2020-12/schema","$$ref":"#/$$defs/$defName","$$defs":$defsText}"""
    val schema = registry.getSchema(wrapper, InputFormat.JSON)
    val errors = schema.validate(encoded, InputFormat.JSON).asScala.toList
    withClue(s"Encoded JSON ($defName):\n$encoded\nViolations:\n${errors.mkString("\n")}\n"):
      errors shouldBe empty

  // --- Lifecycle ---

  it should "produce InitializeRequestParams that match the spec schema" in:
    validate("InitializeRequestParams", InitializeParams(
      protocolVersion = ProtocolVersion.Latest,
      capabilities = ClientCapabilities(),
      clientInfo = Implementation(name = "chimp-test", version = "0.0.1")
    ))

  it should "produce InitializeResult that match the spec schema" in:
    validate("InitializeResult", InitializeResult(
      protocolVersion = ProtocolVersion.Latest,
      capabilities = ServerCapabilities(tools = Some(ServerToolsCapability(listChanged = Some(false)))),
      serverInfo = Implementation(name = "chimp-test", version = "0.0.1"),
      instructions = Some("welcome")
    ))

  it should "produce Implementation that match the spec schema" in:
    validate("Implementation", Implementation(name = "chimp", version = "1.0", title = Some("Chimp")))

  it should "produce ClientCapabilities (full) that match the spec schema" in:
    validate("ClientCapabilities", ClientCapabilities(
      roots = Some(ClientRootsCapability(listChanged = Some(true))),
      sampling = Some(io.circe.Json.obj()),
      elicitation = Some(io.circe.Json.obj())
    ))

  it should "produce ServerCapabilities (full) that match the spec schema" in:
    validate("ServerCapabilities", ServerCapabilities(
      tools     = Some(ServerToolsCapability(listChanged = Some(true))),
      resources = Some(ServerResourcesCapability(subscribe = Some(true), listChanged = Some(false))),
      prompts   = Some(ServerPromptsCapability(listChanged = Some(true))),
      logging   = Some(io.circe.Json.obj()),
      completions = Some(io.circe.Json.obj())
    ))

  // --- Tools ---

  it should "produce a Tool definition that matches the spec schema" in:
    validate("Tool", ToolDefinition(
      name = "add_numbers",
      title = Some("Add numbers"),
      description = Some("Adds two numbers"),
      inputSchema = io.circe.Json.obj("type" -> "object".asJson),
      annotations = Some(ToolAnnotations(title = Some("Add"), idempotentHint = Some(true)))
    ))

  it should "produce CallToolRequestParams that match the spec schema" in:
    validate("CallToolRequestParams", CallToolParams(
      name = "add_numbers",
      arguments = io.circe.Json.obj("a" -> 1.asJson, "b" -> 2.asJson)
    ))

  it should "produce CallToolResult (text) that match the spec schema" in:
    validate("CallToolResult", CallToolResult(content = List(ToolContent.Text(text = "hello"))))

  it should "produce CallToolResult (error) that match the spec schema" in:
    validate("CallToolResult", CallToolResult(content = List(ToolContent.Text(text = "boom")), isError = true))

  it should "produce ListToolsResult that match the spec schema" in:
    val tool = ToolDefinition(name = "t", inputSchema = io.circe.Json.obj("type" -> "object".asJson))
    validate("ListToolsResult", ListToolsResponse(tools = List(tool), nextCursor = Some("c")))

  // --- ContentBlock variants ---

  it should "produce TextContent that match the spec schema" in:
    validate("TextContent", ToolContent.Text(text = "hi"))

  it should "produce ImageContent that match the spec schema" in:
    validate("ImageContent", ToolContent.Image(data = "AAAA", mimeType = "image/png"))

  it should "produce AudioContent that match the spec schema" in:
    validate("AudioContent", ToolContent.Audio(data = "AAAA", mimeType = "audio/wav"))

  // --- Resources ---

  it should "produce Resource that match the spec schema" in:
    validate("Resource", Resource(
      uri = "file:///x",
      name = "x",
      title = Some("X"),
      mimeType = Some("text/plain")
    ))

  it should "produce TextResourceContents that match the spec schema" in:
    validate("TextResourceContents", ResourceContents.Text(uri = "file:///x", text = "body", mimeType = Some("text/plain")))

  it should "produce BlobResourceContents that match the spec schema" in:
    validate("BlobResourceContents", ResourceContents.Blob(uri = "file:///x", blob = "AAAA", mimeType = Some("application/octet-stream")))

  it should "produce ListResourcesResult that match the spec schema" in:
    validate("ListResourcesResult", ListResourcesResult(resources = List(Resource(uri = "file:///x", name = "x"))))

  it should "produce ReadResourceResult that match the spec schema" in:
    validate("ReadResourceResult", ReadResourceResult(contents = List(ResourceContents.Text(uri = "file:///x", text = "y"))))

  it should "produce ResourceTemplate that match the spec schema" in:
    validate("ResourceTemplate", ResourceTemplate(uriTemplate = "file:///{path}", name = "tpl"))

  // --- Prompts ---

  it should "produce Prompt that match the spec schema" in:
    validate("Prompt", Prompt(
      name = "greet",
      title = Some("Greet"),
      description = Some("Greet the user"),
      arguments = Some(List(PromptArgument(name = "who", required = Some(true))))
    ))

  it should "produce PromptMessage that match the spec schema" in:
    validate("PromptMessage", PromptMessage(role = Role.User, content = ToolContent.Text(text = "hi")))

  it should "produce ListPromptsResult that match the spec schema" in:
    validate("ListPromptsResult", ListPromptsResult(prompts = List(Prompt(name = "p"))))

  it should "produce GetPromptResult that match the spec schema" in:
    validate("GetPromptResult", GetPromptResult(messages = List(PromptMessage(Role.Assistant, ToolContent.Text(text = "yo")))))

  // --- Sampling ---

  it should "produce CreateMessageRequestParams that match the spec schema" in:
    validate("CreateMessageRequestParams", CreateMessageParams(
      messages = List(SamplingMessage(role = Role.User, content = ToolContent.Text(text = "say hi"))),
      maxTokens = 100,
      systemPrompt = Some("you are helpful"),
      includeContext = Some(IncludeContext.None),
      temperature = Some(0.5),
      stopSequences = Some(List("\n\n"))
    ))

  it should "produce CreateMessageResult that match the spec schema" in:
    validate("CreateMessageResult", CreateMessageResult(
      role = Role.Assistant,
      content = ToolContent.Text(text = "hi"),
      model = "claude-test",
      stopReason = Some("endTurn")
    ))

  // --- Elicitation ---

  it should "produce ElicitRequestParams (form variant) that match the spec schema" in:
    validate("ElicitRequestParams", ElicitParams(
      message = "what is your name?",
      requestedSchema = io.circe.Json.obj(
        "type" -> "object".asJson,
        "properties" -> io.circe.Json.obj("name" -> io.circe.Json.obj("type" -> "string".asJson))
      )
    ))

  it should "produce ElicitResult that match the spec schema" in:
    validate("ElicitResult", ElicitResult(
      action = ElicitAction.Accept,
      content = Some(Map("name" -> "Alice".asJson))
    ))

  // --- Roots ---

  it should "produce Root that match the spec schema" in:
    validate("Root", Root(uri = "file:///workspace", name = Some("workspace")))

  it should "produce ListRootsResult that match the spec schema" in:
    validate("ListRootsResult", ListRootsResult(roots = List(Root(uri = "file:///workspace"))))

  // --- Completion ---

  it should "produce CompleteRequestParams that match the spec schema" in:
    validate("CompleteRequestParams", CompleteParams(
      ref = CompleteRef.Prompt(PromptReference(name = "greet")),
      argument = CompleteArgument(name = "who", value = "al")
    ))

  it should "produce CompleteResult that match the spec schema" in:
    validate("CompleteResult", CompleteResult(completion = Completion(values = List("Alice", "Alex"), total = Some(2), hasMore = Some(false))))

  // --- Logging ---

  it should "produce SetLevelRequestParams that match the spec schema" in:
    validate("SetLevelRequestParams", SetLevelParams(level = LoggingLevel.Info))

  it should "produce LoggingMessageNotificationParams that match the spec schema" in:
    validate("LoggingMessageNotificationParams", LoggingMessageParams(
      level = LoggingLevel.Warning,
      data  = io.circe.Json.fromString("something happened"),
      logger = Some("chimp")
    ))

  // --- Progress / Cancellation ---

  it should "produce ProgressNotificationParams that match the spec schema" in:
    validate("ProgressNotificationParams", ProgressParams(
      progressToken = ProgressToken("t1"),
      progress = 0.5,
      total = Some(1.0),
      message = Some("halfway")
    ))

  it should "produce CancelledNotificationParams that match the spec schema" in:
    validate("CancelledNotificationParams", CancelledParams(requestId = RequestId("r1"), reason = Some("user cancelled")))

  // --- JSON-RPC envelope ---

  it should "produce a JSON-RPC Request envelope that match the spec schema" in:
    val msg: JSONRPCMessage =
      JSONRPCMessage.Request(method = "tools/list", params = Some(io.circe.Json.obj()), id = RequestId(1))
    validate("JSONRPCRequest", msg)

  it should "produce a JSON-RPC Notification envelope that match the spec schema" in:
    val msg: JSONRPCMessage =
      JSONRPCMessage.Notification(method = "notifications/initialized", params = Some(io.circe.Json.obj()))
    validate("JSONRPCNotification", msg)

  it should "produce a JSON-RPC Response envelope that match the spec schema" in:
    val msg: JSONRPCMessage =
      JSONRPCMessage.Response(id = RequestId(1), result = io.circe.Json.obj())
    validate("JSONRPCResultResponse", msg)

  it should "produce a JSON-RPC Error envelope that match the spec schema" in:
    val msg: JSONRPCMessage =
      JSONRPCMessage.Error(id = RequestId(1), error = JSONRPCErrorObject(code = -32601, message = "not found"))
    validate("JSONRPCErrorResponse", msg)
