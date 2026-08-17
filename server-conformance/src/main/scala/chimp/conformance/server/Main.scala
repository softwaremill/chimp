package chimp.conformance.server

import chimp.protocol.*
import chimp.server.*
import chimp.server.ox.OxServerHttpTransport
import io.circe.parser.parse
import io.circe.{Codec, Json}
import _root_.ox.{sleep, supervised}
import sttp.shared.Identity
import sttp.tapir.Schema
import sttp.tapir.server.netty.sync.NettySyncServer

import java.util.Base64
import scala.concurrent.duration.*

object Main:

  private case class AddNumbersInput(a: Double, b: Double) derives Codec, Schema
  private case class NoInput() derives Codec, Schema

  private def base64Resource(path: String): String =
    val stream = getClass.getResourceAsStream(path)
    require(stream != null, s"conformance fixture resource not found on the classpath: $path")
    try Base64.getEncoder.encodeToString(stream.readAllBytes())
    finally stream.close()

  private val pngData = base64Resource("/sample.png")
  private val wavData = base64Resource("/sample.wav")

  private val addNumbers = tool("add_numbers")
    .description("Adds two numbers and returns the result as text")
    .input[AddNumbersInput]
    .handle(in => ToolResult.text((in.a + in.b).toString))

  private val simpleText = tool("test_simple_text")
    .description("Returns a fixed text string")
    .input[NoInput]
    .handle(_ => ToolResult.text("This is a simple text response for testing"))

  private val errorTool = tool("test_error_handling")
    .description("Always returns an error result")
    .input[NoInput]
    .handle(_ => ToolResult.error("This tool intentionally returns an error for testing"))

  private val imageTool = tool("test_image_content")
    .description("Returns image content")
    .input[NoInput]
    .handle(_ => ToolResult.image(pngData, "image/png"))

  private val audioTool = tool("test_audio_content")
    .description("Returns audio content")
    .input[NoInput]
    .handle(_ => ToolResult.audio(wavData, "audio/wav"))

  private val mixedTool = tool("test_multiple_content_types")
    .description("Returns text, image and embedded-resource content")
    .input[NoInput]
    .handle(_ =>
      ToolResult.content(
        ToolContent.Text(text = "Here is some mixed content"),
        ToolContent.Image(data = pngData, mimeType = "image/png"),
        ToolContent
          .ResourceContent(resource = ResourceContents.Text(uri = "test://embedded", text = "embedded", mimeType = Some("text/plain")))
      )
    )

  private val embeddedResourceTool = tool("test_embedded_resource")
    .description("Returns an embedded resource")
    .input[NoInput]
    .handle(_ =>
      ToolResult.embedded(ResourceContents.Text(uri = "test://embedded", text = "embedded resource content", mimeType = Some("text/plain")))
    )

  private val jsonSchema2020: Json = parse(
    """{
      |  "$schema": "https://json-schema.org/draft/2020-12/schema",
      |  "type": "object",
      |  "$defs": {
      |    "address": {
      |      "$anchor": "addressDef",
      |      "type": "object",
      |      "properties": {
      |        "street": { "type": "string" },
      |        "city": { "type": "string" }
      |      }
      |    }
      |  },
      |  "properties": {
      |    "name": { "type": "string" },
      |    "address": { "$ref": "#/$defs/address" },
      |    "contactMethod": { "type": "string", "enum": ["phone", "email"] },
      |    "phone": { "type": "string" },
      |    "email": { "type": "string" }
      |  },
      |  "allOf": [
      |    { "anyOf": [{ "required": ["phone"] }, { "required": ["email"] }] }
      |  ],
      |  "if": {
      |    "properties": { "contactMethod": { "const": "phone" } },
      |    "required": ["contactMethod"]
      |  },
      |  "then": { "required": ["phone"] },
      |  "else": { "required": ["email"] },
      |  "additionalProperties": false
      |}""".stripMargin
  ).toOption.get

  private val jsonSchemaTool = tool("json_schema_2020_12_tool")
    .description("Advertises a JSON Schema 2020-12 input schema")
    .inputJson(jsonSchema2020)
    .handle(_ => ToolResult.text("ok"))

  private val progressTool = tool("test_tool_with_progress")
    .description("Reports progress notifications while running")
    .input[NoInput]
    .streamingServerLogic[Identity]: (_, ctx, _) =>
      ctx.reportProgress(0.0, Some(100.0))
      sleep(50.millis)
      ctx.reportProgress(50.0, Some(100.0))
      sleep(50.millis)
      ctx.reportProgress(100.0, Some(100.0))
      ToolResult.text("progress reporting complete")

  private val loggingTool = tool("test_tool_with_logging")
    .description("Emits log notifications while running")
    .input[NoInput]
    .streamingServerLogic[Identity]: (_, ctx, _) =>
      ctx.log(LoggingLevel.Info, Json.fromString("Tool execution started"))
      sleep(50.millis)
      ctx.log(LoggingLevel.Info, Json.fromString("Tool processing data"))
      sleep(50.millis)
      ctx.log(LoggingLevel.Info, Json.fromString("Tool execution completed"))
      ToolResult.text("logging complete")

  private val staticText = resource("test://static-text")
    .name("static-text")
    .mimeType("text/plain")
    .handle(() =>
      Right(List(ResourceContents.Text(uri = "test://static-text", text = "Hello, text resource!", mimeType = Some("text/plain"))))
    )

  private val staticBinary = resource("test://static-binary")
    .name("static-binary")
    .mimeType("image/png")
    .handle(() => Right(List(ResourceContents.Blob(uri = "test://static-binary", blob = pngData, mimeType = Some("image/png")))))

  private val dataTemplate = resourceTemplate("test://template/{id}/data")
    .name("data-template")
    .mimeType("text/plain")
    .handle((vars, uri) =>
      Right(List(ResourceContents.Text(uri = uri, text = s"data for ${vars.getOrElse("id", "?")}", mimeType = Some("text/plain"))))
    )

  private val simplePrompt = prompt("test_simple_prompt")
    .description("A simple prompt")
    .handle(_ => GetPromptResult(messages = List(PromptMessage(Role.User, ToolContent.Text(text = "This is a simple prompt.")))))

  private val argsPrompt = prompt("test_prompt_with_arguments")
    .description("A prompt with arguments")
    .argument("arg1", required = true)
    .argument("arg2", required = true)
    .handle: args =>
      val text = s"arg1=${args.getOrElse("arg1", "")}, arg2=${args.getOrElse("arg2", "")}"
      GetPromptResult(messages = List(PromptMessage(Role.User, ToolContent.Text(text = text))))

  private val embeddedResourcePrompt = prompt("test_prompt_with_embedded_resource")
    .description("A prompt embedding a resource")
    .argument("resourceUri", required = true)
    .handle: args =>
      val uri = args.getOrElse("resourceUri", "test://example-resource")
      GetPromptResult(messages =
        List(
          PromptMessage(
            Role.User,
            ToolContent.ResourceContent(resource =
              ResourceContents.Text(uri = uri, text = "embedded resource content", mimeType = Some("text/plain"))
            )
          )
        )
      )

  private val imagePrompt = prompt("test_prompt_with_image")
    .description("A prompt with an image")
    .handle(_ => GetPromptResult(messages = List(PromptMessage(Role.User, ToolContent.Image(data = pngData, mimeType = "image/png")))))

  private val server = McpServer[Identity](name = "chimp-conformance-server", version = "0.1.0")
    .addTools(addNumbers, simpleText, errorTool, imageTool, audioTool, mixedTool, embeddedResourceTool, jsonSchemaTool)
    .addResources(staticText, staticBinary)
    .addResourceTemplate(dataTemplate)
    .addPrompts(simplePrompt, argsPrompt, embeddedResourcePrompt, imagePrompt)
    .withCompletion((_, _, _) => Completion(values = List("alpha", "beta")))
    .withLoggingLevel(_ => ())
    .withSubscriptions(ResourceSubscriptions[Identity](_ => (), _ => ()))

  private val streamingServer = server.streaming.addStreamingTools(progressTool, loggingTool)

  def main(args: Array[String]): Unit =
    val requestedPort = args
      .collectFirst { case s"--port=$p" => p.toInt }
      .orElse(sys.env.get("CHIMP_CONFORMANCE_PORT").map(_.toInt))
      .getOrElse(0)

    val endpoint = OxServerHttpTransport(List("mcp")).serve(streamingServer)

    supervised:
      val binding = NettySyncServer().port(requestedPort).addEndpoint(endpoint).start()
      println(s"http://127.0.0.1:${binding.port}/mcp")
      System.out.flush()
      Thread.currentThread.join()
