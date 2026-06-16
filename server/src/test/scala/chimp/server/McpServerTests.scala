package chimp.server

import chimp.client.McpClient
import chimp.protocol.*
import io.circe.{Codec, Json}
import org.scalatest.Assertion
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.monad.syntax.*
import sttp.tapir.Schema

import scala.concurrent.Future

/** Backend-agnostic tests exercising an MCP server over a real transport, driven by the chimp client. A concrete spec provides [[withServer]]
  * (host the server on some Tapir backend, connect a client) and a [[ToFuture]] for the effect. These cover the request/response surface and
  * run against any backend.
  */
trait McpServerTests[F[_]] extends AsyncFlatSpec with Matchers:
  this: ToFuture[F] =>

  protected def withServer(server: McpServer[F])(test: McpClient[F] => F[Assertion]): Future[Assertion]

  private case class EchoInput(message: String) derives Codec, Schema

  protected def sampleServer: McpServer[F] =
    McpServer[F]()
      .addTool(
        tool("echo")
          .description("Echoes a message")
          .input[EchoInput]
          .serverLogic[F]((in, _, _) => monad.unit(ToolResult.text(in.message)))
      )
      .addResource(
        resource("test://greeting")
          .mimeType("text/plain")
          .read[F](() => monad.unit(Right(List(ResourceContents.Text(uri = "test://greeting", text = "hello", mimeType = Some("text/plain"))))))
      )
      .addPrompt(
        prompt("greet")
          .argument("name", required = true)
          .get[F](args => monad.unit(GetPromptResult(messages = List(PromptMessage(Role.User, ToolContent.Text(text = s"Hi ${args.getOrElse("name", "?")}"))))))
      )
      .withCompletion((_, _, _) => monad.unit(Completion(values = List("alpha", "beta"))))

  "an MCP server" should "list its tools" in withServer(sampleServer): client =>
    client.listTools().map(_.tools.map(_.name) should contain("echo"))

  it should "advertise capabilities for the registered features" in withServer(sampleServer): client =>
    monad.unit:
      client.serverCapabilities.tools shouldBe defined
      client.serverCapabilities.resources shouldBe defined
      client.serverCapabilities.prompts shouldBe defined
      client.serverCapabilities.completions shouldBe defined

  it should "execute a tool call" in withServer(sampleServer): client =>
    client.callTool("echo", Json.obj("message" -> Json.fromString("hi"))).map: result =>
      result.isError shouldBe false
      result.content shouldBe List(ToolContent.Text("text", "hi"))

  it should "read a resource" in withServer(sampleServer): client =>
    client.readResource("test://greeting").map: result =>
      result.contents.head match
        case ResourceContents.Text(_, text, _, _) => text shouldBe "hello"
        case other                                => fail(s"expected text contents, got $other")

  it should "get a prompt with arguments" in withServer(sampleServer): client =>
    client.getPrompt("greet", Map("name" -> "World")).map: result =>
      result.messages.head.content match
        case ToolContent.Text(_, text) => text should include("World")
        case other                     => fail(s"expected text content, got $other")

  it should "return completion suggestions" in withServer(sampleServer): client =>
    client.complete(CompleteRef.Prompt(PromptReference(name = "greet")), CompleteArgument("name", "W")).map: result =>
      result.completion.values shouldBe List("alpha", "beta")
