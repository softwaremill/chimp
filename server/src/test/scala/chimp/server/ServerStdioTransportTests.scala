package chimp.server

import chimp.protocol.{JSONRPCErrorCodes, LoggingLevel}
import io.circe.{parser, Codec, Json}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.monad.syntax.*
import sttp.tapir.Schema

import java.io.{
  BufferedReader,
  BufferedWriter,
  InputStream,
  InputStreamReader,
  OutputStream,
  OutputStreamWriter,
  PipedInputStream,
  PipedOutputStream
}
import java.nio.charset.StandardCharsets

trait ServerStdioTransportTests[F[_]] extends AnyFlatSpec with Matchers:
  this: ToFuture[F] =>

  protected def runStdioServer(server: StreamingMcpServer[F], in: InputStream, out: OutputStream): Unit

  private case class EchoInput(message: String) derives Codec, Schema
  private case class NoInput() derives Codec, Schema

  private def server: StreamingMcpServer[F] =
    StreamingMcpServer[F]()
      .withLoggingLevel(_ => monad.unit(()))
      .addTool(tool("echo").input[EchoInput].serverLogic[F]((in, _) => monad.unit(ToolResult.text(in.message))))
      .addStreamingTool(
        tool("noisy")
          .input[NoInput]
          .streamingServerLogic[F] { (_, ctx, _) =>
            for
              _ <- ctx.log(LoggingLevel.Info, Json.fromString("one"))
              _ <- ctx.log(LoggingLevel.Info, Json.fromString("two"))
              _ <- ctx.log(LoggingLevel.Info, Json.fromString("three"))
            yield ToolResult.text("done")
          }
      )

  private def withStdioServer[A](body: (String => Unit, () => Json) => A): A =
    val toServer = PipedOutputStream()
    val serverIn = PipedInputStream(toServer)
    val fromServer = PipedInputStream()
    val serverOut = PipedOutputStream(fromServer)

    runStdioServer(server, serverIn, serverOut)

    val writer = BufferedWriter(OutputStreamWriter(toServer, StandardCharsets.UTF_8))
    val reader = BufferedReader(InputStreamReader(fromServer, StandardCharsets.UTF_8))

    def send(line: String): Unit =
      writer.write(line)
      writer.newLine()
      writer.flush()

    def readResponse(): Json = parser.parse(reader.readLine()).toOption.get

    try body(send, readResponse)
    finally writer.close()

  "a stdio server" should "answer requests and stream notifications over stdin/stdout" in withStdioServer { (send, readResponse) =>
    send(
      """{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"t","version":"1"}}}"""
    )
    val init = readResponse()
    init.hcursor.downField("id").as[Int] shouldBe Right(1)
    init.hcursor.downField("result").downField("serverInfo").downField("name").as[String].isRight shouldBe true

    send("""{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"echo","arguments":{"message":"hi"}}}""")
    val echo = readResponse()
    echo.hcursor.downField("result").downField("content").downN(0).downField("text").as[String] shouldBe Right("hi")

    send("""{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"noisy","arguments":{}}}""")
    val notifications = List(readResponse(), readResponse(), readResponse())
    notifications.map(_.hcursor.downField("method").as[String]) shouldBe List.fill(3)(Right("notifications/message"))
    notifications.flatMap(_.hcursor.downField("params").downField("data").as[String].toOption) shouldBe List("one", "two", "three")

    val response = readResponse()
    response.hcursor.downField("id").as[Int] shouldBe Right(3)
    response.hcursor.downField("result").downField("content").downN(0).downField("text").as[String] shouldBe Right("done")
  }

  it should "skip notifications and malformed lines, and still report protocol errors" in withStdioServer { (send, readResponse) =>
    send("""{"jsonrpc":"2.0","method":"notifications/initialized"}""")
    send("this is not valid json")
    send("""{"jsonrpc":"2.0","id":9,"method":"tools/call","params":{"name":"missing","arguments":{}}}""")

    val error = readResponse()
    error.hcursor.downField("id").as[Int] shouldBe Right(9)
    error.hcursor.downField("error").downField("code").as[Int] shouldBe Right(JSONRPCErrorCodes.MethodNotFound.code)
    error.hcursor.downField("error").downField("message").as[String].toOption.get should include("missing")
  }
