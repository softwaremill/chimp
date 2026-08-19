package chimp.server

import chimp.protocol.{JSONRPCErrorCodes, LoggingLevel}
import chimp.transport.{McpLineTooLongException, StdioFraming}
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
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

trait ServerStdioTransportTests[F[_]] extends AnyFlatSpec with Matchers:
  this: ToFuture[F] =>

  protected def runStdioServer(server: StreamingMcpServer[F], in: InputStream, out: OutputStream, maxLineLength: Int): Future[Unit]

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

  protected class StdioSession(toServer: PipedOutputStream, fromServer: PipedInputStream, val served: Future[Unit]):
    private val writer = BufferedWriter(OutputStreamWriter(toServer, StandardCharsets.UTF_8))
    private val reader = BufferedReader(InputStreamReader(fromServer, StandardCharsets.UTF_8))

    def send(line: String): Unit =
      sendWithoutNewline(line)
      writer.newLine()
      writer.flush()

    def sendWithoutNewline(line: String): Unit =
      writer.write(line)
      writer.flush()

    def closeInput(): Unit = writer.close()

    def readResponse(): Json = parser.parse(reader.readLine()).toOption.get

  private def withStdioServer[A](maxLineLength: Int = StdioFraming.defaultMaxLineLength)(body: StdioSession => A): A =
    val toServer = PipedOutputStream()
    val serverIn = PipedInputStream(toServer, 64 * 1024)
    val fromServer = PipedInputStream()
    val serverOut = PipedOutputStream(fromServer)

    val session = StdioSession(toServer, fromServer, runStdioServer(server, serverIn, serverOut, maxLineLength))

    try body(session)
    finally session.closeInput()

  "a stdio server" should "answer requests and stream notifications over stdin/stdout" in withStdioServer() { session =>
    session.send(
      """{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"t","version":"1"}}}"""
    )
    val init = session.readResponse()
    init.hcursor.downField("id").as[Int] shouldBe Right(1)
    init.hcursor.downField("result").downField("serverInfo").downField("name").as[String].isRight shouldBe true

    session.send("""{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"echo","arguments":{"message":"hi"}}}""")
    val echo = session.readResponse()
    echo.hcursor.downField("result").downField("content").downN(0).downField("text").as[String] shouldBe Right("hi")

    session.send("""{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"noisy","arguments":{}}}""")
    val notifications = List(session.readResponse(), session.readResponse(), session.readResponse())
    notifications.map(_.hcursor.downField("method").as[String]) shouldBe List.fill(3)(Right("notifications/message"))
    notifications.flatMap(_.hcursor.downField("params").downField("data").as[String].toOption) shouldBe List("one", "two", "three")

    val response = session.readResponse()
    response.hcursor.downField("id").as[Int] shouldBe Right(3)
    response.hcursor.downField("result").downField("content").downN(0).downField("text").as[String] shouldBe Right("done")
  }

  it should "skip notifications and malformed lines, and still report protocol errors" in withStdioServer() { session =>
    session.send("""{"jsonrpc":"2.0","method":"notifications/initialized"}""")
    session.send("this is not valid json")
    session.send("""{"jsonrpc":"2.0","id":9,"method":"tools/call","params":{"name":"missing","arguments":{}}}""")

    val error = session.readResponse()
    error.hcursor.downField("id").as[Int] shouldBe Right(9)
    error.hcursor.downField("error").downField("code").as[Int] shouldBe Right(JSONRPCErrorCodes.MethodNotFound.code)
    error.hcursor.downField("error").downField("message").as[String].toOption.get should include("missing")
  }

  it should "answer a request which is not terminated by a newline" in withStdioServer() { session =>
    session.sendWithoutNewline("""{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"echo","arguments":{"message":"last"}}}""")
    session.closeInput()

    val echo = session.readResponse()
    echo.hcursor.downField("id").as[Int] shouldBe Right(4)
    echo.hcursor.downField("result").downField("content").downN(0).downField("text").as[String] shouldBe Right("last")

    Await.result(session.served, 10.seconds)
  }

  it should "fail when an incoming line is longer than the maximum length" in withStdioServer(maxLineLength = 1024) { session =>
    val padding = "x" * 8192
    session.send(s"""{"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"echo","arguments":{"message":"$padding"}}}""")

    val failure = intercept[McpLineTooLongException](Await.result(session.served, 10.seconds))
    failure.maxLineLength shouldBe 1024
  }
