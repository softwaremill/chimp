package chimp.server

import chimp.protocol.{JSONRPCErrorCodes, LoggingLevel, ProtocolMeta}
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

  private def server(publisher: NotificationPublisher): StreamingMcpServer[F] =
    StreamingMcpServer[F]()
      .withLoggingLevel(_ => monad.unit(()))
      .withPublisher(publisher)
      .addTool(tool("echo").input[EchoInput].serverLogic[F]((in, _) => monad.unit(ToolResult.text(in.message))))
      .addTool(
        tool("ask")
          .input[NoInput]
          .serverLogicWithContext[F]((_, ctx, _) =>
            monad.unit(ctx.request.rootsResult("roots").fold(InputRequired("roots" -> InputRequired.listRoots))(_ => ToolResult.text("ok")))
          )
      )
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

  protected class StdioSession(
      toServer: PipedOutputStream,
      fromServer: PipedInputStream,
      val served: Future[Unit],
      val publisher: NotificationPublisher
  ):
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

    val publisher = NotificationPublisher()
    val session = StdioSession(toServer, fromServer, runStdioServer(server(publisher), serverIn, serverOut, maxLineLength), publisher)

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

  private val modernMeta = """{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{}}"""

  private def listen(id: Int, filter: String): String =
    s"""{"jsonrpc":"2.0","id":$id,"method":"subscriptions/listen","params":{"notifications":$filter,"_meta":$modernMeta}}"""

  private def subscriptionId(message: Json): Option[Int] =
    message.hcursor.downField("params").downField("_meta").get[Int](ProtocolMeta.SubscriptionId).toOption

  private def method(message: Json): Option[String] = message.hcursor.get[String]("method").toOption

  private def echo(session: StdioSession, id: Int): Json =
    session.send(s"""{"jsonrpc":"2.0","id":$id,"method":"tools/call","params":{"name":"echo","arguments":{"message":"m"}}}""")
    session.readResponse()

  it should "keep several subscriptions on one channel apart, and end one on notifications/cancelled" in withStdioServer() { session =>
    session.send(listen(21, """{"toolsListChanged":true}"""))
    val first = session.readResponse()
    session.send(listen(22, """{"toolsListChanged":true,"promptsListChanged":true}"""))
    val second = session.readResponse()
    List(first, second).map(method) shouldBe List.fill(2)(Some("notifications/subscriptions/acknowledged"))
    List(first, second).map(subscriptionId) shouldBe List(Some(21), Some(22))
    second.hcursor.downField("params").downField("notifications").focus shouldBe parser.parse("""{"toolsListChanged":true}""").toOption

    session.publisher.toolsListChanged()
    val changes = List(session.readResponse(), session.readResponse())
    changes.map(method) shouldBe List.fill(2)(Some("notifications/tools/list_changed"))
    changes.flatMap(subscriptionId).sorted shouldBe List(21, 22)

    session.send("""{"jsonrpc":"2.0","method":"notifications/cancelled","params":{"requestId":21}}""")
    echo(session, 23).hcursor.get[Int]("id") shouldBe Right(23)
    session.publisher.toolsListChanged()
    subscriptionId(session.readResponse()) shouldBe Some(22)
    echo(session, 24).hcursor.get[Int]("id") shouldBe Right(24)
  }

  it should "end open subscriptions with a listen result and notifications/cancelled when its input ends" in withStdioServer() { session =>
    session.send(listen(31, """{"toolsListChanged":true}"""))
    method(session.readResponse()) shouldBe Some("notifications/subscriptions/acknowledged")
    session.closeInput()

    val result = session.readResponse()
    result.hcursor.get[Int]("id") shouldBe Right(31)
    result.hcursor.downField("result").get[String]("resultType") shouldBe Right("complete")
    result.hcursor.downField("result").downField("_meta").get[Int](ProtocolMeta.SubscriptionId) shouldBe Right(31)
    val cancelled = session.readResponse()
    method(cancelled) shouldBe Some("notifications/cancelled")
    cancelled.hcursor.downField("params").get[Int]("requestId") shouldBe Right(31)

    Await.result(session.served, 10.seconds)
    session.publisher.openSubscriptions shouldBe 0
  }

  private def ask(id: Int, capabilities: String, extra: String = ""): String =
    val meta = s"""{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":$capabilities}"""
    s"""{"jsonrpc":"2.0","id":$id,"method":"tools/call","params":{"name":"ask","arguments":{}$extra,"_meta":$meta}}"""

  it should "answer an input-required tool call, its retry, and an input request the client has no capability for" in withStdioServer() {
    session =>
      session.send(ask(41, """{"roots":{}}"""))
      session.readResponse().hcursor.downField("result").get[String]("resultType") shouldBe Right("input_required")
      session.send(ask(42, """{"roots":{}}""", ""","inputResponses":{"roots":{"roots":[]}}"""))
      session.readResponse().hcursor.downField("result").downField("content").downN(0).get[String]("text") shouldBe Right("ok")
      session.send(ask(43, "{}"))
      val error = session.readResponse()
      error.hcursor.get[Int]("id") shouldBe Right(43)
      error.hcursor.downField("error").get[Int]("code") shouldBe Right(JSONRPCErrorCodes.MissingRequiredClientCapability.code)
  }
