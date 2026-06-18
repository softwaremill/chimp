package chimp.server

import chimp.protocol.LoggingLevel
import chimp.server.transport.StdioServerTransport
import io.circe.{parser, Codec, Json}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.tapir.Schema

import java.io.{BufferedReader, BufferedWriter, InputStreamReader, OutputStreamWriter, PipedInputStream, PipedOutputStream}
import java.nio.charset.StandardCharsets

class StdioServerTransportSpec extends AnyFlatSpec with Matchers:
  private case class EchoInput(message: String) derives Codec, Schema
  private case class NoInput() derives Codec, Schema

  private def server: StreamingMcpServer[sttp.shared.Identity] =
    StreamingMcpServer[sttp.shared.Identity]()
      .withLoggingLevel(_ => ())
      .addTool(tool("echo").input[EchoInput].handle(in => ToolResult.text(in.message)))
      .addStreamingTool(
        tool("noisy")
          .input[NoInput]
          .streamingServerLogic[sttp.shared.Identity] { (_, ctx, _) =>
            ctx.log(LoggingLevel.Info, Json.fromString("one"))
            ctx.log(LoggingLevel.Info, Json.fromString("two"))
            ctx.log(LoggingLevel.Info, Json.fromString("three"))
            ToolResult.text("done")
          }
      )

  "a stdio server" should "answer requests and stream notifications over stdin/stdout" in {
    val toServer = PipedOutputStream()
    val serverIn = PipedInputStream(toServer)
    val fromServer = PipedInputStream()
    val serverOut = PipedOutputStream(fromServer)

    val thread = Thread(() => StdioServerTransport(serverIn, serverOut).serve(server))
    thread.setDaemon(true)
    thread.start()

    val writer = BufferedWriter(OutputStreamWriter(toServer, StandardCharsets.UTF_8))
    val reader = BufferedReader(InputStreamReader(fromServer, StandardCharsets.UTF_8))

    def send(line: String): Unit =
      writer.write(line)
      writer.newLine()
      writer.flush()

    def readResponse(): Json = parser.parse(reader.readLine()).toOption.get

    try
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
    finally
      writer.close()
      thread.join(2000)
  }
