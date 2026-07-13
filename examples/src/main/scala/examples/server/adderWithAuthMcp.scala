//> using dep com.softwaremill.chimp::chimp-server:0.4.0
//> using dep com.softwaremill.sttp.tapir::tapir-netty-server-sync:1.11.50
//> using dep ch.qos.logback:logback-classic:1.5.20

package examples.server

import chimp.server.*
import io.circe.Codec
import sttp.model.Header
import sttp.tapir.*
import sttp.tapir.server.netty.sync.NettySyncServer

@main def mcpAuthApp(): Unit =
  case class Input(a: Int, b: Int) derives Codec, Schema

  val adderTool = tool("adder")
    .description("Adds two numbers")
    .withAnnotations(ToolAnnotations(idempotentHint = Some(true)))
    .input[Input]

  def logic(i: Input, headers: Seq[Header]): Either[String, String] =
    val tokenMsg =
      headers.find(_.name == "test_header").map(t => s"token: ${t.value} (header name used: ${t.name})").getOrElse("no token provided")
    Right(s"The result is ${i.a + i.b} ($tokenMsg)")

  val adderServerTool = adderTool.handleWithHeaders((i, headers) => ToolResult.fromEither(logic(i, headers)))

  val mcpServerEndpoint = McpServer(tools = List(adderServerTool)).endpoint(List("mcp"))

  NettySyncServer()
    .port(8080)
    .addEndpoint(mcpServerEndpoint)
    .startAndWait()
