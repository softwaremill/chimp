//> using dep com.softwaremill.chimp::core:0.1.6
//> using dep com.softwaremill.sttp.tapir::tapir-netty-server-sync:1.11.50
//> using dep ch.qos.logback:logback-classic:1.5.20

package chimp

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

  val adderServerTool = adderTool.handleWithHeaders(logic)

  val mcpServerEndpoint = mcpEndpoint(List(adderServerTool), List("mcp"))

  NettySyncServer()
    .port(8080)
    .addEndpoint(mcpServerEndpoint)
    .startAndWait()
