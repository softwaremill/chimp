package chimp

//> using dep com.softwaremill.chimp::core:0.1.2
//> using dep com.softwaremill.sttp.tapir::tapir-netty-server-sync:1.11.33
//> using dep ch.qos.logback::logback-classic:1.5.18

import sttp.tapir.*
import sttp.tapir.server.netty.sync.NettySyncServer


@main def mcpAuthApp(): Unit =
  val adderTool = tool("adder")
    .description("Adds two numbers")
    .withAnnotations(ToolAnnotations(idempotentHint = Some(true)))
    .input[Input]

  def logic(i: Input, headerValue: Option[String]): Either[String, String] =
    val tokenMsg = headerValue.map(t => s"token: $t").getOrElse("no token provided")
    Right(s"The result is ${i.a + i.b} ($tokenMsg)")

  val adderServerTool = adderTool.handleWithHeader(logic)

  val mcpServerEndpoint = mcpEndpoint(List(adderServerTool), List("mcp"), Some("test_header_1"))

  NettySyncServer()
    .port(8080)
    .addEndpoint(mcpServerEndpoint)
    .startAndWait()
