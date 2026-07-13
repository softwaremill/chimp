//> using dep com.softwaremill.chimp::chimp-server:0.4.0
//> using dep com.softwaremill.sttp.tapir::tapir-netty-server-sync:1.11.50
//> using dep ch.qos.logback:logback-classic:1.5.20

package examples.server

import chimp.server.*
import io.circe.Codec
import sttp.tapir.*
import sttp.tapir.server.netty.sync.NettySyncServer

case class Input(a: Int, b: Int) derives Codec, Schema

@main def mcpApp(): Unit =
  val adderTool = tool("adder")
    .description("Adds two numbers")
    .withAnnotations(ToolAnnotations(idempotentHint = Some(true)))
    .input[Input]

  def logic(i: Input): Either[String, String] = Right(s"The result is ${i.a + i.b}")

  val adderServerTool = adderTool.handle(i => ToolResult.fromEither(logic(i)))

  val mcpServerEndpoint = McpServer(tools = List(adderServerTool)).endpoint(List("mcp"))

  NettySyncServer()
    .port(8080)
    .addEndpoint(mcpServerEndpoint)
    .startAndWait()
