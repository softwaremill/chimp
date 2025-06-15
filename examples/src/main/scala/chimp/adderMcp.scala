package chimp

//> using dep com.softwaremill.chimp::core:0.1.2
//> using dep com.softwaremill.sttp.tapir::tapir-netty-server-sync:1.11.33

import sttp.tapir.*
import io.circe.Codec
import sttp.tapir.server.netty.sync.NettySyncServer

case class Input(a: Int, b: Int) derives Codec, Schema

@main def mcpApp(): Unit =
  val adderTool = tool("adder")
    .description("Adds two numbers")
    .withAnnotations(ToolAnnotations(idempotentHint = Some(true)))
    .input[Input]

  def logic(i: Input): Either[String, String] = Right(s"The result is ${i.a + i.b}")

  val adderServerTool = adderTool.handle(logic)

  val mcpServerEndpoint = mcpEndpoint(List(adderServerTool), List("mcp"))

  NettySyncServer()
    .port(8080)
    .addEndpoint(mcpServerEndpoint)
    .startAndWait()
