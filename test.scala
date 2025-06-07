//> using dep com.softwaremill.chimp::core:0.1.0

import chimp.*
import sttp.tapir.*
import sttp.tapir.server.netty.sync.NettySyncServer

// define the input type for your tool
case class AdderInput(a: Int, b: Int) derives io.circe.Codec, Schema

@main def mcpApp(): Unit =
  // describe the tool providing the name, description, and input type
  val adderTool = tool("adder")
    .description("Adds two numbers")
    .input[AdderInput]

  // combine the tool description with the server-side logic
  val adderServerTool = adderTool.handle(i => Right(s"The result is ${i.a + i.b}"))

  // create the MCP server endpoint; it will be available at http://localhost:8080/mcp
  val mcpServerEndpoint = mcpEndpoint(List(adderServerTool)).prependSecurityIn("mcp")

  // start the server on port 8080
  NettySyncServer().port(8080).addEndpoint(mcpServerEndpoint).startAndWait()
