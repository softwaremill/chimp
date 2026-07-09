//> using dep com.softwaremill.chimp::chimp-server-ox:0.3.0
//> using dep ch.qos.logback:logback-classic:1.5.37

package examples.server

import chimp.server.ox.OxServerStdioTransport
import chimp.server.{tool, StreamingMcpServer, ToolResult}
import io.circe.Codec
import sttp.shared.Identity
import sttp.tapir.Schema

case class StdioEchoInput(message: String) derives Codec, Schema

@main def mcpStdioOxApp(): Unit =
  val echo = tool("echo").description("Echoes the message").input[StdioEchoInput].handle(in => ToolResult.text(in.message))
  val server = StreamingMcpServer[Identity]().addTool(echo)
  OxServerStdioTransport().serve(server)
