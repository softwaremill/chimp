//> using dep com.softwaremill.chimp::chimp-server:0.3.0
//> using dep com.softwaremill.chimp::chimp-client:0.3.0
//> using dep com.softwaremill.sttp.tapir::tapir-netty-server-sync:1.13.19
//> using dep ch.qos.logback:logback-classic:1.5.20

package examples.both

import chimp.client.McpClient
import chimp.client.transport.ClientHttpTransport
import chimp.protocol.{Implementation, ToolContent}
import chimp.server.{tool, McpServer, ToolResult}
import io.circe.{Codec, Json}
import ox.supervised
import sttp.client4.DefaultSyncBackend
import sttp.model.Uri.UriContext
import sttp.shared.Identity
import sttp.tapir.Schema
import sttp.tapir.server.netty.sync.NettySyncServer

case class AddInput(a: Int, b: Int) derives Codec, Schema

// Runs a plain (non-streaming) MCP server and a synchronous client in one process: the client lists
// the server's tools and calls one over a simple request/response HTTP exchange.
@main def serverAndClient(): Unit =
  supervised:
    val adder = tool("adder").description("Adds two numbers").input[AddInput].handle(in => ToolResult.text(s"${in.a + in.b}"))
    val binding = NettySyncServer().port(0).addEndpoint(McpServer(tools = List(adder)).endpoint(List("mcp"))).start()
    try
      val backend = DefaultSyncBackend()
      try
        val transport = ClientHttpTransport[Identity](backend, uri"http://localhost:${binding.port}/mcp")
        val client = McpClient[Identity](transport, Implementation("both-client", "0.1.0"))
        val tools = client.listTools()
        println(s"tools: ${tools.tools.map(_.name).mkString(", ")}")
        val result = client.callTool("adder", Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(3)))
        result.content.collect { case ToolContent.Text(_, text) => text }.foreach(text => println(s"2 + 3 = $text"))
        client.close()
      finally backend.close()
    finally binding.stop()
