//> using dep com.softwaremill.chimp::chimp-client:0.3.0
//> using dep com.softwaremill.sttp.client4::core:4.0.23
//> using dep ch.qos.logback:logback-classic:1.5.32

// Start the bundled mcp/everything server before running this example:
// cd examples && docker compose up -d

package examples.client

import chimp.client.*
import chimp.client.transport.ClientHttpTransport
import chimp.protocol.*
import io.circe.Json
import sttp.client4.DefaultSyncBackend
import sttp.model.Uri.UriContext
import sttp.shared.Identity

@main def everythingClient(): Unit =
  val backend = DefaultSyncBackend()
  val transport = ClientHttpTransport[Identity](backend, uri"http://localhost:3001/mcp")
  val client = McpClient[Identity](
    transport,
    clientInfo = Implementation(name = "chimp-everything-client", version = "0.1.0"),
    protocolVersion = ProtocolVersion.Latest
  )

  try
    println(s"Connected to ${client.serverInfo.name} ${client.serverInfo.version}")

    val tools = client.listTools().tools
    println(s"Available tools (${tools.size}):")
    tools.foreach(t => println(s"  - ${t.name}: ${t.description.getOrElse("")}"))

    println("\n--- calling echo ---")
    val echoResult = client.callTool("echo", Json.obj("message" -> Json.fromString("hello chimp")))
    echoResult.content.foreach:
      case ToolContent.Text(_, text) => println(text)
      case other                     => println(s"(non-text content: $other)")

    println("\n--- calling get-sum ---")
    val sumResult = client.callTool("get-sum", Json.obj("a" -> Json.fromInt(7), "b" -> Json.fromInt(35)))
    sumResult.content.foreach:
      case ToolContent.Text(_, text) => println(text)
      case other                     => println(s"(non-text content: $other)")
  finally
    client.close()
    backend.close()
