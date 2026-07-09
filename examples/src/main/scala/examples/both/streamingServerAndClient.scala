//> using dep com.softwaremill.chimp::chimp-server-ox:0.3.0
//> using dep com.softwaremill.chimp::chimp-client-ox:0.3.0
//> using dep ch.qos.logback:logback-classic:1.5.37

package examples.both

import chimp.client.McpClient
import chimp.client.notifications.ServerNotification
import chimp.client.transport.ox.OxClientHttpTransport
import chimp.protocol.{Implementation, LoggingLevel, ToolContent}
import chimp.server.ox.OxServerHttpTransport
import chimp.server.{tool, StreamingMcpServer, ToolResult}
import io.circe.{Codec, Json}
import ox.supervised
import sttp.client4.DefaultSyncBackend
import sttp.model.Uri.UriContext
import sttp.shared.Identity
import sttp.tapir.Schema
import sttp.tapir.server.netty.sync.NettySyncServer

case class NoInput() derives Codec, Schema

// Runs an Ox streaming MCP server and an Ox bidirectional client in one process: the client calls
// the `noisy` tool and receives the log notifications the tool emits over SSE while it runs.
@main def streamingServerAndClient(): Unit =
  supervised:
    val noisy = tool("noisy")
      .description("Logs three messages, then returns")
      .input[NoInput]
      .streamingServerLogic[Identity]: (_, ctx, _) =>
        ctx.log(LoggingLevel.Info, Json.fromString("one"))
        ctx.log(LoggingLevel.Info, Json.fromString("two"))
        ctx.log(LoggingLevel.Info, Json.fromString("three"))
        ToolResult.text("done")
    val server = StreamingMcpServer[Identity]().withLoggingLevel(_ => ()).addStreamingTool(noisy)
    val binding = NettySyncServer().port(0).addEndpoint(OxServerHttpTransport(List("mcp")).serve(server)).start()
    try
      val backend = DefaultSyncBackend()
      try
        val transport = OxClientHttpTransport(backend, uri"http://localhost:${binding.port}/mcp")
        val client = McpClient.bidirectional[Identity](transport, Implementation("both-client", "0.1.0"))
        client.onServerNotification:
          case ServerNotification.LoggingMessage(params) => println(s"notification: ${params.data}")
          case _                                         => ()
        val result = client.callTool("noisy", Json.obj())
        result.content.collect { case ToolContent.Text(_, text) => text }.foreach(text => println(s"result: $text"))
        client.close()
      finally backend.close()
    finally binding.stop()
