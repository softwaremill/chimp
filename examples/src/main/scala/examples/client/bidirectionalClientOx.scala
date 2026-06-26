//> using dep com.softwaremill.chimp::chimp-client-ox:0.3.0
//> using dep ch.qos.logback:logback-classic:1.5.20

package examples.client

import chimp.client.*
import chimp.client.notifications.ServerNotification
import chimp.client.transport.ox.OxClientHttpTransport
import chimp.protocol.*
import ox.supervised
import sttp.client4.DefaultSyncBackend
import sttp.model.Uri.UriContext
import sttp.shared.Identity

@main def bidirectionalOxClientApp(): Unit =
  supervised:
    val backend = DefaultSyncBackend()
    val transport = OxClientHttpTransport(backend, uri"http://localhost:8080/mcp")
    val client = McpClient.bidirectional[Identity](transport, Implementation("ox-client", "0.1.0"))
    client.onServerNotification:
      case ServerNotification.ResourceUpdated(params) => println(s"resource changed: ${params.uri}")
      case _                                          => ()
    val tools = client.listTools()
    println(s"server exposes ${tools.tools.size} tools")
    client.close()
    backend.close()
