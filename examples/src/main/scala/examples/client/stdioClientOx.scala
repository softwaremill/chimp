//> using dep com.softwaremill.chimp::chimp-client-ox:0.3.0
//> using dep ch.qos.logback:logback-classic:1.5.20

package examples.client

import chimp.client.*
import chimp.client.transport.ox.OxClientStdioTransport
import chimp.protocol.*
import ox.supervised
import sttp.shared.Identity

@main def stdioOxClientApp(): Unit =
  supervised:
    val transport = OxClientStdioTransport(List("npx", "-y", "@modelcontextprotocol/server-everything"))
    val client = McpClient.bidirectional[Identity](transport, Implementation("ox-stdio-client", "0.1.0"))
    val tools = client.listTools()
    println(s"server exposes ${tools.tools.size} tools")
    client.close()
