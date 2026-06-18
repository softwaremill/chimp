# Examples

## HTTP client

A synchronous client over `HttpTransport`, calling a tool:

```scala
//> using dep com.softwaremill.chimp::chimp-client:0.2.0
//> using dep com.softwaremill.sttp.client4::core:4.0.23

import chimp.client.*
import chimp.client.transport.ClientHttpTransport
import chimp.protocol.*
import io.circe.Json
import sttp.client4.DefaultSyncBackend
import sttp.model.Uri.UriContext
import sttp.shared.Identity

@main def httpClient(): Unit =
val backend = DefaultSyncBackend()
val transport = ClientHttpTransport[Identity](backend, uri"http://localhost:8080/mcp")
val client = McpClient[Identity](transport, Implementation("my-client", "0.1.0"))

val result = client.callTool("adder", Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(3)))
result.content.collect { case ToolContent.Text(_, text) => println(text) }

client.close()
backend.close()
```

## STDIO client

A synchronous client that launches a local MCP server as a subprocess over `StdioTransport`:

```scala
//> using dep com.softwaremill.chimp::chimp-client:0.2.0

import chimp.client.*
import chimp.client.transport.ClientStdioTransport
import chimp.protocol.*
import io.circe.Json
import sttp.shared.Identity

@main def stdioClient(): Unit =
val transport = StdioTransport(command = List("my-mcp-server"))
val client = McpClient[Identity](transport, Implementation("my-client", "0.1.0"))

val result = client.callTool("adder", Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(3)))
result.content.collect { case ToolContent.Text(_, text) => println(text) }

client.close()
```

## Roots over a ZIO streaming transport

[Roots](https://modelcontextprotocol.io/specification/2025-11-25/client/roots) require a bidirectional, streaming transport — here `ZioStreamingHttpTransport`:

```scala
//> using dep com.softwaremill.chimp::chimp-client-zio:0.2.0
//> using dep com.softwaremill.sttp.client4::zio:4.0.23

import chimp.client.*
import chimp.client.transport.zio.ZioClientHttpTransport
import chimp.protocol.*
import sttp.client4.httpclient.zio.HttpClientZioBackend
import sttp.model.Uri.UriContext
import zio.*

object RootsClient extends ZIOAppDefault

:
def run =
  HttpClientZioBackend.scoped().flatMap { backend =>
    ZIO.scoped {
      for
        transport
      <- ZioClientHttpTransport.scoped(backend, uri"http://localhost:8080/mcp")
      client
      <- McpClient.bidirectional[Task](
        transport,
        clientInfo = Implementation("my-client", "0.1.0"),
        rootsHandler = Some(() =>
          ZIO.succeed(ListRootsResult(roots = List(Root("file:///workspace", Some("workspace"))))))
      )
      tools
      <- client.listTools()
      _
      <- Console.printLine(s"server exposes ${tools.tools.size} tools")
      yield ()
    }
  }
```

More runnable examples live in [`examples/`](https://github.com/softwaremill/chimp/tree/master/examples/src/main/scala/examples).
