# Examples

## HTTP client

A synchronous client over `ClientHttpTransport`, calling a tool:

```scala mdoc:compile-only
import chimp.client.*
import chimp.client.transport.ClientHttpTransport
import chimp.protocol.*
import io.circe.Json
import sttp.client4.DefaultSyncBackend
import sttp.model.Uri.UriContext
import sttp.shared.Identity

object HttpClient:
  def main(args: Array[String]): Unit =
    val backend = DefaultSyncBackend()
    val transport = ClientHttpTransport[Identity](backend, uri"http://localhost:8080/mcp")
    val client = McpClient[Identity](transport, Implementation("my-client", "0.1.0"))

    val result = client.callTool("adder", Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(3)))
    result.content.collect { case ToolContent.Text(_, text) => text }.foreach(println)

    client.close()
    backend.close()
```

## STDIO client

A synchronous client that launches a local MCP server as a subprocess over `ClientStdioTransport`:

```scala mdoc:compile-only
import chimp.client.*
import chimp.client.transport.ClientStdioTransport
import chimp.protocol.*
import io.circe.Json
import sttp.shared.Identity

object StdioClient:
  def main(args: Array[String]): Unit =
    val transport = ClientStdioTransport(command = List("my-mcp-server"))
    val client = McpClient[Identity](transport, Implementation("my-client", "0.1.0"))

    val result = client.callTool("adder", Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(3)))
    result.content.collect { case ToolContent.Text(_, text) => text }.foreach(println)

    client.close()
```

## Roots over a ZIO streaming transport

[Roots](https://modelcontextprotocol.io/specification/2025-11-25/client/roots) require a bidirectional, streaming transport — here `ZioClientHttpTransport`:

```scala mdoc:compile-only
import chimp.client.*
import chimp.client.transport.zio.ZioClientHttpTransport
import chimp.protocol.*
import sttp.client4.httpclient.zio.HttpClientZioBackend
import sttp.model.Uri.UriContext
import zio.*

object RootsClient extends ZIOAppDefault:
  def run =
    HttpClientZioBackend.scoped().flatMap { backend =>
      ZIO.scoped {
        for
          transport <- ZioClientHttpTransport.scoped(backend, uri"http://localhost:8080/mcp")
          client <- McpClient.bidirectional[Task](
            transport,
            clientInfo = Implementation("my-client", "0.1.0"),
            rootsHandler = Some(() => ZIO.succeed(ListRootsResult(roots = List(Root("file:///workspace", Some("workspace"))))))
          )
          tools <- client.listTools()
          _ <- Console.printLine(s"server exposes ${tools.tools.size} tools")
        yield ()
      }
    }
```

## Roots over an Ox streaming transport

The same bidirectional client in direct style, using `OxClientHttpTransport`. Its background SSE listener runs as a fork in the surrounding `supervised` scope, so the transport is created and used inside `supervised`:

```scala mdoc:compile-only
import chimp.client.*
import chimp.client.transport.ox.OxClientHttpTransport
import chimp.protocol.*
import ox.supervised
import sttp.client4.DefaultSyncBackend
import sttp.model.Uri.UriContext
import sttp.shared.Identity

object RootsOxClient:
  def main(args: Array[String]): Unit =
    supervised:
      val backend = DefaultSyncBackend()
      val transport = OxClientHttpTransport(backend, uri"http://localhost:8080/mcp")
      val client = McpClient.bidirectional[Identity](
        transport,
        clientInfo = Implementation("my-client", "0.1.0"),
        rootsHandler = Some(() => ListRootsResult(roots = List(Root("file:///workspace", Some("workspace")))))
      )
      val tools = client.listTools()
      println(s"server exposes ${tools.tools.size} tools")
      client.close()
      backend.close()
```

More runnable examples live in [`examples/`](https://github.com/softwaremill/chimp/tree/master/examples/src/main/scala/examples).
