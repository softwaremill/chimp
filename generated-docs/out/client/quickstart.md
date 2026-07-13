# Quickstart

Chimp ships an MCP client that connects to any MCP-compliant server. The client is parameterised over an effect type `F[_]` and is paired with a pluggable transport that carries JSON-RPC messages.

Add the dependency to your `build.sbt`:

```scala
libraryDependencies += "com.softwaremill.chimp" %% "chimp-client" % "0.4.0"
```

## Example: the simplest MCP client

Below is a self-contained, [scala-cli](https://scala-cli.virtuslab.org)-runnable example that connects to an MCP server over HTTP and invokes a tool:

```scala
import chimp.client.*
import chimp.client.transport.ClientHttpTransport
import chimp.protocol.*
import io.circe.Json
import sttp.client4.DefaultSyncBackend
import sttp.model.Uri.UriContext
import sttp.shared.Identity

object QuickstartClient:
  def main(args: Array[String]): Unit =
    val backend = DefaultSyncBackend()
    val transport = ClientHttpTransport[Identity](backend, uri"http://localhost:8080/mcp")
    val client = McpClient[Identity](transport, Implementation("my-client", "0.1.0"))

    val result = client.callTool("adder", Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(3)))
    result.content.collect { case ToolContent.Text(_, text) => text }.foreach(println)

    client.close()
    backend.close()
```

For streaming transports, also add the dependency for your effect system — ZIO:

```scala
libraryDependencies += "com.softwaremill.chimp" %% "chimp-client-zio" % "0.4.0"
```

or direct-style Ox:

```scala
libraryDependencies += "com.softwaremill.chimp" %% "chimp-client-ox" % "0.4.0"
```
