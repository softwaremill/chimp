# Quickstart

Add the dependency to your `build.sbt`:

```scala
libraryDependencies += "com.softwaremill.chimp" %% "chimp-client" % "0.2.0"
```

## Example: the simplest MCP client

Below is a self-contained, [scala-cli](https://scala-cli.virtuslab.org)-runnable example that connects to an MCP server over HTTP and invokes a tool:

```scala
//> using dep com.softwaremill.chimp::chimp-client:0.2.0
//> using dep com.softwaremill.sttp.client4::core:4.0.23

import chimp.client.*
import chimp.client.transport.HttpTransport
import chimp.protocol.*
import io.circe.Json
import sttp.client4.DefaultSyncBackend
import sttp.model.Uri.UriContext
import sttp.shared.Identity

@main def mcpClient(): Unit =
  val backend = DefaultSyncBackend()
  val transport = HttpTransport[Identity](backend, uri"http://localhost:8080/mcp")
  val client = McpClient[Identity](transport, Implementation("my-client", "0.1.0"))

  val result = client.callTool("adder", Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(3)))
  result.content.collect { case ToolContent.Text(_, text) => println(text) }

  client.close()
  backend.close()
```

For streaming transports (e.g. ZIO), also add:

```scala
libraryDependencies += "com.softwaremill.chimp" %% "chimp-client-zio" % "0.2.0"
```
