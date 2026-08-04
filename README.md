# Chimp MCP

[![CI](https://github.com/softwaremill/chimp/actions/workflows/ci.yml/badge.svg)](https://github.com/softwaremill/chimp/actions/workflows/ci.yml)
[![Scala 3](https://img.shields.io/badge/scala-3-blue.svg)](https://www.scala-lang.org/)

An SDK for building [MCP](https://modelcontextprotocol.io/specification) (Model Context Protocol) servers and
clients in Scala 3 using boilerplate-less, type-safe APIs based on [Tapir](https://tapir.softwaremill.com/)
and [sttp](https://github.com/softwaremill/sttp), supporting the variety of the Scala ecosystem.

### Transport

Chimp implements both streamable HTTP and stdio transports. Additional integration modules unlock streaming features of 
the MCP protocol with bidirectional communication between server and client. Currently supporting [Ox](https://ox.softwaremill.com/) and [ZIO](https://zio.dev/).

### Quickstart

#### HTTP

Run a basic MCP server with Netty exposing a simple adder tool:

```scala
//> using dep com.softwaremill.chimp::chimp-server:0.4.0
//> using dep com.softwaremill.sttp.tapir::tapir-netty-server-sync:1.13.19

import chimp.server.*
import io.circe.Codec
import sttp.tapir.*
import sttp.tapir.server.netty.sync.NettySyncServer

case class AdderInput(a: Int, b: Int) derives Codec, Schema

@main def server(): Unit =
  val adder = tool("adder").description("Adds two numbers").input[AdderInput]
    .handle(i => ToolResult.text(s"Result: ${i.a + i.b}"))

  NettySyncServer().port(8080).addEndpoint(McpServer(tools = List(adder)).endpoint(List("mcp"))).startAndWait()
```

Connect and invoke the tool as an MCP client:

```scala
//> using dep com.softwaremill.chimp::chimp-client:0.4.0
//> using dep com.softwaremill.sttp.client4::core:4.0.24

import chimp.client.*
import chimp.client.transport.ClientHttpTransport
import chimp.protocol.*
import sttp.client4.*
import io.circe.Json

@main def client(): Unit =
  val backend = DefaultSyncBackend()
  val transport = ClientHttpTransport(backend, uri"http://localhost:8080/mcp")
  val client = McpClient(transport, Implementation("my-client", "0.1.0"))

  val result = client.callTool("adder", Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(3)))
  val _ = result.content.collect { case ToolContent.Text(_, text) => println(text) }

  client.close()
  backend.close()
```

#### stdio

Run a basic MCP server using stdio transport:

```scala
//> using dep com.softwaremill.chimp::chimp-server:0.4.0

import chimp.server.*
import chimp.server.transport.ServerStdioTransport
import io.circe.Codec
import sttp.tapir.*

case class EchoInput(message: String) derives Codec, Schema

@main def stdioServer(): Unit =
  val echo = tool("echo").description("Echoes the message").input[EchoInput]
    .handle(i => ToolResult.text(i.message))

  ServerStdioTransport().serve(McpServer(tools = List(echo)))
```

Start the server as a subprocess and invoke the tool as an MCP client:

```scala
//> using dep com.softwaremill.chimp::chimp-client:0.4.0

import chimp.client.*
import chimp.client.transport.ClientStdioTransport
import chimp.protocol.*
import io.circe.Json

@main def stdioClient(): Unit =
  val transport = ClientStdioTransport(List("scala-cli", "run", "stdioServer.scala"))
  val client = McpClient(transport, Implementation("my-client", "0.1.0"))

  val result = client.callTool("echo", Json.obj("message" -> Json.fromString("hello")))
  val _ = result.content.collect { case ToolContent.Text(_, text) => println(text) }

  client.close()
```

#### Bidirectional streaming

With the integration modules (like `chimp-server-ox` and `chimp-client-ox` for ox) bidirectional communication becomes possible.
For example, run a basic streaming MCP server using Netty and ox:

```scala
//> using dep com.softwaremill.chimp::chimp-server-ox:0.4.0

import chimp.protocol.LoggingLevel
import chimp.server.*
import chimp.server.ox.OxServerHttpTransport
import io.circe.{Codec, Json}
import sttp.shared.Identity
import sttp.tapir.*
import sttp.tapir.server.netty.sync.NettySyncServer

case class WorkInput(steps: Int) derives Codec, Schema

@main def streamingServer(): Unit =
  val work = tool("work")
    .description("Reports progress and logs while running")
    .input[WorkInput]
    .streamingServerLogic[Identity]: (in, ctx, _) =>
      for step <- 1 to in.steps do
        ctx.reportProgress(step.toDouble / in.steps, total = Some(1.0))
        ctx.log(LoggingLevel.Info, Json.fromString(s"step $step of ${in.steps}"))
      ToolResult.text("done")

  val server = StreamingMcpServer[Identity]().withLoggingLevel(_ => ()).addStreamingTool(work)
  NettySyncServer().port(8080).addEndpoint(OxServerHttpTransport(List("mcp")).serve(server)).startAndWait()
```

Connect and invoke the tool as an MCP client, receiving server's notifications while the tool call is in flight:

```scala
//> using dep com.softwaremill.chimp::chimp-client-ox:0.4.0

import chimp.client.McpClient
import chimp.client.notifications.ServerNotification
import chimp.client.transport.ox.OxClientHttpTransport
import chimp.protocol.{Implementation, ToolContent}
import io.circe.Json
import ox.supervised
import sttp.client4.DefaultSyncBackend
import sttp.model.Uri.UriContext
import sttp.shared.Identity

@main def streamingClient(): Unit =
  supervised:
    val backend = DefaultSyncBackend()
    val transport = OxClientHttpTransport(backend, uri"http://localhost:8080/mcp")
    val client = McpClient.bidirectional[Identity](transport, Implementation("my-client", "0.1.0"))

    client.onServerNotification:
      case ServerNotification.Progress(params)       => println(s"progress: ${params.progress}")
      case ServerNotification.LoggingMessage(params) => println(s"log: ${params.data}")
      case _                                         => ()

    val result = client.callTool("work", Json.obj("steps" -> Json.fromInt(3)))
    val _ = result.content.collect { case ToolContent.Text(_, text) => println(text) }

    client.close()
    backend.close()
```

## Documentation

Full documentation is available at **[chimp.softwaremill.com](https://chimp.softwaremill.com/)**.

## Contributing

Contributions are welcome! Please open issues or pull requests.

## Commercial Support

We offer commercial support for Chimp and related technologies, as well as development
services. [Contact us](https://softwaremill.com) to learn more about our offer!

## Copyright

Copyright (C) 2025-2026 SoftwareMill [https://softwaremill.com](https://softwaremill.com).
