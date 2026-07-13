# Chimp MCP

[![CI](https://github.com/softwaremill/chimp/actions/workflows/ci.yml/badge.svg)](https://github.com/softwaremill/chimp/actions/workflows/ci.yml)
[![Scala 3](https://img.shields.io/badge/scala-3-blue.svg)](https://www.scala-lang.org/)

An SDK for building [MCP](https://modelcontextprotocol.io/specification) (Model Context Protocol) servers and
clients in Scala 3 using boilerplate-less, type-safe APIs based on [Tapir](https://tapir.softwaremill.com/)
and [sttp](https://github.com/softwaremill/sttp), supporting the variety of the Scala ecosystem. Both servers
and clients can communicate over **Streamable HTTP** or **stdio**.

### Quickstart

Run a basic MCP server with Netty exposing a simple _adder_ tool:

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

## Documentation

Full documentation is available at **[chimp.softwaremill.com](https://chimp.softwaremill.com/)**.

## Contributing

Contributions are welcome! Please open issues or pull requests.

## Commercial Support

We offer commercial support for Chimp and related technologies, as well as development
services. [Contact us](https://softwaremill.com) to learn more about our offer!

## Copyright

Copyright (C) 2025-2026 SoftwareMill [https://softwaremill.com](https://softwaremill.com).
