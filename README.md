# Chimp MCP Server

[![CI](https://github.com/softwaremill/chimp/actions/workflows/ci.yml/badge.svg)](https://github.com/softwaremill/chimp/actions/workflows/ci.yml)
[![Scala 3](https://img.shields.io/badge/scala-3.3.6-blue.svg)](https://www.scala-lang.org/)

A library for building [MCP](#mcp-protocol) (Model Context Protocol) servers in Scala 3, based on [Tapir](https://tapir.softwaremill.com/). Describe MCP tools with type-safe input, expose them over a JSON-RPC HTTP API. 

Integrates with any Scala stack, using any of the HTTP server implementations supported by Tapir.

---

## Quickstart

Add the dependency to your `build.sbt`:

```scala
libraryDependencies += "com.softwaremill.chimp" %% "core" % "0.1.1"
```

### Example: the simplest MCP server

Below is a self-contained, [scala-cli](https://scala-cli.virtuslab.org)-runnable example:

```scala
//> using dep com.softwaremill.chimp::core:0.1.1

import chimp.*
import sttp.tapir.*
import sttp.tapir.server.netty.sync.NettySyncServer

// define the input type for your tool
case class AdderInput(a: Int, b: Int) derives io.circe.Codec, Schema

@main def mcpApp(): Unit =
  // describe the tool providing the name, description, and input type
  val adderTool = tool("adder").description("Adds two numbers").input[AdderInput]

  // combine the tool description with the server-side logic
  val adderServerTool = adderTool.handle(i => Right(s"The result is ${i.a + i.b}"))

  // create the MCP server endpoint; it will be available at http://localhost:8080/mcp  
  val mcpServerEndpoint = mcpEndpoint(List(adderServerTool), List("mcp"))

  // start the server
  NettySyncServer().port(8080).addEndpoint(mcpServerEndpoint).startAndWait()
```

### More examples

Are available [here](https://github.com/softwaremill/chimp/tree/master/examples/src/main/scala/chimp).

---

## MCP Protocol

Chimp implements the HTTP transport of the [MCP protocol](https://modelcontextprotocol.io/specification/2025-03-26) (version **2025-03-26**). Only tools are supported, via the following JSON-RPC commands:

- Initialization and capabilities negotiation (`initialize`)
- Listing available tools (`tools/list`)
- Invoking a tool (`tools/call`)

All requests and responses use JSON-RPC 2.0. Tool input schemas are described using JSON Schema, auto-generated from Scala types.

---

## Defining Tools and Server Logic

- Use `tool(name)` to start defining a tool.
- Add a description and annotations for metadata and hints.
- Specify the input type (must have a Circe `Codec` and Tapir `Schema`).
- Provide the server logic as a function from input to `Either[String, String]` (or a generic effect type).
- Create a Tapir endpoint by providing your tools to `mcpEndpoint` 
- Start an HTTP server using your preferred Tapir server interpreter.

---

## Contributing

Contributions are welcome! Please open issues or pull requests.

---

## Commercial Support

We offer commercial support for Tapir and related technologies, as well as development services. [Contact us](https://softwaremill.com) to learn more about our offer!

---

## Copyright

Copyright (C) 2025 SoftwareMill [https://softwaremill.com](https://softwaremill.com).