# Chimp MCP Server

[![CI](https://github.com/softwaremill/chimp/actions/workflows/ci.yml/badge.svg)](https://github.com/softwaremill/chimp/actions/workflows/ci.yml)
[![Scala 3](https://img.shields.io/badge/scala-3.3.6-blue.svg)](https://www.scala-lang.org/)

A library for building [MCP](#mcp-protocol) (Model Context Protocol) servers in Scala 3, based on [Tapir](https://tapir.softwaremill.com/). Describe MCP tools with type-safe input, expose them over a JSON-RPC HTTP API. 

Integrates with any Scala stack, using any of the HTTP server implementations supported by Tapir.

---

## Quickstart

Add the dependency to your `build.sbt`:

```scala
libraryDependencies += "com.softwaremill.chimp" %% "core" % "0.0.0"
```

### Example: Defining and Running an MCP Server

```scala
import chimp.*
import sttp.tapir.server.netty.sync.NettySyncServer
import io.circe.Codec

// Define the input type for your tool
def case class Input(a: Int, b: Int) derives Codec, Schema

@main def mcpApp(): Unit =
  // Define a tool with a name, description, and input type
  val adderTool = tool("adder")
    .description("Adds two numbers")
    .withAnnotations(ToolAnnotations(idempotentHint = Some(true)))
    .input[Input]

  // Provide the logic for the tool
  def logic(i: Input): Either[String, String] =
    Right(s"The result is ${i.a + i.b}")

  // Combine the tool with its logic
  val adderServerTool = adderTool.handle(logic)

  // Create the MCP server endpoint
  val mcpServerEndpoint = mcpEndpoint(List(adderServerTool)).prependSecurityIn("jsonrpc")

  // Start the server on port 8080
  NettySyncServer()
    .port(8080)
    .addEndpoint(mcpServerEndpoint)
    .startAndWait()
```

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