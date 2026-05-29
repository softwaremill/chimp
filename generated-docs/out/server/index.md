# Server

Chimp lets you expose MCP tools over a JSON-RPC HTTP API. Tool inputs are described with type-safe Scala types; the JSON schema and JSON-RPC plumbing are generated for you.

## Quickstart

Add the dependency to your `build.sbt`:

```scala
libraryDependencies += "com.softwaremill.chimp" %% "chimp-server" % "0.1.8"
```

### Example: the simplest MCP server

Below is a self-contained, [scala-cli](https://scala-cli.virtuslab.org)-runnable example:

```scala
//> using dep com.softwaremill.chimp::chimp-server:0.1.8

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

Available [here](https://github.com/softwaremill/chimp/tree/master/examples/src/main/scala/chimp).

## MCP Protocol

Chimp implements the HTTP transport of the [MCP protocol](https://modelcontextprotocol.io/specification/2025-03-26) (version **2025-03-26**). Only tools are supported, via the following JSON-RPC commands:

- Initialization and capabilities negotiation (`initialize`)
- Listing available tools (`tools/list`)
- Invoking a tool (`tools/call`)

All requests and responses use JSON-RPC 2.0. Tool input schemas are described using JSON Schema, auto-generated from Scala types.

## Defining tools and server logic

- Use `tool(name)` to start defining a tool.
- Add a description and annotations for metadata and hints.
- Specify the input type (must have a Circe `Codec` and Tapir `Schema`).
- Provide the server logic as a function from input to `Either[String, String]` (or a generic effect type).
  - Use `handle` to connect the tool definition with the server logic when the use of headers is not required.
  - Use `handleWithHeaders` to connect the tool definition with the server logic when headers are required.
- Create a Tapir endpoint by providing your tools to `mcpEndpoint`.
- Start an HTTP server using your preferred Tapir server interpreter.

## Using with ZIO

When using ZIO, you might have to explicitly state the effect type that you are using, as the Tapir-ZIO integration requires a `RIO[R, A]` effect (which is an alias for `ZIO[R, Throwable, A]`), for example:

```scala
val myServerTool = myTool.serverLogic[[X] =>> RIO[Any, X]]: (input, headers) =>
  ZIO.succeed(???)
```
