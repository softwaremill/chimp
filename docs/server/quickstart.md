# Quickstart

Chimp lets you expose MCP tools over a JSON-RPC HTTP API. Tool inputs are described with type-safe Scala types; the JSON schema and JSON-RPC plumbing are generated for you.

Add the dependency to your `build.sbt`:

```scala
libraryDependencies += "com.softwaremill.chimp" %% "chimp-server" % "0.2.0"
```

## Example: the simplest MCP server

Below is a self-contained, [scala-cli](https://scala-cli.virtuslab.org)-runnable example:

```scala
//> using dep com.softwaremill.chimp::chimp-server:0.2.0

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

More runnable examples live in [`examples/`](https://github.com/softwaremill/chimp/tree/master/examples/src/main/scala/examples).
