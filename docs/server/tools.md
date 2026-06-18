# Tools

- Use `tool(name)` to start defining a [tool](https://modelcontextprotocol.io/specification/2025-11-25/server/tools).
- Add a description and annotations for metadata and hints.
- Specify the input type (must have a Circe `Codec` and Tapir `Schema`), or use `.inputJson(schema)` for a raw JSON Schema.
- Provide the server logic:
  - `handle` — synchronous logic from input to `ToolResult`.
  - `handleWithHeaders` — synchronous logic that also receives the request headers.
  - `serverLogic` — effectful logic, with access to the [server context](capabilities.md) and headers.
- Assemble tools into an `McpServer` and call `.endpoint(path)` to create a Tapir endpoint.

```scala mdoc:compile-only
import chimp.server.*
import io.circe.Codec
import sttp.tapir.*

case class AddInput(a: Int, b: Int) derives Codec, Schema

val adder = tool("adder")
  .description("Adds two numbers")
  .withAnnotations(ToolAnnotations(idempotentHint = Some(true)))
  .input[AddInput]
  .handle(in => ToolResult.text(s"The result is ${in.a + in.b}"))

val endpoint = McpServer(tools = List(adder)).endpoint(List("mcp"))
```

A `ToolResult` can carry text, images, audio, embedded resources and structured output — see its constructors (`text`, `image`, `audio`, `embedded`, `structured`).
