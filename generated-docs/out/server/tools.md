# Tools

- Use `tool(name)` to start defining a [tool](https://modelcontextprotocol.io/specification/2025-11-25/server/tools).
- Add a description and annotations for metadata and hints.
- Specify the input type (must have a Circe `Codec` and Tapir `Schema`), or use `.inputJson(schema)` for a raw JSON Schema.
- Optionally specify the structured output type (must have a Tapir `Schema`), or use `.outputJson(schema)` for a raw JSON Schema.
- Provide the server logic:
  - `handle` — synchronous logic from input to `ToolResult`.
  - `handleWithHeaders` — synchronous logic that also receives the request headers.
  - `serverLogic` — effectful logic, with the request headers.

  A tool that pushes to the client while running (progress, logging) instead uses `streamingServerLogic` — see [server capabilities](capabilities.md).
- Assemble tools into an `McpServer` and call `.endpoint(path)` to create a Tapir endpoint.

```scala
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

## Structured output

A tool that returns structured data can specify its type, which is advertised to clients as the tool's `outputSchema` — the contract for the
`structuredContent` of each result:

```scala
import chimp.server.*
import io.circe.Codec
import sttp.tapir.*

case class SumInput(a: Int, b: Int) derives Codec, Schema
case class SumOutput(sum: Int) derives Codec, Schema

val adder = tool("adder")
  .description("Adds two numbers")
  .input[SumInput]
  .output[SumOutput]
  .handle(in => ToolResult.text(s"The result is ${in.a + in.b}").withStructured(SumOutput(in.a + in.b)))
```

The output type carries into the logic, which returns a `ToolResult[SumOutput]`; `ToolResult.error` is accepted whatever the output type. A
result with structured output but no content is also sent as serialized JSON text, as the spec recommends.
