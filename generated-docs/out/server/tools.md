# Tools

- Use `tool(name)` to start defining a [tool](https://modelcontextprotocol.io/specification/2025-11-25/server/tools).
- Add a description and annotations for metadata and hints.
- Specify the input type (must have a Circe `Codec` and Tapir `Schema`), or use `.inputJson(schema)` for a raw JSON Schema.
- Optionally declare the structured output type with `.output[O]` (needs a Tapir `Schema`), or `.outputJson(schema)` for a raw JSON
  Schema. This is advertised as the tool's `outputSchema`, so clients can validate the `structuredContent` it returns.
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

A tool declaring its output type advertises the derived JSON Schema as `outputSchema`, so the `structuredContent` of a result becomes a
contract clients can validate against:

```scala
import chimp.server.*
import io.circe.Codec
import io.circe.syntax.*
import sttp.tapir.*

case class SumInput(a: Int, b: Int) derives Codec, Schema
case class SumOutput(sum: Int) derives Codec, Schema

val adder = tool("adder")
  .description("Adds two numbers")
  .input[SumInput]
  .output[SumOutput]
  .handle(in => ToolResult.text(s"The result is ${in.a + in.b}").withStructured(SumOutput(in.a + in.b).asJson))
```

Both content and structured output are returned above; the text is for clients that don't read `structuredContent`. A result carrying only
structured output can be built with `ToolResult.structured(value)`.
