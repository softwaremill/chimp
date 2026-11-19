# Tools

- Use `tool(name)` to start defining a [tool](https://modelcontextprotocol.io/specification/2025-11-25/server/tools).
- Add a description and annotations for metadata and hints.
- Specify the input type (must have a Circe `Codec` and Tapir `Schema`), or use `.inputJson(schema)` for a raw JSON Schema.
- Optionally specify the structured output type (must have a Tapir `Schema`), or use `.outputJson(schema)` for a raw JSON Schema.
- Provide the server logic:
  - `handle` — synchronous logic from input to `ToolResult`.
  - `handleWithHeaders` — synchronous logic that also receives the request headers.
  - `serverLogic` — effectful logic, with the request headers.
  - `handleSecured` (synchronous) or `securedServerLogic` (effectful) — logic that also receives the principal made by the server's security logic; usable only on a secured server — see [transport security](transport.md).

  A tool that pushes to the client while running (progress, logging) instead uses `streamingServerLogic` — see [server capabilities](capabilities.md). A secured, streaming tool uses `securedStreamingServerLogic`, which gives both the principal and the streaming context — see [transport security](transport.md).
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

## Structured output

A tool that returns structured data can specify its type, which is advertised to clients as the tool's `outputSchema` — the contract for the
`structuredContent` of each result:

```scala mdoc:compile-only
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

A tool can also ask the client for input before it returns its result: see [asking the client for input](mrtr.md).

## Mirroring arguments into HTTP headers

A tool can mark an input property with [`x-mcp-header`](https://modelcontextprotocol.io/specification/2026-07-28/server/tools#x-mcp-header). A `2026-07-28` client on Streamable HTTP then also sends the argument in the `Mcp-Param-{name}` header, so that load balancers, proxies and gateways can route on it without reading the body. Use `withMcpHeader` with the property name, or with the chain of property names to a nested property:

```scala mdoc:compile-only
import chimp.server.*
import io.circe.Codec
import sttp.tapir.*

case class QueryInput(region: String, query: String) derives Codec, Schema

val executeSql = tool("execute_sql")
  .description("Executes SQL in a region")
  .input[QueryInput]
  .withMcpHeader("region", "Region")
  .handle(in => ToolResult.text(s"Ran in ${in.region}: ${in.query}"))
```

The `inputSchema` of the tool then has `"x-mcp-header": "Region"` on the `region` property. `withMcpHeader` works the same for a raw JSON Schema given to `.inputJson(schema)`, and an `x-mcp-header` which is already in a raw JSON Schema is kept.

The server checks the `Mcp-Param-*` headers against the arguments, and rejects a mismatch with `-32020` — see [protocol versions](capabilities.md#protocol-versions).

The tool builder (`input`, `inputJson` and `withMcpHeader`) throws an `IllegalArgumentException` when an annotation breaks a constraint of the spec:

- The header name is empty, is not an HTTP token (letters, digits and ``!#$%&'*+-.^_`|~``), or has control characters.
- Two properties of one tool use the same header name, ignoring case.
- The property type is not `string`, `integer` or `boolean`. A `number` property is not permitted.
- The property is not reachable from the schema root through `properties` keys only. For example, it is in `items`, in `anyOf`, or behind a `$ref`. A nested case class gets a `$ref` in a derived schema, so mark only the top-level fields of a derived schema, or use a raw JSON Schema.

```{warning}
Do not mark sensitive parameters, such as passwords, tokens or personal data, with `x-mcp-header`. HTTP headers are often written to logs and are visible to every intermediary on the way to the server.
```
