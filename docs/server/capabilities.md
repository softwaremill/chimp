# Server capabilities

Tool logic runs with a **server context**. There are two:

- `ServerContext[F]` — available on every server; exposes cancellation observation (`isCancelled`, `onCancel`).
- `StreamingServerContext[F]` — extends it with server→client interactions emitted while a tool runs:
  - `reportProgress` — [progress](https://modelcontextprotocol.io/specification/2025-11-25/basic/utilities/progress) notifications, auto-wired to the request's progress token.
  - `log` — [logging](https://modelcontextprotocol.io/specification/2025-11-25/server/utilities/logging) notifications.
  - `sample` / `elicit` — server-initiated [sampling](https://modelcontextprotocol.io/specification/2025-11-25/client/sampling) and [elicitation](https://modelcontextprotocol.io/specification/2025-11-25/client/elicitation) requests (planned).

```{note}
Pushing to the client requires an open stream, so `StreamingServerContext` is only available over a **streaming transport**. A tool that uses it is registered with `addStreamingTool` on a `StreamingMcpServer`, and will not compile on the plain request/response endpoint.
```

Use `serverLogic` for a tool that only needs the base context, and `streamingServerLogic` for one that pushes to the client:

```scala mdoc:compile-only
import chimp.server.*
import chimp.protocol.LoggingLevel
import io.circe.{Codec, Json}
import sttp.shared.Identity
import sttp.tapir.*

case class WorkInput(steps: Int) derives Codec, Schema

val work = tool("work")
  .input[WorkInput]
  .streamingServerLogic[Identity]: (_, ctx, _) =>
    ctx.reportProgress(0.5, total = Some(1.0))
    ctx.log(LoggingLevel.Info, Json.fromString("halfway"))
    ToolResult.text("done")

val server = StreamingMcpServer[Identity]().addStreamingTool(work)
```

Server-wide capabilities are enabled by registering a handler — only what you wire up is advertised: `.withCompletion`, `.withLoggingLevel`, `.withSubscriptions`.
