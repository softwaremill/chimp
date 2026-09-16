# Server capabilities

Most tools just answer a request, so `serverLogic`/`handle` expose no context. A tool that needs to **push to the client while it runs** uses `streamingServerLogic`, which receives a `StreamingServerContext[F]`:

- `reportProgress` — [progress](https://modelcontextprotocol.io/specification/2025-11-25/basic/utilities/progress) notifications, auto-wired to the request's progress token.
- `log` — [logging](https://modelcontextprotocol.io/specification/2025-11-25/server/utilities/logging) notifications.

```{note}
Pushing to the client requires an open stream, so a `streamingServerLogic` tool is registered with `addStreamingTool` on a `StreamingMcpServer`, and will not compile on the plain request/response endpoint.
```

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

## Protocol versions

chimp is **dual-era**: it speaks the legacy handshake revisions (`2025-11-25` and earlier, negotiated via `initialize`) and the modern `2026-07-28` revision, where each request carries its protocol version in `_meta` under `io.modelcontextprotocol/protocolVersion` instead of a handshake.

The server answers `server/discover` automatically with its supported versions, capabilities and identity (`serverInfo` in `_meta`), so a client can learn them in one request without connecting. A request that declares a protocol version the server does not support is rejected with an `UnsupportedProtocolVersion` error (`-32022`) listing the supported versions, per the [versioning compatibility matrix](https://modelcontextprotocol.io/specification/2026-07-28/basic/versioning#compatibility-matrix). No configuration is required for any of this.
