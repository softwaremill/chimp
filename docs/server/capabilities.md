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

Server-wide capabilities are enabled by registering a handler — only what you wire up is advertised: `.withCompletion`, `.withLoggingLevel`, `.withSubscriptions`. `.withLoggingLevel` and `.withSubscriptions` serve only legacy clients: `2026-07-28` removed `logging/setLevel` and `resources/{,un}subscribe`, so a modern client gets a `404` for them (see below).

## Protocol versions

chimp is **dual-era**: it speaks the legacy handshake revisions (`2025-11-25` and earlier, negotiated via `initialize`) and the modern `2026-07-28` revision, where each request carries its protocol version in `_meta` under `io.modelcontextprotocol/protocolVersion` instead of a handshake.

The server answers `server/discover` automatically with its supported versions, capabilities and identity (`serverInfo` in `_meta`), so a client can learn them in one request without connecting. A request that declares a protocol version the server does not support is rejected with an `UnsupportedProtocolVersion` error (`-32022`) listing the supported versions, per the [versioning compatibility matrix](https://modelcontextprotocol.io/specification/2026-07-28/basic/versioning#compatibility-matrix). No configuration is required for any of this.

A request is **modern** when its `_meta` or its `MCP-Protocol-Version` header declares `2026-07-28`. A legacy client that writes a legacy version into `_meta` stays on the legacy rules. chimp checks every modern request before any tool logic runs, and answers a failed check with a plain JSON error and its own HTTP status:

| Problem | JSON-RPC error | HTTP status |
|---|---|---|
| unsupported protocol version | `-32022` UnsupportedProtocolVersion | `400` |
| `_meta` has no `protocolVersion` or no `clientCapabilities` | `-32602` InvalidParams | `400` |
| the `MCP-Protocol-Version` header and `_meta` declare different versions | `-32020` HeaderMismatch | `400` |
| `Mcp-Method` / `Mcp-Name` header missing or different from the body (HTTP only) | `-32020` HeaderMismatch | `400` |
| removed method (`initialize`, `ping`, `logging/setLevel`, `resources/subscribe`, `resources/unsubscribe`) or unknown method | `-32601` MethodNotFound | `404` |

Over HTTP, a modern request must carry the SEP-2243 standard headers: `Mcp-Method` with the body's `method`, and `Mcp-Name` with `params.name` on `tools/call` and `prompts/get`, or `params.uri` on `resources/read`. Header names are case-insensitive, values are case-sensitive, and surrounding whitespace is ignored. chimp's own client sends these headers and the `_meta` when you give it a modern protocol version — see [client transport](../client/transport.md).
