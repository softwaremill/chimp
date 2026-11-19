# Server capabilities

Most tools just answer a request, so `serverLogic`/`handle` expose no context. Use `serverLogicWithContext` to read the facts of the request (see [request facts](#request-facts)). A tool that needs to **push to the client while it runs** uses `streamingServerLogic`, which receives a `StreamingServerContext[F]`:

- `reportProgress` — [progress](https://modelcontextprotocol.io/specification/2025-11-25/basic/utilities/progress) notifications, auto-wired to the request's progress token.
- `log` — [logging](https://modelcontextprotocol.io/specification/2025-11-25/server/utilities/logging) notifications. For a modern (`2026-07-28`) request, chimp sends a message only when its level is at or above the [per-request log level](https://modelcontextprotocol.io/specification/2026-07-28/server/utilities/logging#per-request-log-level) in `_meta` (`io.modelcontextprotocol/logLevel`). If the request sets no level, chimp sends no messages. For a legacy request, chimp sends all messages; use `.withLoggingLevel` to receive `logging/setLevel`.

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

Server-wide capabilities are enabled by registering a handler — only what you wire up is advertised: `.withCompletion`, `.withLoggingLevel`, `.withSubscriptions`, `.withPublisher`. `.withLoggingLevel` and `.withSubscriptions` serve only legacy clients: `2026-07-28` removed `logging/setLevel` and `resources/{,un}subscribe`, so a modern client gets a `404` for them (see below). `.withPublisher` serves only modern clients (see [publishing list changes and resource updates](#publishing-list-changes-and-resource-updates)).

## Protocol versions

chimp is **dual-era**: it speaks the legacy handshake revisions (`2025-11-25` and earlier, negotiated via `initialize`) and the modern `2026-07-28` revision, where each request carries its protocol version in `_meta` under `io.modelcontextprotocol/protocolVersion` instead of a handshake.

The server answers `server/discover` automatically with its supported versions, capabilities and identity (`serverInfo` in `_meta`), so a client can learn them in one request without connecting. A request that declares a protocol version the server does not support is rejected with an `UnsupportedProtocolVersion` error (`-32022`) listing the supported versions, per the [versioning compatibility matrix](https://modelcontextprotocol.io/specification/2026-07-28/basic/versioning#compatibility-matrix). No configuration is required for any of this.

A request is **modern** when its `_meta` or its `MCP-Protocol-Version` header declares `2026-07-28`. A legacy client that writes a legacy version into `_meta` stays on the legacy rules. chimp checks every modern request, `server/discover` included, before any tool logic runs, and answers a failed check with a plain JSON error and its own HTTP status. A `server/discover` that declares no version (or a legacy version) gets no checks, so any client can discover the server:

| Problem | JSON-RPC error | HTTP status |
|---|---|---|
| unsupported protocol version | `-32022` UnsupportedProtocolVersion | `400` |
| `_meta` has no `protocolVersion` or no `clientCapabilities` | `-32602` InvalidParams | `400` |
| the `MCP-Protocol-Version` header is missing (HTTP only), or it and `_meta` declare different versions | `-32020` HeaderMismatch | `400` |
| `Mcp-Method` / `Mcp-Name` header missing or different from the body (HTTP only) | `-32020` HeaderMismatch | `400` |
| removed method (`initialize`, `ping`, `logging/setLevel`, `resources/subscribe`, `resources/unsubscribe`) or unknown method | `-32601` MethodNotFound | `404` |
| `subscriptions/listen` on a server without a publisher, or on the sync HTTP transport | `-32601` MethodNotFound | `404` |
| `tools/call` of a tool whose required client capabilities `_meta` does not declare | `-32021` MissingRequiredClientCapability | `400` |

Over HTTP, a modern request must carry the SEP-2243 standard headers: `Mcp-Method` with the body's `method`, and `Mcp-Name` with `params.name` on `tools/call` and `prompts/get`, or `params.uri` on `resources/read`. Header names are case-insensitive, values are case-sensitive, and surrounding whitespace is ignored. chimp's own client sends these headers and the `_meta` when you give it a modern protocol version — see [client transport](../client/transport.md).

## Request facts

Each handler can read the facts of its request from `ctx.request`, a `RequestInfo`:

- `protocolVersion` — the protocol version of the request.
- `clientCapabilities` and `clientInfo` — what the client declares in `_meta`.
- `logLevel` — the per-request log level from `_meta`.
- `progressToken` — the progress token from `_meta`.

A modern request carries all of these in `_meta`. The server keeps no state between requests, so a legacy request gives only the protocol version (from the `MCP-Protocol-Version` header or `_meta`) and the progress token. The other facts are `None`.

`serverLogicWithContext` gives the context to tools, prompts, resources and resource templates on every server. `streamingServerLogic` and `securedStreamingServerLogic` give it too, because `StreamingServerContext` is a `ServerContext`.

A tool that needs a client capability declares it with `withRequiredClientCapabilities`. If a modern request does not declare all of these capabilities in `_meta`, chimp rejects it before the logic runs, with `-32021` (MissingRequiredClientCapability), HTTP `400`, and the capabilities in `data.requiredCapabilities`. chimp compares the capabilities key by key, also in nested objects. A legacy request has no per-request capabilities, so chimp does not check it.

```scala mdoc:compile-only
import chimp.server.*
import chimp.protocol.ClientCapabilities
import io.circe.{Codec, Json}
import sttp.shared.Identity
import sttp.tapir.*

case class AskInput(question: String) derives Codec, Schema

val ask = tool("ask")
  .withRequiredClientCapabilities(ClientCapabilities(sampling = Some(Json.obj())))
  .input[AskInput]
  .serverLogicWithContext[Identity]: (in, ctx, _) =>
    val client = ctx.request.clientInfo.fold("unknown client")(_.name)
    ToolResult.text(s"$client asked: ${in.question}")

val server = McpServer[Identity]().addTool(ask)
```

## Publishing list changes and resource updates

A server tells its clients that its tools, prompts or resources changed, or that a resource was updated, through a `NotificationPublisher`. Create one, give it to the server with `.withPublisher`, and call it from any part of your application, not only from a tool:

- `toolsListChanged()`, `promptsListChanged()`, `resourcesListChanged()` — send the list-changed notification of that list.
- `resourceUpdated(uri)` — send `notifications/resources/updated` for one resource URI.
- `close()` — end all open subscriptions, for example when the server shuts down.

The methods are thread-safe and return `Unit`, not an effect. In an effect system, wrap the call, for example `ZIO.succeed(publisher.toolsListChanged())`.

```scala mdoc:compile-only
import chimp.server.*
import chimp.server.ox.OxServerHttpTransport
import io.circe.Codec
import sttp.shared.Identity
import sttp.tapir.*

case class NoteInput(text: String) derives Codec, Schema

val publisher = NotificationPublisher()

val addNote = tool("add_note")
  .input[NoteInput]
  .handle: _ =>
    publisher.resourcesListChanged()
    ToolResult.text("note added")

val server = McpServer[Identity]().addTool(addNote).withPublisher(publisher).streaming
val endpoint = OxServerHttpTransport(List("mcp")).serve(server)
```

A `2026-07-28` client receives these notifications on a [`subscriptions/listen`](https://modelcontextprotocol.io/specification/2026-07-28/basic/patterns/subscriptions) stream:

- With a publisher, `server/discover` advertises `listChanged` for tools, prompts and resources, and `subscribe` for resources. The capability of a list is there only when the server has that feature, for example `prompts` only when the server has prompts. The `initialize` result for legacy clients does not change: it advertises no list changes, and `subscribe` only for `.withSubscriptions`.
- The first message on the stream is `notifications/subscriptions/acknowledged`. Its filter is the part of the requested filter that the server honours. The server sends no other notification types.
- Each message on the stream carries the id of the listen request in `_meta`, under `io.modelcontextprotocol/subscriptionId`.
- `notifications/resources/updated` goes only to the streams which list the URI in `resourceSubscriptions`.
- Progress and log notifications do not go on a listen stream. They stay on the stream of their request.
- Over HTTP, a subscription ends when the client closes the stream. The server notices a closed stream at the latest when it next sends on it.
- Over stdio, all subscriptions share the one channel. The client ends one with `notifications/cancelled` that names the listen request. When the input ends, the server ends each open subscription.
- When the server ends a subscription (on `close()`, or at the end of the stdio input), it sends a successful `subscriptions/listen` result with the subscription id. Over stdio, it then sends `notifications/cancelled` for the listen request.

```{note}
A listen stream needs a streaming transport. On the sync HTTP transport (`McpServer.endpoint`), the server answers `subscriptions/listen` with `-32601` and advertises no list changes.
```
