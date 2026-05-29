# Client

Chimp ships an MCP client that connects to any MCP-compliant server. The client is parameterised over an effect type `F[_]`, and is paired with a pluggable transport that carries JSON-RPC messages.

## Quickstart

Add the dependency to your `build.sbt`:

```scala
libraryDependencies += "com.softwaremill.chimp" %% "chimp-client" % "0.1.8"
```

For the ZIO streaming transport, also add:

```scala
libraryDependencies += "com.softwaremill.chimp" %% "chimp-client-zio" % "0.1.8"
```

## Two client flavours

- `McpClient[F]` — unidirectional. The client issues requests and receives responses; suitable for any `Transport[F]`.
- `BidirectionalMcpClient[F]` — extends `McpClient[F]` and additionally supports server-initiated interactions: resource subscriptions, roots-changed notifications, and notification listeners. Requires a `BidirectionalTransport[F]`.

Both are created via the `McpClient` companion object after the initialization handshake completes.

## Transports

| Transport | Sync (`F = Identity`) | Streaming |
|---|---|---|
| HTTP | `HttpTransport` | `StreamingHttpTransport` |
| stdio | `StdioTransport` | `StreamingStdioTransport` |

The streaming variants are bidirectional and unlock `BidirectionalMcpClient`. The sync variants are simpler and direct-style.

## Notifications

When using a `BidirectionalMcpClient`, register a `ServerNotificationListener` via `onServerNotification` to react to server-pushed events (resource updates, list-changed notifications, log messages).

## More examples

See the runnable examples under [`examples/`](https://github.com/softwaremill/chimp/tree/master/examples/src/main/scala/chimp).
