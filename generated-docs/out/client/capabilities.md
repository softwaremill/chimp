# Client capabilities

Beyond calling tools, an MCP client can advertise capabilities that let the server interact with it. Chimp supports:

- [Roots](https://modelcontextprotocol.io/specification/2025-11-25/client/roots) — exposing the filesystem boundaries the client can operate in.
- [Sampling](https://modelcontextprotocol.io/specification/2025-11-25/client/sampling) — letting the server request an LLM completion through the client.
- [Elicitation](https://modelcontextprotocol.io/specification/2025-11-25/client/elicitation) — letting the server request additional input from the user.
- [Logging](https://modelcontextprotocol.io/specification/2025-11-25/server/utilities/logging) — receiving log messages forwarded by the server.
- [Notifications](https://modelcontextprotocol.io/specification/2025-11-25/basic/index#notifications) — receiving server-pushed events such as resource updates and list changes.

```{note}
All of these require the server to push messages to the client, so they only work over a **bidirectional, streaming transport** (e.g. `ZioStreamingHttpTransport`). They are unavailable on the plain `HttpTransport`.
```

Create the client with `McpClient.bidirectional`, providing a handler for each capability you want to enable — only capabilities backed by a handler are advertised to the server:

```scala
val client = McpClient.bidirectional[Task](
  transport,
  clientInfo = Implementation("my-client", "0.1.0"),
  rootsHandler = Some(() => ZIO.succeed(ListRootsResult(roots = List(Root("file:///workspace", Some("workspace")))))),
  // samplingHandler = Some(...),
  // elicitationHandler = Some(...),
)
```

Register a listener for server notifications with `onServerNotification`:

```scala
client.onServerNotification {
  case ServerNotification.ResourceUpdated(uri) => ZIO.logInfo(s"resource changed: $uri")
  case _                                       => ZIO.unit
}
```
