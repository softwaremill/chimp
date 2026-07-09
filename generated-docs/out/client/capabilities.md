# Client capabilities

Beyond calling tools, an MCP client can advertise capabilities that let the server interact with it. Chimp supports:

- [Roots](https://modelcontextprotocol.io/specification/2025-11-25/client/roots) — exposing the filesystem boundaries the client can operate in.
- [Sampling](https://modelcontextprotocol.io/specification/2025-11-25/client/sampling) — letting the server request an LLM completion through the client.
- [Elicitation](https://modelcontextprotocol.io/specification/2025-11-25/client/elicitation) — letting the server request additional input from the user.
- [Logging](https://modelcontextprotocol.io/specification/2025-11-25/server/utilities/logging) — receiving log messages forwarded by the server.
- [Notifications](https://modelcontextprotocol.io/specification/2025-11-25/basic/index#notifications) — receiving server-pushed events such as resource updates and list changes.

```{note}
All of these require the server to push messages to the client, so they only work over a **bidirectional, streaming transport** (e.g. `ZioClientHttpTransport`). They are unavailable on the plain `ClientHttpTransport`.
```

Create the client with `McpClient.bidirectional`, providing a handler for each capability you want to enable — only capabilities backed by a handler are advertised to the server:

```scala
import chimp.client.*
import chimp.client.transport.ClientBidirectionalTransport
import chimp.protocol.*
import zio.*

def connect(transport: ClientBidirectionalTransport[Task]): Task[BidirectionalMcpClient[Task]] =
  McpClient.bidirectional[Task](
    transport,
    clientInfo = Implementation("my-client", "0.1.0"),
    rootsHandler = Some(() => ZIO.succeed(ListRootsResult(roots = List(Root("file:///workspace", Some("workspace"))))))
    // samplingHandler = Some(...),
    // elicitationHandler = Some(...),
  )
```

Register a listener for server notifications with `onServerNotification`:

```scala
import chimp.client.*
import chimp.client.notifications.ServerNotification
import zio.*

def listen(client: BidirectionalMcpClient[Task]): Task[Unit] =
  client.onServerNotification {
    case ServerNotification.ResourceUpdated(params) => ZIO.logInfo(s"resource changed: ${params.uri}")
    case _                                          => ZIO.unit
  }
```
