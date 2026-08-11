# Transport

A transport carries JSON-RPC messages between the client and the server. There are two families:

- **Unidirectional** (`ClientTransport[F]`) — the client sends a message and optionally gets a response back. Enough for calling tools, listing resources, etc.
- **Bidirectional** (`ClientBidirectionalTransport[F]`) — additionally lets the server push messages to the client (server-initiated requests and notifications). Required for [client capabilities](capabilities.md).

The streaming transports are abstract; their concrete, effect-specific implementations live in separate modules (e.g. ZIO).

```{mermaid}
classDiagram
    class ClientTransport~F~ {
        <<trait>>
        +send(msg) Option~Message~
        +close()
    }
    class ClientBidirectionalTransport~F~ {
        <<trait>>
        +onIncoming(handler)
    }
    class ClientHttpTransport~F~
    class ClientStdioTransport
    class ClientStreamingHttpTransport~F, S~ {
        <<abstract>>
    }
    class ClientStreamingStdioTransport~F~ {
        <<abstract>>
    }

    ClientTransport <|-- ClientBidirectionalTransport
    ClientTransport <|-- ClientHttpTransport
    ClientBidirectionalTransport <|-- ClientStdioTransport
    ClientBidirectionalTransport <|-- ClientStreamingHttpTransport
    ClientBidirectionalTransport <|-- ClientStreamingStdioTransport
```

## Streaming integrations

The streaming transports have concrete implementations per effect system, in separate modules:

| Integration | Streaming HTTP | STDIO |
|---|---|---|
| ZIO | `ZioClientHttpTransport` | `ZioClientStdioTransport` |
| Ox (direct style) | `OxClientHttpTransport` | `OxClientStdioTransport` |

The Ox implementations are direct-style (`F = Identity`). As sttp has no `StreamBackend` for Ox streams, `OxClientHttpTransport` extends `ClientBidirectionalTransport` directly: it runs on a plain `SyncBackend` and consumes Server-Sent Event responses by reading the response body as an `InputStream`, draining it on Ox forks.

## Backends

- **HTTP** transports run on any [sttp](https://sttp.softwaremill.com/en/latest/) backend. The streaming HTTP transports additionally require a backend with streaming capability.
- **STDIO** transports, on the other hand, can run using plain JDK components (synchronous), or using various libraries that support asynchronous streaming.

## Custom headers

Each HTTP transport has a `headers` parameter. The transport sends these headers with all of its requests to the server:

- the POST requests that carry the JSON-RPC messages,
- the GET request that opens the Server-Sent Event stream,
- the DELETE request that ends the session.

Use the parameter if the server needs an `Authorization` header. The protocol tells the client to send this header with each HTTP request, also for the requests of one session.

```scala mdoc:compile-only
import chimp.client.transport.ClientHttpTransport
import sttp.client4.DefaultSyncBackend
import sttp.model.Header
import sttp.model.Uri.UriContext
import sttp.shared.Identity

val backend = DefaultSyncBackend()
val transport = ClientHttpTransport[Identity](
  backend,
  uri"http://localhost:8080/mcp",
  headers = List(Header.authorization("Bearer", "my-token"))
)
```

The streaming transports have the same parameter:

```scala mdoc:compile-only
import chimp.client.transport.ox.OxClientHttpTransport
import ox.supervised
import sttp.client4.DefaultSyncBackend
import sttp.model.Header
import sttp.model.Uri.UriContext

supervised:
  val backend = DefaultSyncBackend()
  val transport = OxClientHttpTransport(
    backend,
    uri"http://localhost:8080/mcp",
    headers = List(Header.authorization("Bearer", "my-token"))
  )
  transport.close()
```

A header that you give replaces the header with the same name that the transport makes. Do not use the parameter for the protocol headers: `Accept`, `Content-Type`, `MCP-Protocol-Version` and `Mcp-Session-Id`. The transport controls these headers.

The STDIO transports have no headers. Give the credentials in the environment of the server process instead.
