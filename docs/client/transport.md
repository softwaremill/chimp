# Transport

A transport carries JSON-RPC messages between the client and the server. There are two families:

- **Unidirectional** (`ClientTransport[F]`) — the client sends a message and optionally gets a response back. Enough for calling tools, listing resources, etc.
- **Bidirectional** (`ClientBidirectionalTransport[F]`) — additionally lets the server push messages to the client (server-initiated requests and notifications). Required for [client capabilities](capabilities.md).

The streaming transports are abstract; their concrete, effect-specific implementations live in separate modules (e.g. ZIO and Ox).

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
