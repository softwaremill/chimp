# Transport

A transport carries JSON-RPC messages between the client and the server. There are two families:

- **Unidirectional** (`Transport[F]`) — the client sends a message and optionally gets a response back. Enough for calling tools, listing resources, etc.
- **Bidirectional** (`BidirectionalTransport[F]`) — additionally lets the server push messages to the client (server-initiated requests and notifications). Required for [client capabilities](capabilities.md).

The streaming transports are abstract; their concrete, effect-specific implementations live in separate modules (e.g. ZIO).

```{mermaid}
classDiagram
    class Transport~F~ {
        <<trait>>
        +send(msg) Option~Message~
        +close()
    }
    class BidirectionalTransport~F~ {
        <<trait>>
        +onIncoming(handler)
    }
    class HttpTransport~F~
    class StdioTransport
    class StreamingHttpTransport~F, S~ {
        <<abstract>>
    }
    class StreamingStdioTransport~F~ {
        <<abstract>>
    }

    Transport <|-- BidirectionalTransport
    Transport <|-- HttpTransport
    BidirectionalTransport <|-- StdioTransport
    BidirectionalTransport <|-- StreamingHttpTransport
    BidirectionalTransport <|-- StreamingStdioTransport
```

## Backends

- **HTTP** transports run on any [sttp](https://sttp.softwaremill.com/en/latest/) backend. The streaming HTTP transports additionally require a backend with streaming capability.
- **STDIO** transports, on the other hand, can run using plain JDK components (synchronous), or using various libraries that support asynchronous streaming.
