# Transport

A transport exposes an `McpServer` over a particular medium. `serve(server)` produces the transport-specific artifact `A` — a Tapir `ServerEndpoint` for HTTP, or a runnable loop for stdio. There are two families:

- **Unidirectional** (`ServerTransport[F, A]`) — request/response only. Enough for tools, resources, prompts, completion.
- **Bidirectional** (`StreamingServerTransport[F, A]`) — additionally lets the server push messages to the client (progress and logging notifications). Required for [streaming server capabilities](capabilities.md).

The streaming transports are abstract; their concrete, effect-specific implementations live in separate modules (e.g. ZIO).

```{mermaid}
classDiagram
    class ServerTransport~F, A~ {
        <<trait>>
        +serve(server) A
    }
    class StreamingServerTransport~F, A~ {
        <<trait>>
        +serve(server) A
    }
    class ServerHttpTransport~F~
    class ServerStdioTransport
    class ServerStreamingHttpTransport~F, S~ {
        <<abstract>>
    }
    class ServerStreamingStdioTransport~F~ {
        <<abstract>>
    }

    ServerTransport <|-- ServerHttpTransport
    ServerTransport <|-- ServerStdioTransport
    StreamingServerTransport <|-- ServerStdioTransport
    StreamingServerTransport <|-- ServerStreamingHttpTransport
    StreamingServerTransport <|-- ServerStreamingStdioTransport
```

`McpServer(...).endpoint(path)` is a shortcut for `ServerHttpTransport(path).serve(...)`.

## Streaming integrations

The streaming transports have concrete implementations per effect system, in separate modules:

| Integration | Streaming HTTP | STDIO |
|---|---|---|
| ZIO | `ZioServerHttpTransport` | `ZioServerStdioTransport` |

## Medium

- **HTTP** transports produce a Tapir `ServerEndpoint` that you run on any Tapir server interpreter. The streaming HTTP transport additionally requires an interpreter with streaming capability.
- **STDIO** transports run the read/dispatch/write loop using plain JDK components (synchronous), or an effect's own semantics (e.g. ZIO).
