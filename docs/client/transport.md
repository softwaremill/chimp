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

## Protocol versions

The HTTP transports and `McpClient` each take a `protocolVersion`, which defaults to `ProtocolVersion.LatestLegacy` (`2025-11-25`). Give both the same version.

- At a legacy version, the client runs the `initialize` handshake.
- At `ProtocolVersion.Latest` (`2026-07-28`), there is no handshake. The client calls `server/discover` to learn the server's capabilities and info, and fails if the server does not support its version. Every request then carries `_meta` with the protocol version, the client capabilities and the client info. The HTTP transports also send the `Mcp-Method` header, and `Mcp-Name` on `tools/call`, `prompts/get` and `resources/read`.

`2026-07-28` removed `ping`, `logging/setLevel` and `resources/{,un}subscribe`, so a modern server answers them with "method not found".

## Streaming integrations

The streaming transports have concrete implementations per effect system, in separate modules:

| Integration | Streaming HTTP | STDIO |
|---|---|---|
| ZIO | `ZioClientHttpTransport` | `ZioClientStdioTransport` |
| Ox (direct style) | `OxClientHttpTransport` | `OxClientStdioTransport` |
| Pekko | `PekkoClientHttpTransport` | `PekkoClientStdioTransport` |

## Backends

- **HTTP** transports run on any [sttp](https://sttp.softwaremill.com/en/latest/) backend. The streaming HTTP transports additionally require a backend with streaming capability.
- **STDIO** transports, on the other hand, can run using plain JDK components (synchronous), or using various libraries that support asynchronous streaming.

## Message size limits

The **STDIO** transports bound a single line read from the subprocess with `maxLineLength`, 10 MB by default. A longer line closes the transport: the reader stops and the requests waiting for a response fail.

```scala mdoc:compile-only
import chimp.client.*
import chimp.client.transport.ClientStdioTransport
import chimp.protocol.Implementation
import sttp.shared.Identity

object BoundedStdioClient:
  def main(args: Array[String]): Unit =
    val transport = ClientStdioTransport(command = List("my-mcp-server"), maxLineLength = 4 * 1024 * 1024)
    val client = McpClient[Identity](transport, Implementation("my-client", "0.1.0"))

    client.close()
```
