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

The transports have no protocol version parameter. You set the version on `McpClient`, and the client gives it to the transport (see [Protocol versions](protocol.md)).

At the stateless version `2026-07-28`, the HTTP transports:

- send `MCP-Protocol-Version` equal to the version in the `_meta` of the request
- send the `Mcp-Method` header, and `Mcp-Name` on `tools/call`, `prompts/get` and `resources/read`
- mirror tool arguments into `Mcp-Param-*` headers (see below)
- open no GET stream for server messages

Legacy requests do not get these headers, and the STDIO transports send no headers.

### Headers from tool arguments

A tool can mark a property of its input schema with `x-mcp-header`. A `tools/list` result shows the HTTP transport these annotations. When you then call that tool, the transport sends each annotated argument in the header `Mcp-Param-<name>`:

| Argument | Header value |
|---|---|
| a string of printable ASCII, without leading or trailing whitespace | the string |
| any other string | `=?base64?<Base64 of the UTF-8 bytes>?=` |
| a boolean | `true` or `false` |
| an integer | its JSON text |
| `null`, or no argument | no header |

`Mcp-Name` uses the same rule for strings. Call `listTools` before `callTool`: the transport knows the annotations of a tool only from a `tools/list` result.

The transport removes a tool from the `tools/list` result, and logs a warning, when its `x-mcp-header` annotations are invalid: a value that is empty, not a string or not an HTTP token (for example, with spaces, colons, non-ASCII or control characters), the same name on two properties (names are case-insensitive), or an annotation on a property that is not a string, integer or boolean, or that is not reachable through `properties` keys only. The other tools stay in the result.

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
