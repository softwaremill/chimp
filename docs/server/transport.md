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
| Ox (direct style) | `OxServerHttpTransport` | `OxServerStdioTransport` |
| Pekko | `PekkoServerHttpTransport` | `PekkoServerStdioTransport` |

## Backends

- **HTTP** transports produce a Tapir `ServerEndpoint` that you run on any Tapir server interpreter. The streaming HTTP transport additionally requires an interpreter with streaming capability.
- **STDIO** transports run the read/dispatch/write loop using plain JDK components (synchronous), or an effect's own semantics.

## Security

An HTTP transport produces a plain Tapir `ServerEndpoint`, so you can protect the MCP endpoint with Tapir's endpoint security. `prependSecurity` (or `prependSecurityPure`, if the check needs no effect) adds a security input and the logic which validates it. The logic runs before any MCP message is handled:

```scala mdoc:compile-only
import chimp.server.*
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.server.netty.sync.NettySyncServer

object SecuredMcpServer:
  def main(args: Array[String]): Unit =
    val adder = tool("echo").input[String].handle(echo => ToolResult.text(echo))
    val mcpEndpoint = McpServer(tools = List(adder)).endpoint(List("mcp"))

    val securedEndpoint = mcpEndpoint.prependSecurityPure(
      auth.bearer[String](),
      statusCode(StatusCode.Unauthorized).and(stringBody)
    )(token => if token == "s3cret" then Right(()) else Left("Invalid token"))

    NettySyncServer().port(8080).addEndpoint(securedEndpoint).startAndWait()
```

The result of the security logic does not reach the tool logic. If a tool needs data from the caller, read the request headers with `handleWithHeaders` (or `serverLogic` with headers).

For all the security inputs - API keys, basic and bearer authorization, OAuth2 flows - see the [Tapir endpoint security documentation](https://tapir.softwaremill.com/en/latest/endpoint/security.html).
