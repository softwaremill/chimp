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

## Message size limits

The MCP specification sets no limit on the size of a message.

**STDIO** transports read newline-delimited messages and bound a single incoming line with `maxLineLength`, 10 MB by default. A longer line fails the serve loop with a `chimp.transport.McpLineTooLongException` on every backend:

```scala mdoc:compile-only
import chimp.server.*
import chimp.server.transport.ServerStdioTransport

object BoundedStdioServer:
  def main(args: Array[String]): Unit =
    val echo = tool("echo").input[String].handle(echo => ToolResult.text(echo))
    ServerStdioTransport(maxLineLength = 4 * 1024 * 1024).serve(McpServer(tools = List(echo)))
```

**HTTP** transports produce a Tapir endpoint, so the limit is Tapir's `maxRequestBodyLength`. A longer body gets `413 Payload Too Large`:

```scala mdoc:compile-only
import chimp.server.*
import sttp.tapir.server.model.EndpointExtensions.*
import sttp.tapir.server.netty.sync.NettySyncServer

object BoundedHttpServer:
  def main(args: Array[String]): Unit =
    val echo = tool("echo").input[String].handle(echo => ToolResult.text(echo))
    val mcpEndpoint = McpServer(tools = List(echo)).endpoint(List("mcp")).maxRequestBodyLength(4 * 1024 * 1024)

    NettySyncServer().port(8080).addEndpoint(mcpEndpoint).startAndWait()
```

Some server interpreters have a limit of their own, refer to their configuration if needed. For example Pekko HTTP `pekko.http.server.parsing.max-content-length` defaults to 8 MB.

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
