package chimp.server.transport

/** Abstract base for streaming stdio MCP server transports that should consume the stdin as an asynchronous stream. Concrete
 * implementations live in effect-specific modules.
 */
abstract class StreamingStdioServerTransport[F[_]] extends StreamingServerTransport[F, F[Unit]]
