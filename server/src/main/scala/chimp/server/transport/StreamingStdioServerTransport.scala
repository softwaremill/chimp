package chimp.server.transport

/** Abstract base for streaming stdio MCP server transports that should consume the stdin as an asynchronous stream.
 */
abstract class StreamingStdioServerTransport[F[_]] extends StreamingServerTransport[F, F[Unit]]
