package chimp.server.transport

import chimp.server.{McpServer, StreamingMcpServer}

/** A unidirectional MCP server transport. 
  * Binds a server definition producing the transport-specific medium - an endpoint for HTTP or runnable loop for stdio.
  */
trait ServerTransport[F[_], A]:
  def serve(server: McpServer[F]): A

/** A bidirectional MCP server transport that can push messages to the clients.
  * Binds a server definition producing the transport-specific medium - an streamable endpoint for HTTP or runnable loop for stdio.
  */
trait StreamingServerTransport[F[_], A]:
  def serve(server: StreamingMcpServer[F]): A
