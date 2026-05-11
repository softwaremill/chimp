package chimp.client

import chimp.client.internal.CapsBuilder
import chimp.client.transport.Transport
import chimp.protocol.*
import io.circe.Json

/** An MCP client. The `Caps` type parameter is an intersection of capability typeclasses (`Roots[F]`, `Sampling[F]`, `Elicitation[F]`)
  * the host application opts into; each one is advertised on `initialize` and routed to the corresponding `given` handler when the
  * server invokes it.
  */
trait McpClient[F[_], +Caps]:
  def initialize(): F[InitializeResult]
  def ping(): F[Unit]
  def close(): F[Unit]

  def listTools(cursor: Option[Cursor] = None): F[ListToolsResponse]
  def callTool(name: String, arguments: Json): F[CallToolResult]

object McpClient:
  inline def apply[F[_], Caps](
      transport: Transport[F],
      clientInfo: Implementation,
      protocolVersion: String = ProtocolVersion.Latest
  ): McpClient[F, Caps] =
    val wire = CapsBuilder.wire[F, Caps]
    DefaultMcpClient.create[F, Caps](transport, clientInfo, protocolVersion, wire)
