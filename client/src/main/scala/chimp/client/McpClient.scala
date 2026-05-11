package chimp.client

import chimp.client.capabilities.CapsDerive
import chimp.client.notifications.ServerNotificationListener
import chimp.client.transport.Transport
import chimp.protocol.*
import io.circe.Json
import sttp.monad.MonadError

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

  def listPrompts(cursor: Option[Cursor] = None): F[ListPromptsResult]
  def getPrompt(name: String, arguments: Map[String, String] = Map.empty): F[GetPromptResult]

  def listResources(cursor: Option[Cursor] = None): F[ListResourcesResult]
  def listResourceTemplates(cursor: Option[Cursor] = None): F[ListResourceTemplatesResult]
  def readResource(uri: String): F[ReadResourceResult]
  def subscribeResource(uri: String): F[Unit]
  def unsubscribeResource(uri: String): F[Unit]

  def complete(ref: CompleteRef, argument: CompleteArgument): F[CompleteResult]

  def setLoggingLevel(level: LoggingLevel): F[Unit]

  def sendProgress(token: ProgressToken, progress: Double, total: Option[Double] = None, message: Option[String] = None): F[Unit]
  def sendCancelled(requestId: RequestId, reason: Option[String] = None): F[Unit]
  def sendRootsListChanged(): F[Unit]

  def onServerNotification(listener: ServerNotificationListener[F]): F[Unit]

object McpClient:
  def apply[F[_], Caps](
      transport: Transport[F],
      clientInfo: Implementation,
      protocolVersion: String = ProtocolVersion.Latest
  )(using d: CapsDerive[F, Caps]): McpClient[F, Caps] =
    given MonadError[F] = transport.monad
    DefaultMcpClient.create[F, Caps](transport, clientInfo, protocolVersion, d.wire, d.handlers)
