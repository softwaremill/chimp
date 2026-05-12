package chimp.client

import chimp.client.notifications.ServerNotificationListener
import chimp.client.transport.Transport
import chimp.protocol.*
import io.circe.Json

/** An MCP client.
  *
  * Server-initiated requests (`roots/list`, `sampling/createMessage`, `elicitation/create`) are dispatched to the optional
  * handler functions supplied at construction. Each handler that is `Some` causes the corresponding capability to be advertised
  * on `initialize`; capabilities the host application doesn't opt into are answered with `MethodNotFound`.
  */
trait McpClient[F[_]]:
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
  def apply[F[_]](
      transport: Transport[F],
      clientInfo: Implementation,
      rootsHandler: Option[() => F[ListRootsResult]] = None,
      samplingHandler: Option[CreateMessageRequest => F[CreateMessageResult]] = None,
      elicitationHandler: Option[ElicitRequest => F[ElicitResult]] = None,
      protocolVersion: String = ProtocolVersion.Latest
  ): McpClient[F] =
    DefaultMcpClient.create(
      transport,
      clientInfo,
      protocolVersion,
      rootsHandler,
      samplingHandler,
      elicitationHandler
    )
