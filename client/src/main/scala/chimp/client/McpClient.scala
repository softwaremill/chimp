package chimp.client

import chimp.client.notifications.ServerNotificationListener
import chimp.client.transport.{BidirectionalTransport, Transport}
import chimp.protocol.*
import io.circe.Json

trait McpClient[F[_]]:
  def ping(): F[Unit]
  def close(): F[Unit]

  def serverCapabilities: ServerCapabilities
  def serverInfo: Implementation

  def listTools(cursor: Option[Cursor] = None): F[ListToolsResponse]
  def callTool(name: String, arguments: Json): F[CallToolResult]

  def listPrompts(cursor: Option[Cursor] = None): F[ListPromptsResult]
  def getPrompt(name: String, arguments: Map[String, String] = Map.empty): F[GetPromptResult]

  def listResources(cursor: Option[Cursor] = None): F[ListResourcesResult]
  def listResourceTemplates(cursor: Option[Cursor] = None): F[ListResourceTemplatesResult]
  def readResource(uri: String): F[ReadResourceResult]

  def complete(ref: CompleteRef, argument: CompleteArgument): F[CompleteResult]

  def setLoggingLevel(level: LoggingLevel): F[Unit]

  def sendProgress(token: ProgressToken, progress: Double, total: Option[Double] = None, message: Option[String] = None): F[Unit]
  def sendCancelled(requestId: RequestId, reason: Option[String] = None): F[Unit]

trait BidirectionalMcpClient[F[_]] extends McpClient[F]:
  def subscribeResource(uri: String): F[Unit]
  def unsubscribeResource(uri: String): F[Unit]
  def sendRootsListChanged(): F[Unit]
  def onServerNotification(listener: ServerNotificationListener[F]): F[Unit]

object McpClient:
  def apply[F[_]](
      transport: Transport[F],
      clientInfo: Implementation,
      protocolVersion: ProtocolVersion
  ): F[McpClient[F]] =
    McpClientImpl.create(transport, clientInfo, protocolVersion)

  def apply[F[_]](
      transport: BidirectionalTransport[F],
      clientInfo: Implementation,
      rootsHandler: Option[() => F[ListRootsResult]] = None,
      samplingHandler: Option[CreateMessageRequest => F[CreateMessageResult]] = None,
      elicitationHandler: Option[ElicitRequest => F[ElicitResult]] = None,
      protocolVersion: ProtocolVersion = ProtocolVersion.Latest
  ): F[BidirectionalMcpClient[F]] =
    McpClientImpl.createBidirectional(transport, clientInfo, protocolVersion, rootsHandler, samplingHandler, elicitationHandler)
