package chimp.client

import chimp.client.notifications.ServerNotificationListener
import chimp.client.transport.{BidirectionalTransport, Transport}
import chimp.protocol.*
import io.circe.Json

/** A Model Context Protocol (MCP) client that has completed the initialization handshake with a server.
  *
  * Exposes the server's advertised capabilities and identity, and provides methods for sending client-initiated requests and notifications
  * over the underlying [[chimp.client.transport.Transport]].
  *
  * For bidirectional interaction (server-initiated requests, resource subscriptions, notification listeners), use
  * [[BidirectionalMcpClient]] instead.
  */
trait McpClient[F[_]]:
  /** Sends a `ping` request to the server. */
  def ping(): F[Unit]

  /** Closes the underlying transport and releases any resources held by this client. */
  def close(): F[Unit]

  /** Capabilities advertised by the server during the initialization handshake. */
  def serverCapabilities: ServerCapabilities

  /** Server implementation name and version, received during the initialization handshake. */
  def serverInfo: Implementation

  /** Lists tools exposed by the server.
    *
    * @param cursor
    *   Optional pagination cursor returned by a previous call; by default uses `None` and starts from the beginning.
    */
  def listTools(cursor: Option[Cursor] = None): F[ListToolsResponse]

  /** Invokes a tool on the server.
    *
    * @param name
    *   The tool's name, as reported by [[listTools]].
    * @param arguments
    *   JSON object containing the tool's input, matching its declared input schema.
    */
  def callTool(name: String, arguments: Json): F[CallToolResult]

  /** Lists prompt templates exposed by the server.
    *
    * @param cursor
    *   Optional pagination cursor returned by a previous call; by default uses `None` and starts from the beginning.
    */
  def listPrompts(cursor: Option[Cursor] = None): F[ListPromptsResult]

  /** Retrieves a prompt template by name, substituting the given arguments.
    *
    * @param name
    *   The prompt's name, as reported by [[listPrompts]].
    * @param arguments
    *   Values for the prompt template's arguments; defaults to empty.
    */
  def getPrompt(name: String, arguments: Map[String, String] = Map.empty): F[GetPromptResult]

  /** Lists resources exposed by the server.
    *
    * @param cursor
    *   Optional pagination cursor returned by a previous call; by default uses `None` and starts from the beginning.
    */
  def listResources(cursor: Option[Cursor] = None): F[ListResourcesResult]

  /** Lists resource templates exposed by the server.
    *
    * @param cursor
    *   Optional pagination cursor returned by a previous call; by default uses `None` and starts from the beginning.
    */
  def listResourceTemplates(cursor: Option[Cursor] = None): F[ListResourceTemplatesResult]

  /** Reads the contents of a resource identified by its URI. */
  def readResource(uri: String): F[ReadResourceResult]

  /** Requests completion suggestions for an argument of a prompt or resource template reference. */
  def complete(ref: CompleteRef, argument: CompleteArgument): F[CompleteResult]

  /** Sets the minimum severity level for log messages the server should forward to this client. */
  def setLoggingLevel(level: LoggingLevel): F[Unit]

  /** Sends a progress notification for a previously issued request that advertised a progress token.
    *
    * @param token
    *   The progress token associated with the in-flight request.
    * @param progress
    *   Current progress value; the unit is opaque and chosen by the sender.
    * @param total
    *   Optional total value, used by the receiver to compute a percentage.
    * @param message
    *   Optional human-readable description of the current step.
    */
  def sendProgress(token: ProgressToken, progress: Double, total: Option[Double] = None, message: Option[String] = None): F[Unit]

  /** Sends a `cancelled` notification, asking the server to stop processing a previously issued request.
    *
    * @param requestId
    *   Identifier of the request to cancel.
    * @param reason
    *   Optional human-readable explanation.
    */
  def sendCancelled(requestId: RequestId, reason: Option[String] = None): F[Unit]

/** An [[McpClient]] used over a [[chimp.client.transport.BidirectionalTransport]], which additionally supports server-initiated
  * interactions: subscribing to resource updates, notifying the server about changes to the client's roots, and handling notifications
  * pushed by the server.
  */
trait BidirectionalMcpClient[F[_]] extends McpClient[F]:
  /** Subscribes to updates for the resource identified by the given URI. The server will emit `resources/updated` notifications, which can
    * be observed via [[onServerNotification]].
    */
  def subscribeResource(uri: String): F[Unit]

  /** Cancels a subscription previously created with [[subscribeResource]]. */
  def unsubscribeResource(uri: String): F[Unit]

  /** Notifies the server that the list of roots exposed by this client has changed. */
  def sendRootsListChanged(): F[Unit]

  /** Registers a listener for notifications pushed by the server (e.g. resource updates, tool/prompt list changes, log messages). */
  def onServerNotification(listener: ServerNotificationListener[F]): F[Unit]

object McpClient:
  /** Creates an unidirectional [[McpClient]] over the given [[chimp.client.transport.Transport]] and performs the initialization handshake
    * with the server.
    *
    * @param transport
    *   The transport carrying JSON-RPC messages between client and server.
    * @param clientInfo
    *   Implementation name and version advertised to the server during initialization.
    * @param protocolVersion
    *   Protocol version proposed during initialization; defaults to the latest version supported by chimp.
    */
  def apply[F[_]](
      transport: Transport[F],
      clientInfo: Implementation,
      protocolVersion: ProtocolVersion = ProtocolVersion.Latest
  ): F[McpClient[F]] =
    McpClientImpl.create(transport, clientInfo, protocolVersion)

  /** Creates a [[BidirectionalMcpClient]] over the given [[chimp.client.transport.BidirectionalTransport]] and performs the initialization
    * handshake. The optional handlers determine which client capabilities (roots, sampling, elicitation) are advertised to the server; only
    * capabilities backed by a handler are enabled.
    *
    * @param transport
    *   The bidirectional transport carrying JSON-RPC messages in both directions.
    * @param clientInfo
    *   Implementation name and version advertised to the server during initialization.
    * @param rootsHandler
    *   Optional handler invoked when the server requests the client's roots; enables the `roots` capability when provided.
    * @param samplingHandler
    *   Optional handler invoked when the server requests an LLM completion via sampling; enables the `sampling` capability when provided.
    * @param elicitationHandler
    *   Optional handler invoked when the server requests user input via elicitation; enables the `elicitation` capability when provided.
    * @param protocolVersion
    *   Protocol version proposed during initialization; defaults to the latest version supported by chimp.
    */
  def bidirectional[F[_]](
      transport: BidirectionalTransport[F],
      clientInfo: Implementation,
      rootsHandler: Option[() => F[ListRootsResult]] = None,
      samplingHandler: Option[CreateMessageRequest => F[CreateMessageResult]] = None,
      elicitationHandler: Option[ElicitRequest => F[ElicitResult]] = None,
      protocolVersion: ProtocolVersion = ProtocolVersion.Latest
  ): F[BidirectionalMcpClient[F]] =
    McpClientImpl.createBidirectional(transport, clientInfo, protocolVersion, rootsHandler, samplingHandler, elicitationHandler)
