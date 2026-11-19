package chimp.client

import chimp.client.notifications.ServerNotificationListener
import chimp.client.transport.{ClientBidirectionalTransport, ClientTransport}
import chimp.protocol.*
import io.circe.Json

/** A Model Context Protocol (MCP) client that is connected to a server: at a legacy protocol version it has completed the `initialize`
  * handshake, at a stateless version (2026-07-28) it has called `server/discover`.
  *
  * Exposes the server's advertised capabilities and identity, and provides methods for sending client-initiated requests and notifications
  * over the underlying [[chimp.client.transport.ClientTransport]].
  *
  * For bidirectional interaction (server-initiated requests, resource subscriptions, notification listeners), use
  * [[BidirectionalMcpClient]] instead.
  */
trait McpClient[F[_]]:
  /** The protocol version that the client uses. At a stateless version it can change once, when the server rejects the version with
    * `-32022` and the client retries with a version from the server's list.
    */
  def protocolVersion: ProtocolVersion

  /** Sends `server/discover` and returns the server's versions, capabilities and instructions. Only at a stateless version. */
  def discover(): F[DiscoverResult]

  /** Sends a `ping` request to the server. Only at a legacy version. */
  def ping(): F[Unit]

  /** Closes the underlying transport and releases any resources held by this client. */
  def close(): F[Unit]

  /** Capabilities advertised by the server in the `initialize` or `server/discover` result. */
  def serverCapabilities: ServerCapabilities

  /** Server implementation name and version, from the `initialize` or `server/discover` result. If a stateless server does not send it,
    * both values are `unknown`.
    */
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

  /** Sets the minimum severity level for log messages the server should forward to this client. Only at a legacy version; at a stateless
    * version use the `logLevel` option of the client.
    */
  def setLoggingLevel(level: LoggingLevel): F[Unit]

  /** Sends a progress notification for a previously issued request that advertised a progress token. Only at a legacy version.
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

/** An [[McpClient]] used over a [[chimp.client.transport.ClientBidirectionalTransport]], which additionally supports server-initiated
  * interactions: subscribing to resource updates, notifying the server about changes to the client's roots, and handling notifications
  * pushed by the server.
  */
trait BidirectionalMcpClient[F[_]] extends McpClient[F]:
  /** Subscribes to updates for the resource identified by the given URI. The server will emit `resources/updated` notifications, which can
    * be observed via [[onServerNotification]]. Only at a legacy version.
    */
  def subscribeResource(uri: String): F[Unit]

  /** Cancels a subscription previously created with [[subscribeResource]]. Only at a legacy version. */
  def unsubscribeResource(uri: String): F[Unit]

  /** Notifies the server that the list of roots exposed by this client has changed. Only at a legacy version. */
  def sendRootsListChanged(): F[Unit]

  /** Registers a listener for notifications pushed by the server (e.g. resource updates, tool/prompt list changes, log messages). */
  def onServerNotification(listener: ServerNotificationListener[F]): F[Unit]

object McpClient:
  /** Creates an unidirectional [[McpClient]] over the given [[chimp.client.transport.ClientTransport]] and connects it to the server.
    *
    * @param transport
    *   The transport carrying JSON-RPC messages between client and server.
    * @param clientInfo
    *   Implementation name and version sent to the server.
    * @param protocolVersion
    *   The protocol version of the client; the client gives it to the transport. A legacy version runs the `initialize` handshake, a
    *   stateless version (2026-07-28) calls `server/discover`. Defaults to the latest legacy version.
    * @param logLevel
    *   The minimum level of the log messages the server sends. At a stateless version every request carries it in `_meta`; at a legacy
    *   version the client sends `logging/setLevel` once after `initialize`. Without it the server sends no log messages.
    */
  def apply[F[_]](
      transport: ClientTransport[F],
      clientInfo: Implementation,
      protocolVersion: ProtocolVersion = ProtocolVersion.LatestLegacy,
      logLevel: Option[LoggingLevel] = None
  ): F[McpClient[F]] =
    McpClientImpl.create(transport, clientInfo, protocolVersion, logLevel)

  /** Creates a [[BidirectionalMcpClient]] over the given [[chimp.client.transport.ClientBidirectionalTransport]] and connects it to the
    * server. The optional handlers determine which client capabilities (roots, sampling, elicitation) are advertised to the server; only
    * capabilities backed by a handler are enabled.
    *
    * @param transport
    *   The bidirectional transport carrying JSON-RPC messages in both directions.
    * @param clientInfo
    *   Implementation name and version sent to the server.
    * @param rootsHandler
    *   Optional handler invoked when the server requests the client's roots; enables the `roots` capability when provided.
    * @param samplingHandler
    *   Optional handler invoked when the server requests an LLM completion via sampling; enables the `sampling` capability when provided.
    * @param elicitationHandler
    *   Optional handler invoked when the server requests user input via elicitation; enables the `elicitation` capability when provided.
    * @param protocolVersion
    *   The protocol version of the client, see [[apply]].
    * @param logLevel
    *   The minimum level of the log messages the server sends, see [[apply]].
    */
  def bidirectional[F[_]](
      transport: ClientBidirectionalTransport[F],
      clientInfo: Implementation,
      rootsHandler: Option[() => F[ListRootsResult]] = None,
      samplingHandler: Option[CreateMessageRequest => F[CreateMessageResult]] = None,
      elicitationHandler: Option[ElicitRequest => F[ElicitResult]] = None,
      protocolVersion: ProtocolVersion = ProtocolVersion.LatestLegacy,
      logLevel: Option[LoggingLevel] = None
  ): F[BidirectionalMcpClient[F]] =
    McpClientImpl.createBidirectional(transport, clientInfo, protocolVersion, logLevel, rootsHandler, samplingHandler, elicitationHandler)
