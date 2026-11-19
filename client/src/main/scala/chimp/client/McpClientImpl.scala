package chimp.client

import chimp.client.internal.{Correlator, UUIDCorrelator}
import chimp.client.notifications.{ServerNotification, ServerNotificationListener}
import chimp.client.transport.{ClientBidirectionalTransport, ClientTransport}
import chimp.protocol.*
import io.circe.syntax.*
import io.circe.{Decoder, Json}
import sttp.monad.MonadError
import sttp.monad.syntax.*

import java.util.concurrent.atomic.AtomicReference

object McpClientImpl:
  def create[F[_]](
      transport: ClientTransport[F],
      clientInfo: Implementation,
      protocolVersion: ProtocolVersion,
      logLevel: Option[LoggingLevel],
      correlator: Correlator = UUIDCorrelator()
  ): F[McpClient[F]] =
    given MonadError[F] = transport.monad
    val requests = Requests(transport, clientInfo, ClientCapabilities(), logLevel, correlator, protocolVersion)
    connect(requests).map(server => new Impl[F](transport, requests, server))

  def createBidirectional[F[_]](
      transport: ClientBidirectionalTransport[F],
      clientInfo: Implementation,
      protocolVersion: ProtocolVersion,
      logLevel: Option[LoggingLevel],
      rootsHandler: Option[() => F[ListRootsResult]],
      samplingHandler: Option[CreateMessageRequest => F[CreateMessageResult]],
      elicitationHandler: Option[ElicitRequest => F[ElicitResult]],
      correlator: Correlator = UUIDCorrelator()
  ): F[BidirectionalMcpClient[F]] =
    given monad: MonadError[F] = transport.monad
    val clientCapabilities = ClientCapabilities(
      roots = rootsHandler.map(_ => ClientRootsCapability(listChanged = Some(true))),
      sampling = samplingHandler.map(_ => Json.obj()),
      elicitation = elicitationHandler.map(_ => Json.obj())
    )
    val requests = Requests(transport, clientInfo, clientCapabilities, logLevel, correlator, protocolVersion)
    val serverNotificationListeners = AtomicReference[List[ServerNotificationListener[F]]](Nil)
    val serverInitiatedRequestHandlers = buildServerInitiatedRequestHandlers(rootsHandler, samplingHandler, elicitationHandler)
    val incomingHandler = buildIncomingHandler(transport, serverInitiatedRequestHandlers, serverNotificationListeners)

    transport
      .onIncoming(incomingHandler)
      .flatMap: _ =>
        connect(requests).map: server =>
          new BidirectionalImpl[F](transport, requests, server, serverNotificationListeners)

  /** What the client learns about the server when it connects. */
  private final case class ConnectedServer(capabilities: ServerCapabilities, serverInfo: Implementation)

  /** Legacy versions run the `initialize` handshake; a stateless (2026-07-28) version has no handshake and asks `server/discover`. */
  private def connect[F[_]](requests: Requests[F]): F[ConnectedServer] =
    given MonadError[F] = requests.monad
    requests.transport
      .setProtocolVersion(requests.protocolVersion)
      .flatMap: _ =>
        if requests.protocolVersion.isModern then discoverServer(requests)
        else initialize(requests)

  /** A discover result that does not list the client's version switches to the newest common stateless version, or fails. */
  private def discoverServer[F[_]](requests: Requests[F]): F[ConnectedServer] =
    given monad: MonadError[F] = requests.monad
    requests
      .discover()
      .flatMap: discovered =>
        val supported = discovered.result.supportedVersions
        if supported.contains(requests.protocolVersion.name) then monad.unit(discovered.server)
        else
          retryVersion(supported) match
            case Some(next) => requests.useVersion(next).flatMap(_ => requests.discover()).map(_.server)
            case None       => monad.error(McpUnsupportedProtocolVersionException(supported, statelessVersions))

  /** Sends requests and turns their responses into results. On the stateless wire it stamps `_meta` on every request, and on `-32022` it
    * retries once with a version that both sides support.
    */
  private final class Requests[F[_]](
      val transport: ClientTransport[F],
      val clientInfo: Implementation,
      val clientCapabilities: ClientCapabilities,
      val logLevel: Option[LoggingLevel],
      correlator: Correlator,
      initialVersion: ProtocolVersion
  ):
    given monad: MonadError[F] = transport.monad

    private val version = AtomicReference(initialVersion)

    def protocolVersion: ProtocolVersion = version.get()

    def useVersion(next: ProtocolVersion): F[Unit] =
      monad.eval(version.set(next)).flatMap(_ => transport.setProtocolVersion(next))

    def nextId(): RequestId = correlator.nextId()

    def send[R: Decoder](method: String, params: Option[Json]): F[R] =
      sendForResult(method, params).flatMap(decode[R](method, _))

    def discover(): F[Discovered] =
      sendForResult("server/discover", None).flatMap: result =>
        decode[DiscoverResult]("server/discover", result).map(discovered => Discovered(discovered, serverInfoOf(result, discovered)))

    def notify(method: String, params: Option[Json]): F[Unit] =
      transport.send(JSONRPCMessage.Notification(method = method, params = params)).map(_ => ())

    private def sendForResult(method: String, params: Option[Json]): F[Json] =
      val sentWith = protocolVersion
      attempt(method, params, sentWith).flatMap:
        case Right(result)   => monad.unit(result)
        case Left(supported) =>
          retryVersion(supported) match
            case None       => monad.error(McpUnsupportedProtocolVersionException(supported, statelessVersions))
            case Some(next) =>
              useVersion(next)
                .flatMap(_ => attempt(method, params, next))
                .flatMap:
                  case Right(result)   => monad.unit(result)
                  case Left(supported) => monad.error(McpUnsupportedProtocolVersionException(supported, statelessVersions))

    /** `Left` holds the versions a `-32022` error names. */
    private def attempt(method: String, params: Option[Json], sentWith: ProtocolVersion): F[Either[List[String], Json]] =
      val request = JSONRPCMessage.Request(method = method, params = stamp(params, sentWith), id = nextId())
      transport
        .send(request)
        .flatMap:
          case Some(JSONRPCMessage.Response(_, _, result)) =>
            checkResultType(method, result).map(Right(_))
          case Some(JSONRPCMessage.Error(_, _, error))
              if sentWith.isModern && error.code == JSONRPCErrorCodes.UnsupportedProtocolVersion.code =>
            monad.unit(Left(error.data.flatMap(_.hcursor.get[List[String]]("supported").toOption).getOrElse(Nil)))
          case Some(JSONRPCMessage.Error(_, _, error)) =>
            monad.error(McpProtocolException(s"$method failed: ${error.code} ${error.message}"))
          case Some(other) =>
            monad.error(McpProtocolException(s"Unexpected response to $method: $other"))
          case None =>
            monad.error(McpProtocolException(s"No response received for request $method"))

    private def stamp(params: Option[Json], sentWith: ProtocolVersion): Option[Json] =
      if sentWith.isModern then Some(params.getOrElse(Json.obj()).deepMerge(Json.obj("_meta" -> requestMeta(sentWith))))
      else params

    /** The `_meta` that every stateless request carries in place of the `initialize` handshake. */
    private def requestMeta(sentWith: ProtocolVersion): Json =
      Json.fromFields(
        List(
          ProtocolMeta.ProtocolVersionKey -> sentWith.name.asJson,
          ProtocolMeta.ClientCapabilities -> clientCapabilities.asJson.deepDropNullValues,
          ProtocolMeta.ClientInfo -> clientInfo.asJson.deepDropNullValues
        ) ++ logLevel.map(level => ProtocolMeta.LogLevel -> level.asJson)
      )

    private def checkResultType(method: String, result: Json): F[Json] =
      result.hcursor.downField("resultType").focus match
        case None                                                                            => monad.unit(result)
        case Some(resultType) if resultType.asString.contains(ResultType.Complete.name)      => monad.unit(result)
        case Some(resultType) if resultType.asString.contains(ResultType.InputRequired.name) =>
          monad.error(
            McpProtocolException(s"$method returned an input_required result; the client does not support multi round-trip requests yet")
          )
        case Some(resultType) => monad.error(McpProtocolException(s"$method returned an unknown resultType: ${resultType.noSpaces}"))

    private def decode[R: Decoder](method: String, result: Json): F[R] =
      result.as[R] match
        case Right(response) => monad.unit(response)
        case Left(error)     => monad.error(McpProtocolException(s"Failed to decode $method result: ${error.getMessage}"))

  private val statelessVersions: List[String] = ProtocolVersion.supported.filter(_.isModern).map(_.name)

  /** The newest stateless version that chimp shares with a server's `supported` list. */
  private def retryVersion(supported: List[String]): Option[ProtocolVersion] =
    supported.flatMap(ProtocolVersion.from).filter(_.isModern).maxOption

  private final case class Discovered(result: DiscoverResult, serverInfo: Implementation):
    def server: ConnectedServer = ConnectedServer(result.capabilities, serverInfo)

  /** The spec puts `serverInfo` in `_meta`; a top-level `serverInfo` is also accepted. A server MAY omit it. */
  private def serverInfoOf(result: Json, discovered: DiscoverResult): Implementation =
    discovered._meta
      .flatMap(_.get(ProtocolMeta.ServerInfo))
      .orElse(result.hcursor.downField("serverInfo").focus)
      .flatMap(_.as[Implementation].toOption)
      .getOrElse(UnknownServer)

  private val UnknownServer: Implementation = Implementation(name = "unknown", version = "unknown")

  private def initialize[F[_]](requests: Requests[F]): F[ConnectedServer] =
    given monad: MonadError[F] = requests.monad
    val params = InitializeParams(
      protocolVersion = requests.protocolVersion,
      capabilities = requests.clientCapabilities,
      clientInfo = requests.clientInfo
    )
    val request = JSONRPCMessage.Request(method = "initialize", params = Some(params.asJson), id = requests.nextId())
    requests.transport
      .send(request)
      .flatMap:
        case Some(JSONRPCMessage.Response(_, _, result)) =>
          result.as[InitializeResult] match
            case Right(initResult) =>
              ProtocolVersion.from(initResult.protocolVersion).filterNot(_.isModern) match
                case Some(negotiated) =>
                  val server = ConnectedServer(initResult.capabilities, initResult.serverInfo)
                  requests
                    .useVersion(negotiated)
                    .flatMap(_ => requests.notify("notifications/initialized", None))
                    .flatMap(_ => applyLegacyLogLevel(requests, server))
                    .map(_ => server)
                case None =>
                  monad.error(
                    McpProtocolException(
                      s"Server responded with unsupported protocol version '${initResult.protocolVersion}', " +
                        s"client supports: ${ProtocolVersion.values.toList.filterNot(_.isModern).map(_.name).sorted.mkString(", ")}"
                    )
                  )
            case Left(error) =>
              monad.error(McpProtocolException(s"Failed to decode InitializeResult: ${error.getMessage}"))
        case Some(JSONRPCMessage.Error(_, _, error)) =>
          monad.error(McpProtocolException(s"Initialize failed: ${error.code} ${error.message}"))
        case Some(other) =>
          monad.error(McpProtocolException(s"Unexpected response to initialize: $other"))
        case None =>
          monad.error(McpProtocolException("No response received for initialize"))

  /** A legacy version has no per-request log level, so the client sets it once with `logging/setLevel`. */
  private def applyLegacyLogLevel[F[_]](requests: Requests[F], server: ConnectedServer): F[Unit] =
    given MonadError[F] = requests.monad
    requests.logLevel match
      case Some(level) if server.capabilities.logging.isDefined =>
        requests.send[Json]("logging/setLevel", Some(SetLevelParams(level = level).asJson)).map(_ => ())
      case _ => requests.monad.unit(())

  private def buildServerInitiatedRequestHandlers[F[_]](
      rootsHandler: Option[() => F[ListRootsResult]],
      samplingHandler: Option[CreateMessageRequest => F[CreateMessageResult]],
      elicitationHandler: Option[ElicitRequest => F[ElicitResult]]
  )(using monad: MonadError[F]): Map[String, Json => F[Json]] =
    val entries = List(
      rootsHandler.map(fn => "roots/list" -> ((_: Json) => fn().map(_.asJson))),
      samplingHandler.map(fn =>
        "sampling/createMessage" -> ((params: Json) =>
          params.as[CreateMessageParams] match
            case Right(decoded) => fn(CreateMessageRequest(params = decoded)).map(_.asJson)
            case Left(error)    =>
              monad.error(IllegalArgumentException(s"Failed to decode CreateMessageParams: ${error.getMessage}"))
        )
      ),
      elicitationHandler.map(fn =>
        "elicitation/create" -> ((params: Json) =>
          params.as[ElicitParams] match
            case Right(decoded) => fn(ElicitRequest(params = decoded)).map(_.asJson)
            case Left(error)    =>
              monad.error(IllegalArgumentException(s"Failed to decode ElicitParams: ${error.getMessage}"))
        )
      )
    ).flatten
    entries.toMap

  private def buildIncomingHandler[F[_]](
      transport: ClientBidirectionalTransport[F],
      serverInitiatedRequestHandlers: Map[String, Json => F[Json]],
      serverNotificationListeners: AtomicReference[List[ServerNotificationListener[F]]]
  )(using monad: MonadError[F]): JSONRPCMessage => F[Unit] =
    case JSONRPCMessage.Request(_, method, params, id) =>
      serverInitiatedRequestHandlers.get(method) match
        case Some(handler) =>
          handler(params.getOrElse(Json.obj()))
            .flatMap(result => transport.send(JSONRPCMessage.Response(id = id, result = result)).map(_ => ()))
            .handleError { case t =>
              val error = JSONRPCMessage.Error(
                id = id,
                error = JSONRPCErrorObject(
                  code = JSONRPCErrorCodes.InternalError.code,
                  message = Option(t.getMessage).getOrElse("Internal error")
                )
              )
              transport.send(error).map(_ => ())
            }
        case None =>
          val error = JSONRPCMessage.Error(
            id = id,
            error = JSONRPCErrorObject(code = JSONRPCErrorCodes.MethodNotFound.code, message = s"Client doesn't support method: $method")
          )
          transport.send(error).map(_ => ())
    case message: JSONRPCMessage.Notification =>
      val notification = ServerNotification.parse(message)
      serverNotificationListeners
        .get()
        .foldLeft(monad.unit(())): (acc, listener) =>
          acc.flatMap(_ => listener.onNotification(notification).handleError(_ => monad.unit(())))
    case _ =>
      monad.unit(())

  private class Impl[F[_]](
      protected val transport: ClientTransport[F],
      protected val requests: Requests[F],
      server: ConnectedServer
  ) extends McpClient[F]:
    protected given MonadError[F] = transport.monad
    protected val monad: MonadError[F] = transport.monad

    override val serverCapabilities: ServerCapabilities = server.capabilities
    override val serverInfo: Implementation = server.serverInfo

    override def protocolVersion: ProtocolVersion = requests.protocolVersion

    override def discover(): F[DiscoverResult] =
      requireStateless("server/discover")(requests.discover().map(_.result))

    override def ping(): F[Unit] = requireLegacy("ping")(requests.send[Json]("ping", None).map(_ => ()))

    override def close(): F[Unit] = transport.close()

    override def listTools(cursor: Option[Cursor]): F[ListToolsResponse] =
      requireServerCapability("tools/list", _.tools.isDefined):
        val params = cursor.map(c => ListToolsParams(cursor = Some(c)).asJson)
        requests.send[ListToolsResponse]("tools/list", params)

    override def callTool(name: String, arguments: Json): F[CallToolResult] =
      requireServerCapability("tools/call", _.tools.isDefined):
        val params = CallToolParams(name = name, arguments = arguments).asJson
        requests.send[CallToolResult]("tools/call", Some(params))

    override def listPrompts(cursor: Option[Cursor]): F[ListPromptsResult] =
      requireServerCapability("prompts/list", _.prompts.isDefined):
        val params = cursor.map(c => ListPromptsParams(cursor = Some(c)).asJson)
        requests.send[ListPromptsResult]("prompts/list", params)

    override def getPrompt(name: String, arguments: Map[String, String]): F[GetPromptResult] =
      requireServerCapability("prompts/get", _.prompts.isDefined):
        val params = GetPromptParams(name = name, arguments = if arguments.isEmpty then None else Some(arguments)).asJson
        requests.send[GetPromptResult]("prompts/get", Some(params))

    override def listResources(cursor: Option[Cursor]): F[ListResourcesResult] =
      requireServerCapability("resources/list", _.resources.isDefined):
        val params = cursor.map(c => ListResourcesParams(cursor = Some(c)).asJson)
        requests.send[ListResourcesResult]("resources/list", params)

    override def listResourceTemplates(cursor: Option[Cursor]): F[ListResourceTemplatesResult] =
      requireServerCapability("resources/templates/list", _.resources.isDefined):
        val params = cursor.map(c => ListResourceTemplatesParams(cursor = Some(c)).asJson)
        requests.send[ListResourceTemplatesResult]("resources/templates/list", params)

    override def readResource(uri: String): F[ReadResourceResult] =
      requireServerCapability("resources/read", _.resources.isDefined):
        requests.send[ReadResourceResult]("resources/read", Some(ReadResourceParams(uri = uri).asJson))

    override def complete(ref: CompleteRef, argument: CompleteArgument): F[CompleteResult] =
      requireServerCapability("completion/complete", _.completions.isDefined):
        val params = CompleteParams(ref = ref, argument = argument).asJson
        requests.send[CompleteResult]("completion/complete", Some(params))

    override def setLoggingLevel(level: LoggingLevel): F[Unit] =
      requireLegacy("logging/setLevel"):
        requireServerCapability("logging/setLevel", _.logging.isDefined):
          requests.send[Json]("logging/setLevel", Some(SetLevelParams(level = level).asJson)).map(_ => ())

    override def sendProgress(token: ProgressToken, progress: Double, total: Option[Double], message: Option[String]): F[Unit] =
      requireLegacy("notifications/progress"):
        val params = ProgressParams(progressToken = token, progress = progress, total = total, message = message).asJson
        requests.notify("notifications/progress", Some(params))

    protected def requireServerCapability[A](method: String, present: ServerCapabilities => Boolean)(action: => F[A]): F[A] =
      if present(serverCapabilities) then action
      else monad.error(McpProtocolException(s"Server did not negotiate the capability required for $method"))

    /** Operations that the stateless (2026-07-28) wire removed. */
    protected def requireLegacy[A](method: String)(action: => F[A]): F[A] =
      if protocolVersion.isModern then
        monad.error(McpProtocolException(s"$method is not available at protocol version ${protocolVersion.name}, only at legacy versions"))
      else action

    private def requireStateless[A](method: String)(action: => F[A]): F[A] =
      if protocolVersion.isModern then action
      else monad.error(McpProtocolException(s"$method is not available at legacy protocol version ${protocolVersion.name}"))

  private final class BidirectionalImpl[F[_]](
      bidiTransport: ClientBidirectionalTransport[F],
      requests: Requests[F],
      server: ConnectedServer,
      serverNotificationListeners: AtomicReference[List[ServerNotificationListener[F]]]
  ) extends Impl[F](bidiTransport, requests, server)
      with BidirectionalMcpClient[F]:

    override def subscribeResource(uri: String): F[Unit] =
      requireLegacy("resources/subscribe"):
        requireServerCapability("resources/subscribe", _.resources.flatMap(_.subscribe).getOrElse(false)):
          requests.send[Json]("resources/subscribe", Some(SubscribeParams(uri = uri).asJson)).map(_ => ())

    override def unsubscribeResource(uri: String): F[Unit] =
      requireLegacy("resources/unsubscribe"):
        requireServerCapability("resources/unsubscribe", _.resources.flatMap(_.subscribe).getOrElse(false)):
          requests.send[Json]("resources/unsubscribe", Some(UnsubscribeParams(uri = uri).asJson)).map(_ => ())

    override def sendRootsListChanged(): F[Unit] =
      requireLegacy("notifications/roots/list_changed"):
        requests.notify("notifications/roots/list_changed", None)

    override def onServerNotification(listener: ServerNotificationListener[F]): F[Unit] =
      val _ = serverNotificationListeners.updateAndGet(listeners => listeners :+ listener)
      monad.unit(())
