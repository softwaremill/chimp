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
      correlator: Correlator = UUIDCorrelator()
  ): F[McpClient[F]] =
    given MonadError[F] = transport.monad
    val clientCapabilities = ClientCapabilities()
    initialize(transport, clientInfo, protocolVersion, clientCapabilities, correlator).map: initResult =>
      new Impl[F](transport, clientInfo, protocolVersion, clientCapabilities, correlator, initResult)

  def createBidirectional[F[_]](
      transport: ClientBidirectionalTransport[F],
      clientInfo: Implementation,
      protocolVersion: ProtocolVersion,
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
    val serverNotificationListeners = AtomicReference[List[ServerNotificationListener[F]]](Nil)
    val serverInitiatedRequestHandlers = buildServerInitiatedRequestHandlers(rootsHandler, samplingHandler, elicitationHandler)
    val incomingHandler = buildIncomingHandler(transport, serverInitiatedRequestHandlers, serverNotificationListeners)

    transport
      .onIncoming(incomingHandler)
      .flatMap: _ =>
        initialize(transport, clientInfo, protocolVersion, clientCapabilities, correlator).map: initResult =>
          new BidirectionalImpl[F](
            transport,
            clientInfo,
            protocolVersion,
            clientCapabilities,
            correlator,
            initResult,
            serverNotificationListeners
          )

  private def initialize[F[_]](
      transport: ClientTransport[F],
      clientInfo: Implementation,
      protocolVersion: ProtocolVersion,
      clientCapabilities: ClientCapabilities,
      correlator: Correlator
  ): F[InitializeResult] =
    given monad: MonadError[F] = transport.monad
    val params = InitializeParams(
      protocolVersion = protocolVersion,
      capabilities = clientCapabilities,
      clientInfo = clientInfo
    )
    val request = JSONRPCMessage.Request(method = "initialize", params = Some(params.asJson), id = correlator.nextId())
    transport
      .send(request)
      .flatMap:
        case Some(JSONRPCMessage.Response(_, _, result)) =>
          result.as[InitializeResult] match
            case Right(initResult) if ProtocolVersion.from(initResult.protocolVersion).isDefined =>
              val notification = JSONRPCMessage.Notification(method = "notifications/initialized")
              transport.send(notification).map(_ => initResult)
            case Right(initResult) =>
              monad.error(
                McpProtocolException(
                  s"Server responded with unsupported protocol version '${initResult.protocolVersion}', " +
                    s"client supports: ${ProtocolVersion.values.toList.map(_.name).sorted.mkString(", ")}"
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
      protected val clientInfo: Implementation,
      protected val protocolVersion: ProtocolVersion,
      protected val clientCapabilities: ClientCapabilities,
      protected val correlator: Correlator,
      initializeResult: InitializeResult
  ) extends McpClient[F]:
    protected given MonadError[F] = transport.monad
    protected val monad: MonadError[F] = transport.monad

    override val serverCapabilities: ServerCapabilities = initializeResult.capabilities
    override val serverInfo: Implementation = initializeResult.serverInfo

    override def ping(): F[Unit] = sendRequest[Json]("ping", None).map(_ => ())

    override def close(): F[Unit] = transport.close()

    override def listTools(cursor: Option[Cursor]): F[ListToolsResponse] =
      requireServerCapability("tools/list", _.tools.isDefined):
        val params = cursor.map(c => ListToolsParams(cursor = Some(c)).asJson)
        sendRequest[ListToolsResponse]("tools/list", params)

    override def callTool(name: String, arguments: Json): F[CallToolResult] =
      requireServerCapability("tools/call", _.tools.isDefined):
        val params = CallToolParams(name = name, arguments = arguments).asJson
        sendRequest[CallToolResult]("tools/call", Some(params))

    override def listPrompts(cursor: Option[Cursor]): F[ListPromptsResult] =
      requireServerCapability("prompts/list", _.prompts.isDefined):
        val params = cursor.map(c => ListPromptsParams(cursor = Some(c)).asJson)
        sendRequest[ListPromptsResult]("prompts/list", params)

    override def getPrompt(name: String, arguments: Map[String, String]): F[GetPromptResult] =
      requireServerCapability("prompts/get", _.prompts.isDefined):
        val params = GetPromptParams(name = name, arguments = if arguments.isEmpty then None else Some(arguments)).asJson
        sendRequest[GetPromptResult]("prompts/get", Some(params))

    override def listResources(cursor: Option[Cursor]): F[ListResourcesResult] =
      requireServerCapability("resources/list", _.resources.isDefined):
        val params = cursor.map(c => ListResourcesParams(cursor = Some(c)).asJson)
        sendRequest[ListResourcesResult]("resources/list", params)

    override def listResourceTemplates(cursor: Option[Cursor]): F[ListResourceTemplatesResult] =
      requireServerCapability("resources/templates/list", _.resources.isDefined):
        val params = cursor.map(c => ListResourceTemplatesParams(cursor = Some(c)).asJson)
        sendRequest[ListResourceTemplatesResult]("resources/templates/list", params)

    override def readResource(uri: String): F[ReadResourceResult] =
      requireServerCapability("resources/read", _.resources.isDefined):
        sendRequest[ReadResourceResult]("resources/read", Some(ReadResourceParams(uri = uri).asJson))

    override def complete(ref: CompleteRef, argument: CompleteArgument): F[CompleteResult] =
      requireServerCapability("completion/complete", _.completions.isDefined):
        val params = CompleteParams(ref = ref, argument = argument).asJson
        sendRequest[CompleteResult]("completion/complete", Some(params))

    override def setLoggingLevel(level: LoggingLevel): F[Unit] =
      requireServerCapability("logging/setLevel", _.logging.isDefined):
        sendRequest[Json]("logging/setLevel", Some(SetLevelParams(level = level).asJson)).map(_ => ())

    override def sendProgress(token: ProgressToken, progress: Double, total: Option[Double], message: Option[String]): F[Unit] =
      val params = ProgressParams(progressToken = token, progress = progress, total = total, message = message).asJson
      sendNotification("notifications/progress", Some(params))

    protected def requireServerCapability[A](method: String, present: ServerCapabilities => Boolean)(action: => F[A]): F[A] =
      if present(serverCapabilities) then action
      else monad.error(McpProtocolException(s"Server did not negotiate the capability required for $method"))

    protected def sendRequest[R: Decoder](method: String, params: Option[Json]): F[R] =
      val request = JSONRPCMessage.Request(method = method, params = params, id = correlator.nextId())
      transport
        .send(request)
        .flatMap:
          case Some(JSONRPCMessage.Response(_, _, result)) =>
            result.as[R] match
              case Right(response) => monad.unit(response)
              case Left(error)     => monad.error(McpProtocolException(s"Failed to decode $method result: ${error.getMessage}"))
          case Some(JSONRPCMessage.Error(_, _, error)) =>
            monad.error(McpProtocolException(s"$method failed: ${error.code} ${error.message}"))
          case Some(other) =>
            monad.error(McpProtocolException(s"Unexpected response to $method: $other"))
          case None =>
            monad.error(McpProtocolException(s"No response received for request $method"))

    protected def sendNotification(method: String, params: Option[Json]): F[Unit] =
      val notification = JSONRPCMessage.Notification(method = method, params = params)
      transport.send(notification).map(_ => ())

  private final class BidirectionalImpl[F[_]](
      bidiTransport: ClientBidirectionalTransport[F],
      clientInfo: Implementation,
      protocolVersion: ProtocolVersion,
      clientCapabilities: ClientCapabilities,
      correlator: Correlator,
      initializeResult: InitializeResult,
      serverNotificationListeners: AtomicReference[List[ServerNotificationListener[F]]]
  ) extends Impl[F](bidiTransport, clientInfo, protocolVersion, clientCapabilities, correlator, initializeResult)
      with BidirectionalMcpClient[F]:

    override def subscribeResource(uri: String): F[Unit] =
      requireServerCapability("resources/subscribe", _.resources.flatMap(_.subscribe).getOrElse(false)):
        sendRequest[Json]("resources/subscribe", Some(SubscribeParams(uri = uri).asJson)).map(_ => ())

    override def unsubscribeResource(uri: String): F[Unit] =
      requireServerCapability("resources/unsubscribe", _.resources.flatMap(_.subscribe).getOrElse(false)):
        sendRequest[Json]("resources/unsubscribe", Some(UnsubscribeParams(uri = uri).asJson)).map(_ => ())

    override def sendRootsListChanged(): F[Unit] =
      sendNotification("notifications/roots/list_changed", None)

    override def onServerNotification(listener: ServerNotificationListener[F]): F[Unit] =
      val _ = serverNotificationListeners.updateAndGet(listeners => listeners :+ listener)
      monad.unit(())
