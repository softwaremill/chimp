package chimp.client

import chimp.client.internal.{Correlator, UUIDCorrelator}
import chimp.client.notifications.ServerNotificationListener
import chimp.client.transport.Transport
import chimp.protocol.*
import io.circe.syntax.*
import io.circe.{Decoder, Json}
import sttp.monad.MonadError
import sttp.monad.syntax.*

import java.util.concurrent.atomic.AtomicReference

object McpClientImpl:
  def create[F[_]](
      transport: Transport[F],
      clientInfo: Implementation,
      protocolVersion: ProtocolVersion,
      rootsHandler: Option[() => F[ListRootsResult]],
      samplingHandler: Option[CreateMessageRequest => F[CreateMessageResult]],
      elicitationHandler: Option[ElicitRequest => F[ElicitResult]],
      correlator: Correlator = UUIDCorrelator()
  ): McpClient[F] =
    val impl = new Impl[F](transport, clientInfo, protocolVersion, rootsHandler, samplingHandler, elicitationHandler, correlator)
    impl.installIncoming()
    impl

  private final class Impl[F[_]](
      transport: Transport[F],
      clientInfo: Implementation,
      protocolVersion: ProtocolVersion,
      rootsHandler: Option[() => F[ListRootsResult]],
      samplingHandler: Option[CreateMessageRequest => F[CreateMessageResult]],
      elicitationHandler: Option[ElicitRequest => F[ElicitResult]],
      correlator: Correlator
  ) extends McpClient[F]:
    private given MonadError[F] = transport.monad

    private val clientCapabilities: ClientCapabilities = ClientCapabilities(
      roots = rootsHandler.map(_ => ClientRootsCapability(listChanged = Some(true))),
      sampling = samplingHandler.map(_ => Json.obj()),
      elicitation = elicitationHandler.map(_ => Json.obj())
    )
    private val negotiatedServerCapabilities = AtomicReference[Option[ServerCapabilities]](None)
    private val notificationListeners = AtomicReference[List[ServerNotificationListener[F]]](Nil)

    override def serverCapabilities: Option[ServerCapabilities] = negotiatedServerCapabilities.get()

    private val handlers: Map[String, Json => F[Json]] =
      val entries = List(
        rootsHandler.map(fn => "roots/list" -> ((_: Json) => fn().map(_.asJson))),
        samplingHandler.map(fn =>
          "sampling/createMessage" -> ((params: Json) =>
            params.as[CreateMessageRequest] match
              case Right(req) => fn(req).map(_.asJson)
              case Left(e)    =>
                summon[MonadError[F]].error(IllegalArgumentException(s"Failed to decode CreateMessageRequest: ${e.getMessage}"))
          )
        ),
        elicitationHandler.map(fn =>
          "elicitation/create" -> ((params: Json) =>
            params.as[ElicitRequest] match
              case Right(req) => fn(req).map(_.asJson)
              case Left(e)    => summon[MonadError[F]].error(IllegalArgumentException(s"Failed to decode ElicitRequest: ${e.getMessage}"))
          )
        )
      ).flatten
      entries.toMap

    def installIncoming(): Unit =
      val _ = transport.onIncoming(handleIncoming)

    private def handleIncoming(msg: JSONRPCMessage): F[Unit] = msg match
      case JSONRPCMessage.Request(_, method, params, id) =>
        handlers.get(method) match
          case Some(handler) =>
            val rawParams = params.getOrElse(Json.obj())
            handler(rawParams)
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
      case notification: JSONRPCMessage.Notification =>
        notificationListeners
          .get()
          .foldLeft(summon[MonadError[F]].unit(())): (acc, listener) =>
            acc.flatMap(_ => listener.onNotification(notification).handleError(_ => summon[MonadError[F]].unit(())))
      case _ =>
        summon[MonadError[F]].unit(())

    override def initialize(): F[InitializeResult] =
      val params = InitializeParams(
        protocolVersion = protocolVersion,
        capabilities = clientCapabilities,
        clientInfo = clientInfo
      )
      sendRequest[InitializeResult]("initialize", Some(params.asJson)).flatMap: result =>
        if ProtocolVersion.from(result.protocolVersion).isDefined then
          negotiatedServerCapabilities.set(Some(result.capabilities))
          sendNotification("notifications/initialized", None).map(_ => result)
        else
          transport
            .close()
            .flatMap: _ =>
              summon[MonadError[F]].error(
                McpProtocolException(
                  s"Server responded with unsupported protocol version '${result.protocolVersion}', " +
                    s"client supports: ${ProtocolVersion.values.toList.map(_.name).sorted.mkString(", ")}"
                )
              )

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

    override def subscribeResource(uri: String): F[Unit] =
      requireServerCapability("resources/subscribe", _.resources.flatMap(_.subscribe).getOrElse(false)):
        sendRequest[Json]("resources/subscribe", Some(SubscribeParams(uri = uri).asJson)).map(_ => ())

    override def unsubscribeResource(uri: String): F[Unit] =
      requireServerCapability("resources/unsubscribe", _.resources.flatMap(_.subscribe).getOrElse(false)):
        sendRequest[Json]("resources/unsubscribe", Some(UnsubscribeParams(uri = uri).asJson)).map(_ => ())

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

    override def sendCancelled(requestId: RequestId, reason: Option[String]): F[Unit] =
      val params = CancelledParams(requestId = requestId, reason = reason).asJson
      sendNotification("notifications/cancelled", Some(params))

    override def sendRootsListChanged(): F[Unit] =
      sendNotification("notifications/roots/list_changed", None)

    override def onServerNotification(listener: ServerNotificationListener[F]): F[Unit] =
      val _ = notificationListeners.updateAndGet(listeners => listeners :+ listener)
      summon[MonadError[F]].unit(())

    private def requireServerCapability[A](method: String, present: ServerCapabilities => Boolean)(action: => F[A]): F[A] =
      negotiatedServerCapabilities.get() match
        case None =>
          summon[MonadError[F]].error(McpProtocolException(s"Client not initialized"))
        case Some(caps) if !present(caps) =>
          summon[MonadError[F]].error(McpProtocolException(s"Server did not negotiate the capability required for $method"))
        case Some(_) =>
          action

    private def sendRequest[R: Decoder](method: String, params: Option[Json]): F[R] =
      val request = JSONRPCMessage.Request(method = method, params = params, id = correlator.nextId())
      transport
        .send(request)
        .flatMap:
          case Some(JSONRPCMessage.Response(_, _, result)) =>
            result.as[R] match
              case Right(r) => summon[MonadError[F]].unit(r)
              case Left(e)  => summon[MonadError[F]].error(McpProtocolException(s"Failed to decode $method result: ${e.getMessage}"))
          case Some(JSONRPCMessage.Error(_, _, error)) =>
            summon[MonadError[F]].error(McpProtocolException(s"$method failed: ${error.code} ${error.message}"))
          case Some(other) =>
            summon[MonadError[F]].error(McpProtocolException(s"Unexpected response to $method: $other"))
          case None =>
            summon[MonadError[F]].error(McpProtocolException(s"No response received for request $method"))

    private def sendNotification(method: String, params: Option[Json]): F[Unit] =
      val notification = JSONRPCMessage.Notification(method = method, params = params)
      transport.send(notification).map(_ => ())
