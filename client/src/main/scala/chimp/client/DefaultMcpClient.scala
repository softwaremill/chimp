package chimp.client

import chimp.client.internal.Correlator
import chimp.client.notifications.ServerNotificationListener
import chimp.client.transport.Transport
import chimp.protocol.*
import io.circe.syntax.*
import io.circe.{Decoder, Json}
import sttp.monad.MonadError
import sttp.monad.syntax.*

import java.util.concurrent.atomic.AtomicReference

object DefaultMcpClient:
  def create[F[_]](
      transport: Transport[F],
      clientInfo: Implementation,
      protocolVersion: String,
      rootsHandler: Option[() => F[ListRootsResult]],
      samplingHandler: Option[CreateMessageRequest => F[CreateMessageResult]],
      elicitationHandler: Option[ElicitRequest => F[ElicitResult]]
  ): McpClient[F] =
    val impl = new Impl[F](transport, clientInfo, protocolVersion, rootsHandler, samplingHandler, elicitationHandler)
    impl.installIncoming()
    impl

  private final class Impl[F[_]](
      transport: Transport[F],
      clientInfo: Implementation,
      protocolVersion: String,
      rootsHandler: Option[() => F[ListRootsResult]],
      samplingHandler: Option[CreateMessageRequest => F[CreateMessageResult]],
      elicitationHandler: Option[ElicitRequest => F[ElicitResult]]
  ) extends McpClient[F]:
    private given MonadError[F] = transport.monad
    private val correlator = Correlator()
    private val listeners = AtomicReference[List[ServerNotificationListener[F]]](Nil)

    private val wireCaps: ClientCapabilities = ClientCapabilities(
      roots       = rootsHandler.map(_ => ClientRootsCapability(listChanged = Some(true))),
      sampling    = samplingHandler.map(_ => Json.obj()),
      elicitation = elicitationHandler.map(_ => Json.obj())
    )

    private val dispatch: Map[String, Json => F[Json]] =
      val entries = List(
        rootsHandler.map(fn =>
          "roots/list" -> ((_: Json) => fn().map(_.asJson))
        ),
        samplingHandler.map(fn =>
          "sampling/createMessage" -> ((params: Json) =>
            params.as[CreateMessageRequest] match
              case Right(req) => fn(req).map(_.asJson)
              case Left(e)    => summon[MonadError[F]].error(IllegalArgumentException(s"Failed to decode CreateMessageRequest: ${e.getMessage}"))
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
        dispatch.get(method) match
          case Some(h) =>
            val rawParams = params.getOrElse(Json.obj())
            h(rawParams).flatMap(result =>
              transport.send(JSONRPCMessage.Response(id = id, result = result)).map(_ => ())
            ).handleError { case t =>
              val err = JSONRPCMessage.Error(
                id = id,
                error = JSONRPCErrorObject(code = JSONRPCErrorCodes.InternalError.code, message = Option(t.getMessage).getOrElse("internal error"))
              )
              transport.send(err).map(_ => ())
            }
          case None =>
            val err = JSONRPCMessage.Error(
              id = id,
              error = JSONRPCErrorObject(code = JSONRPCErrorCodes.MethodNotFound.code, message = s"No handler for method: $method")
            )
            transport.send(err).map(_ => ())
      case n: JSONRPCMessage.Notification =>
        listeners.get().foldLeft(summon[MonadError[F]].unit(())): (acc, l) =>
          acc.flatMap(_ => l.onNotification(n).handleError(_ => summon[MonadError[F]].unit(())))
      case _ =>
        summon[MonadError[F]].unit(())

    override def initialize(): F[InitializeResult] =
      val params = InitializeParams(
        protocolVersion = protocolVersion,
        capabilities = wireCaps,
        clientInfo = clientInfo
      )
      sendRequest[InitializeResult]("initialize", Some(params.asJson)).flatMap: r =>
        sendNotification("notifications/initialized", None).map(_ => r)

    override def ping(): F[Unit] = sendRequest[Json]("ping", None).map(_ => ())

    override def close(): F[Unit] = transport.close()

    override def listTools(cursor: Option[Cursor]): F[ListToolsResponse] =
      val params = cursor.map(c => ListToolsParams(cursor = Some(c)).asJson)
      sendRequest[ListToolsResponse]("tools/list", params)

    override def callTool(name: String, arguments: Json): F[CallToolResult] =
      val params = CallToolParams(name = name, arguments = arguments).asJson
      sendRequest[CallToolResult]("tools/call", Some(params))

    override def listPrompts(cursor: Option[Cursor]): F[ListPromptsResult] =
      val params = cursor.map(c => ListPromptsParams(cursor = Some(c)).asJson)
      sendRequest[ListPromptsResult]("prompts/list", params)

    override def getPrompt(name: String, arguments: Map[String, String]): F[GetPromptResult] =
      val argOpt = if arguments.isEmpty then None else Some(arguments)
      val params = GetPromptParams(name = name, arguments = argOpt).asJson
      sendRequest[GetPromptResult]("prompts/get", Some(params))

    override def listResources(cursor: Option[Cursor]): F[ListResourcesResult] =
      val params = cursor.map(c => ListResourcesParams(cursor = Some(c)).asJson)
      sendRequest[ListResourcesResult]("resources/list", params)

    override def listResourceTemplates(cursor: Option[Cursor]): F[ListResourceTemplatesResult] =
      val params = cursor.map(c => ListResourceTemplatesParams(cursor = Some(c)).asJson)
      sendRequest[ListResourceTemplatesResult]("resources/templates/list", params)

    override def readResource(uri: String): F[ReadResourceResult] =
      sendRequest[ReadResourceResult]("resources/read", Some(ReadResourceParams(uri = uri).asJson))

    override def subscribeResource(uri: String): F[Unit] =
      sendRequest[Json]("resources/subscribe", Some(SubscribeParams(uri = uri).asJson)).map(_ => ())

    override def unsubscribeResource(uri: String): F[Unit] =
      sendRequest[Json]("resources/unsubscribe", Some(UnsubscribeParams(uri = uri).asJson)).map(_ => ())

    override def complete(ref: CompleteRef, argument: CompleteArgument): F[CompleteResult] =
      val params = CompleteParams(ref = ref, argument = argument).asJson
      sendRequest[CompleteResult]("completion/complete", Some(params))

    override def setLoggingLevel(level: LoggingLevel): F[Unit] =
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
      val _ = listeners.updateAndGet(ls => ls :+ listener)
      summon[MonadError[F]].unit(())

    private def sendRequest[R: Decoder](method: String, params: Option[Json]): F[R] =
      val req = JSONRPCMessage.Request(method = method, params = params, id = correlator.nextId())
      transport.send(req).flatMap:
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
      val n = JSONRPCMessage.Notification(method = method, params = params)
      transport.send(n).map(_ => ())
