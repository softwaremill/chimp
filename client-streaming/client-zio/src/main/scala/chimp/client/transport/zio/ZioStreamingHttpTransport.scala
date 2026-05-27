package chimp.client.transport.zio

import chimp.client.transport.HttpTransport.HttpOutcome
import chimp.client.transport.{HttpTransport, StreamingHttpTransport, Transport}
import chimp.client.{McpProtocolException, McpSessionNotFoundException}
import chimp.protocol.{JSONRPCErrorCodes, JSONRPCErrorObject, JSONRPCMessage, ProtocolVersion, RequestId}
import org.slf4j.LoggerFactory
import sttp.capabilities.zio.ZioStreams
import sttp.client4.{asStreamUnsafe, basicRequest, Response, StreamBackend}
import sttp.model.sse.ServerSentEvent
import sttp.model.{MediaType, StatusCode, Uri}
import sttp.monad.MonadError
import zio.stream.{Stream, ZPipeline, ZStream}
import zio.{Duration, Exit, Promise, Ref, Schedule, Scope, Task, ZIO, ZLayer}

import scala.concurrent.duration.FiniteDuration

final class ZioStreamingHttpTransport private (
    backend: StreamBackend[Task, ZioStreams],
    uri: Uri,
    protocolVersion: ProtocolVersion,
    timeout: FiniteDuration,
    reconnectSchedule: Schedule[Any, Any, Any],
    scope: Scope.Closeable,
    sessionRef: Ref[Option[String]],
    sessionReady: Promise[Nothing, Unit],
    pending: ZioPendingRequests,
    incomingRef: Ref[JSONRPCMessage => Task[Unit]],
    lastEventId: Ref[Option[String]],
    closingRef: Ref[Boolean]
) extends StreamingHttpTransport[Task, ZioStreams](backend, uri, ZioStreams):

  private val log = LoggerFactory.getLogger(classOf[ZioStreamingHttpTransport])

  override given monad: MonadError[Task] = backend.monad

  override def send(msg: JSONRPCMessage): Task[Option[JSONRPCMessage]] =
    msg match
      case request: JSONRPCMessage.Request =>
        pending
          .register(request.id, timeout)
          .flatMap: await =>
            sendRequest(request, await)
          .ensuring(pending.complete(request.id, cancelled(request.id)).orDie)
      case other =>
        sendNonRequest(other)

  override def onIncoming(handler: JSONRPCMessage => Task[Unit]): Task[Unit] =
    incomingRef.set(handler)

  override def close(): Task[Unit] =
    val deleteSession = sessionRef
      .getAndSet(None)
      .flatMap:
        case Some(id) =>
          HttpTransport
            .baseDeleteRequest(uri, protocolVersion, id)
            .response(asStreamUnsafe(ZioStreams))
            .send(backend)
            .flatMap(drainBody)
            .ignore
        case None => ZIO.unit
    closingRef.set(true) *> deleteSession *> scope.close(Exit.unit).ignore

  private def sendRequest(request: JSONRPCMessage.Request, await: () => Task[JSONRPCMessage]): Task[Option[JSONRPCMessage]] =
    post(request).flatMap: resp =>
      captureSession(resp) *>
        sessionRef.get.flatMap: session =>
          HttpTransport.resolveResponse(resp, session) match
            case Left(err: McpSessionNotFoundException) =>
              sessionRef.set(None) *> ZIO.fail(err)
            case Left(err) =>
              ZIO.fail(err)
            case Right(HttpOutcome.NoBody) =>
              drainBody(resp) *> ZIO.fail(McpProtocolException("Server returned 202 Accepted for a Request"))
            case Right(HttpOutcome.JsonBody) =>
              for
                body <- collectBody(resp)
                msg <- decode(body)
                _ <- routeMessage(msg)
                out <- await().map(Some(_))
              yield out
            case Right(HttpOutcome.SseBody) =>
              forkSseDrain(resp, Some(request.id)) *> await().map(Some(_))

  private def sendNonRequest(msg: JSONRPCMessage): Task[Option[JSONRPCMessage]] =
    post(msg).flatMap: response =>
      captureSession(response) *>
        sessionRef.get.flatMap: session =>
          HttpTransport.resolveResponse(response, session) match
            case Left(err: McpSessionNotFoundException) =>
              sessionRef.set(None) *> ZIO.fail(err)
            case Left(err) =>
              ZIO.fail(err)
            case Right(HttpOutcome.NoBody) =>
              drainBody(response).as(None)
            case Right(HttpOutcome.JsonBody) =>
              drainBody(response).as(None)
            case Right(HttpOutcome.SseBody) =>
              forkSseDrain(response, None).as(None)

  private def post(msg: JSONRPCMessage): Task[Response[Either[String, Stream[Throwable, Byte]]]] =
    sessionRef.get.flatMap: session =>
      HttpTransport
        .basePostRequest(uri, protocolVersion, session, Transport.encode(msg))
        .response(asStreamUnsafe(ZioStreams))
        .send(backend)

  private def captureSession(response: Response[?]): Task[Unit] =
    val maybeUpdate = response.header("Mcp-Session-Id") match
      case Some(id) => sessionRef.set(Some(id))
      case None     => ZIO.unit
    maybeUpdate *> sessionReady.succeed(()).unit

  private def collectBody(response: Response[Either[String, Stream[Throwable, Byte]]]): Task[String] =
    response.body match
      case Left(err)     => ZIO.fail(McpProtocolException(s"HTTP 200 with non-stream body: $err"))
      case Right(stream) => stream.via(ZPipeline.utf8Decode).runCollect.map(_.mkString)

  private def drainBody(response: Response[Either[String, Stream[Throwable, Byte]]]): Task[Unit] =
    response.body match
      case Left(_)       => ZIO.unit
      case Right(stream) => stream.runDrain.ignore

  private def decode(body: String): Task[JSONRPCMessage] =
    Transport.decode(body) match
      case Right(msg) => ZIO.succeed(msg)
      case Left(err)  => ZIO.fail(McpProtocolException(s"Failed to decode response body: ${err.getMessage}, payload $body"))

  private def forkSseDrain(
      response: Response[Either[String, Stream[Throwable, Byte]]],
      requestId: Option[RequestId]
  ): Task[Unit] =
    response.body match
      case Left(err) =>
        ZIO.fail(McpProtocolException(s"Expected SSE stream, got: $err"))
      case Right(stream) =>
        val shouldContinue: Task[Boolean] = requestId match
          case Some(id) =>
            closingRef.get.flatMap:
              case true  => ZIO.succeed(false)
              case false => pending.isPending(id)
          case None => ZIO.succeed(false)
        val drain = Ref
          .make(Option.empty[String])
          .flatMap: localLastEventId =>
            streamWithResume(stream, localLastEventId, shouldContinue)
          .ensuring:
            requestId match
              case Some(id) => pending.complete(id, sseEnded(id)).orDie
              case None     => ZIO.unit
        drain.forkIn(scope).unit

  private def parseSseEvents(stream: Stream[Throwable, Byte]): Stream[Throwable, ServerSentEvent] =
    stream
      .via(ZPipeline.utf8Decode)
      .via(ZPipeline.splitLines)
      .concat(ZStream.succeed(""))
      .mapAccum(List.empty[String]): (buffered, line) =>
        if line.isEmpty then (Nil, Some(ServerSentEvent.parse(buffered.reverse)))
        else (line :: buffered, None)
      .collect { case Some(event) => event }

  private def dispatch(event: ServerSentEvent): Task[Unit] =
    event.data match
      case Some(data) if data.nonEmpty =>
        Transport.decode(data) match
          case Right(msg) => routeMessage(msg)
          case Left(_)    => ZIO.unit
      case _ => ZIO.unit

  private def routeMessage(msg: JSONRPCMessage): Task[Unit] = msg match
    case response: JSONRPCMessage.Response => pending.complete(response.id, response).unit
    case err: JSONRPCMessage.Error         => pending.complete(err.id, err).unit
    case other                             => incomingRef.get.flatMap(_(other))

  private[zio] def startGetSseListener: Task[Unit] =
    val listener = sessionReady.await *> openGetSseStream(None).flatMap:
      case None         => ZIO.unit
      case Some(stream) => streamWithResume(stream, lastEventId, closingRef.get.map(!_))
    listener.catchAll(t => ZIO.succeed(log.warn(s"GET listener failed: ${t.getMessage}"))).forkIn(scope).unit

  private def streamWithResume(
      stream: Stream[Throwable, Byte],
      lastEventIdRef: Ref[Option[String]],
      shouldContinue: Task[Boolean]
  ): Task[Unit] =
    reconnectSchedule.driver.flatMap: driver =>
      def loop(stream: Stream[Throwable, Byte]): Task[Unit] =
        drainSseStream(stream, lastEventIdRef)
          .catchAll(t => ZIO.succeed(log.warn(s"SSE drain error: ${t.getMessage}")))
          .flatMap: _ =>
            shouldContinue.flatMap:
              case false => ZIO.unit
              case true  =>
                driver
                  .next(())
                  .foldZIO(
                    _ => ZIO.unit,
                    _ =>
                      lastEventIdRef.get.flatMap: lastEventId =>
                        openGetSseStream(lastEventId).flatMap:
                          case Some(stream) => loop(stream)
                          case None         => ZIO.unit
                  )
      loop(stream)

  private def drainSseStream(
      stream: Stream[Throwable, Byte],
      lastEventIdRef: Ref[Option[String]]
  ): Task[Unit] =
    parseSseEvents(stream)
      .mapZIO: event =>
        dispatch(event) *> (event.id match
          case Some(id) if id.nonEmpty => lastEventIdRef.set(Some(id))
          case _                       => ZIO.unit)
      .runDrain

  private def openGetSseStream(lastEventId: Option[String]): Task[Option[Stream[Throwable, Byte]]] =
    sessionRef.get.flatMap: session =>
      val base = basicRequest
        .get(uri)
        .header("Accept", MediaType.TextEventStream.toString)
        .header("MCP-Protocol-Version", protocolVersion.name)
        .response(asStreamUnsafe(ZioStreams))
      val withSession = session.fold(base)(s => base.header("Mcp-Session-Id", s))
      val withLastEventId = lastEventId.fold(withSession)(id => withSession.header("Last-Event-ID", id))
      withLastEventId
        .send(backend)
        .flatMap: response =>
          response.code match
            case StatusCode.Ok =>
              response.body match
                case Right(stream) => ZIO.succeed(Some(stream))
                case Left(err)     =>
                  ZIO.succeed {
                    log.warn(s"GET SSE stream returned non-stream body: $err")
                    None
                  }
            case StatusCode.MethodNotAllowed =>
              drainBody(response) *> ZIO.succeed {
                log.info("Server does not support GET SSE stream")
                None
              }
            case other =>
              drainBody(response) *> ZIO.succeed {
                log.warn(s"GET SSE stream returned HTTP ${other.code}; not reconnecting")
                None
              }

  private def cancelled(id: RequestId): JSONRPCMessage.Error =
    JSONRPCMessage.Error(id = id, error = JSONRPCErrorObject(code = JSONRPCErrorCodes.InvocationError.code, message = "Request cancelled"))

  private def sseEnded(id: RequestId): JSONRPCMessage.Error =
    JSONRPCMessage.Error(
      id = id,
      error = JSONRPCErrorObject(code = JSONRPCErrorCodes.InvocationError.code, message = "SSE stream ended before response")
    )

object ZioStreamingHttpTransport:

  val defaultReconnectSchedule: Schedule[Any, Any, Any] =
    Schedule.exponential(Duration.fromMillis(100)).jittered || Schedule.spaced(Duration.fromSeconds(30))

  def apply(
      backend: StreamBackend[Task, ZioStreams],
      uri: Uri,
      protocolVersion: ProtocolVersion = ProtocolVersion.Latest,
      timeout: FiniteDuration = Transport.defaultTimeout,
      reconnectSchedule: Schedule[Any, Any, Any] = defaultReconnectSchedule
  ): Task[ZioStreamingHttpTransport] =
    for
      scope <- Scope.make
      sessionRef <- Ref.make(Option.empty[String])
      sessionReady <- Promise.make[Nothing, Unit]
      pending <- ZioPendingRequests.make
      incomingRef <- Ref.make[JSONRPCMessage => Task[Unit]](_ => ZIO.unit)
      lastEventId <- Ref.make(Option.empty[String])
      closingRef <- Ref.make(false)
      transport = new ZioStreamingHttpTransport(
        backend,
        uri,
        protocolVersion,
        timeout,
        reconnectSchedule,
        scope,
        sessionRef,
        sessionReady,
        pending,
        incomingRef,
        lastEventId,
        closingRef
      )
      _ <- transport.startGetSseListener
    yield transport

  def scoped(
      backend: StreamBackend[Task, ZioStreams],
      uri: Uri,
      protocolVersion: ProtocolVersion = ProtocolVersion.Latest,
      timeout: FiniteDuration = Transport.defaultTimeout,
      reconnectSchedule: Schedule[Any, Any, Any] = defaultReconnectSchedule
  ): ZIO[Scope, Throwable, ZioStreamingHttpTransport] =
    ZIO.acquireRelease(apply(backend, uri, protocolVersion, timeout, reconnectSchedule))(_.close().ignore)

  def layer(
      backend: StreamBackend[Task, ZioStreams],
      uri: Uri,
      protocolVersion: ProtocolVersion = ProtocolVersion.Latest,
      timeout: FiniteDuration = Transport.defaultTimeout,
      reconnectSchedule: Schedule[Any, Any, Any] = defaultReconnectSchedule
  ): ZLayer[Any, Throwable, ZioStreamingHttpTransport] =
    ZLayer.scoped(scoped(backend, uri, protocolVersion, timeout, reconnectSchedule))
