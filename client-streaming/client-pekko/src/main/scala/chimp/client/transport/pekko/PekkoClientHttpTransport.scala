package chimp.client.transport.pekko

import chimp.client.transport.ClientHttpTransport.HttpOutcome
import chimp.client.transport.pekko.internal.PekkoPendingRequests
import chimp.client.transport.{ClientHttpTransport, ClientStreamingHttpTransport, ClientTransport}
import chimp.client.{McpProtocolException, McpSessionNotFoundException, McpTransportException}
import chimp.protocol.{JSONRPCErrorCodes, JSONRPCErrorObject, JSONRPCMessage, ProtocolVersion, RequestId}
import org.apache.pekko.NotUsed
import org.apache.pekko.stream.scaladsl.{Keep, RestartSource, Sink, Source}
import org.apache.pekko.stream.{KillSwitch, KillSwitches, Materializer, RestartSettings}
import org.apache.pekko.util.ByteString
import org.slf4j.LoggerFactory
import sttp.capabilities.pekko.PekkoStreams
import sttp.client4.pekkohttp.PekkoHttpServerSentEvents
import sttp.client4.{asStreamUnsafe, Response, StreamBackend}
import sttp.model.sse.ServerSentEvent
import sttp.model.{Header, StatusCode, Uri}
import sttp.monad.MonadError

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Success
import scala.util.control.NonFatal

final class PekkoClientHttpTransport private (
    backend: StreamBackend[Future, PekkoStreams],
    uri: Uri,
    protocolVersion: ProtocolVersion,
    timeout: FiniteDuration,
    headers: Seq[Header],
    reconnectSettings: RestartSettings,
    pending: PekkoPendingRequests,
    openStreams: java.util.Set[KillSwitch]
)(using mat: Materializer)
    extends ClientStreamingHttpTransport[Future, PekkoStreams](backend, uri, PekkoStreams, headers):

  private val log = LoggerFactory.getLogger(classOf[PekkoClientHttpTransport])

  private given ExecutionContext = mat.executionContext

  private val sessionId = AtomicReference[Option[String]](None)
  private val incoming = AtomicReference[JSONRPCMessage => Future[Unit]](_ => Future.unit)
  private val lastEventId = AtomicReference[Option[String]](None)
  private val closing = AtomicBoolean(false)
  private val serverSupportsGet = AtomicBoolean(true)
  private val sessionReady = Promise[Unit]()

  override given monad: MonadError[Future] = backend.monad

  override def send(msg: JSONRPCMessage): Future[Option[JSONRPCMessage]] =
    if closing.get() then Future.failed(McpTransportException("HTTP transport is closed"))
    else
      msg match
        case request: JSONRPCMessage.Request =>
          pending
            .register(request.id, timeout)
            .flatMap(await => sendRequest(request, await))
            .andThen { case _ => pending.complete(request.id, cancelled(request.id)) }
        case other => sendNonRequest(other)

  override def onIncoming(handler: JSONRPCMessage => Future[Unit]): Future[Unit] =
    incoming.set(handler)
    Future.unit

  override def close(): Future[Unit] =
    if !closing.compareAndSet(false, true) then Future.unit
    else
      val _ = sessionReady.trySuccess(())
      openStreams.forEach(_.shutdown())
      val deleteSession = sessionId.getAndSet(None) match
        case Some(id) =>
          ClientHttpTransport
            .baseDeleteRequest(uri, protocolVersion, id, headers)
            .response(asStreamUnsafe(PekkoStreams))
            .send(backend)
            .flatMap(drainBody)
            .recover { case NonFatal(_) => () }
        case None => Future.unit
      deleteSession.flatMap(_ => pending.closeAll("Transport closed"))

  private def sendRequest(request: JSONRPCMessage.Request, await: () => Future[JSONRPCMessage]): Future[Option[JSONRPCMessage]] =
    post(request).flatMap: response =>
      captureSession(response)
      ClientHttpTransport.resolveResponse(response, sessionId.get()) match
        case Left(err: McpSessionNotFoundException) =>
          sessionId.set(None)
          Future.failed(err)
        case Left(err) =>
          Future.failed(err)
        case Right(HttpOutcome.NoBody) =>
          drainBody(response).flatMap(_ => Future.failed(McpProtocolException("Server returned 202 Accepted for a Request")))
        case Right(HttpOutcome.JsonBody) =>
          for
            body <- collectBody(response)
            msg <- decode(body)
            _ <- routeMessage(msg)
            out <- await()
          yield Some(out)
        case Right(HttpOutcome.SseBody) =>
          response.body match
            case Left(err)     => Future.failed(McpProtocolException(s"Expected SSE stream, got: $err"))
            case Right(stream) =>
              forkSseDrain(stream, Some(request.id))
              await().map(Some(_))

  private def sendNonRequest(msg: JSONRPCMessage): Future[Option[JSONRPCMessage]] =
    post(msg).flatMap: response =>
      captureSession(response)
      ClientHttpTransport.resolveResponse(response, sessionId.get()) match
        case Left(err: McpSessionNotFoundException) =>
          sessionId.set(None)
          Future.failed(err)
        case Left(err)                   => Future.failed(err)
        case Right(HttpOutcome.NoBody)   => drainBody(response).map(_ => None)
        case Right(HttpOutcome.JsonBody) => drainBody(response).map(_ => None)
        case Right(HttpOutcome.SseBody)  =>
          response.body match
            case Left(_)       => Future.successful(None)
            case Right(stream) =>
              forkSseDrain(stream, None)
              Future.successful(None)

  private def post(msg: JSONRPCMessage): Future[Response[Either[String, Source[ByteString, Any]]]] =
    ClientHttpTransport
      .basePostRequest(uri, protocolVersion, sessionId.get(), ClientTransport.encode(msg), headers)
      .response(asStreamUnsafe(PekkoStreams))
      .send(backend)

  private def captureSession(response: Response[?]): Unit =
    response.header("Mcp-Session-Id").foreach(id => sessionId.set(Some(id)))
    val _ = sessionReady.trySuccess(())

  private def collectBody(response: Response[Either[String, Source[ByteString, Any]]]): Future[String] =
    response.body match
      case Left(err)     => Future.failed(McpProtocolException(s"HTTP 200 with non-stream body: $err"))
      case Right(stream) => stream.runFold(ByteString.empty)(_ ++ _).map(_.utf8String)

  private def drainBody(response: Response[Either[String, Source[ByteString, Any]]]): Future[Unit] =
    response.body match
      case Left(_)       => Future.unit
      case Right(stream) => stream.runWith(Sink.ignore).map(_ => ()).recover { case NonFatal(_) => () }

  private def decode(body: String): Future[JSONRPCMessage] =
    ClientTransport.decode(body) match
      case Right(msg) => Future.successful(msg)
      case Left(err)  => Future.failed(McpProtocolException(s"Failed to decode response body: ${err.getMessage}, payload $body"))

  private def routeMessage(msg: JSONRPCMessage): Future[Unit] = msg match
    case response: JSONRPCMessage.Response => pending.complete(response.id, response).map(_ => ())
    case err: JSONRPCMessage.Error         => pending.complete(err.id, err).map(_ => ())
    case other                             => incoming.get()(other)

  private def forkSseDrain(stream: Source[ByteString, Any], requestId: Option[RequestId]): Unit =
    val shouldResume: () => Future[Boolean] = requestId match
      case Some(id) => () => if closing.get() then Future.successful(false) else pending.isPending(id)
      case None     => () => Future.successful(false)
    val drained = drainSse(Some(stream), AtomicReference[Option[String]](None), shouldResume)
    drained.onComplete: _ =>
      requestId.foreach(id => pending.complete(id, sseEnded(id)))

  private[pekko] def startGetListener(): Unit =
    val listener = sessionReady.future.flatMap: _ =>
      if closing.get() then Future.unit
      else drainSse(None, lastEventId, () => Future.successful(!closing.get()))
    listener.failed.foreach(t => if !closing.get() then log.warn(s"GET SSE listener failed: ${t.getMessage}"))

  private def drainSse(
      initial: Option[Source[ByteString, Any]],
      lastEventIdRef: AtomicReference[Option[String]],
      shouldResume: () => Future[Boolean]
  ): Future[Unit] =
    val drained = Promise[Unit]()

    def loop(stream: Option[Source[ByteString, Any]], backoff: FiniteDuration): Unit =
      runSse(resumableSse(stream, lastEventIdRef, shouldResume), lastEventIdRef)
        .recover:
          case NonFatal(t) =>
            if !closing.get() then log.warn(s"SSE drain error: ${t.getMessage}")
        .flatMap(_ => shouldResume())
        .onComplete:
          case Success(true) if serverSupportsGet.get() =>
            val _ = mat.scheduleOnce(backoff, () => loop(None, nextBackoff(backoff)))
          case _ => val _ = drained.trySuccess(())

    loop(initial, reconnectSettings.minBackoff)
    drained.future

  private def resumableSse(
      initial: Option[Source[ByteString, Any]],
      lastEventIdRef: AtomicReference[Option[String]],
      shouldResume: () => Future[Boolean]
  ): Source[ServerSentEvent, NotUsed] =
    val first = AtomicReference(initial)
    RestartSource.onFailuresWithBackoff(reconnectSettings): () =>
      first.getAndSet(None) match
        case Some(stream) => stream.via(PekkoHttpServerSentEvents.parse)
        case None         => Source.futureSource(reopenGetSseStream(lastEventIdRef, shouldResume)).via(PekkoHttpServerSentEvents.parse)

  private def reopenGetSseStream(
      lastEventIdRef: AtomicReference[Option[String]],
      shouldResume: () => Future[Boolean]
  ): Future[Source[ByteString, Any]] =
    shouldResume().flatMap:
      case false => Future.successful(Source.empty[ByteString])
      case true  => openGetSseStream(lastEventIdRef.get()).map(_.getOrElse(Source.empty[ByteString]))

  private def runSse(source: Source[ServerSentEvent, NotUsed], lastEventIdRef: AtomicReference[Option[String]]): Future[Unit] =
    val (killSwitch, done) = source
      .viaMat(KillSwitches.single)(Keep.right)
      .mapAsync(1)(event => dispatch(event, lastEventIdRef))
      .toMat(Sink.ignore)(Keep.both)
      .run()
    val _ = openStreams.add(killSwitch)
    done
      .map(_ => ())
      .andThen { case _ => openStreams.remove(killSwitch) }

  private def dispatch(event: ServerSentEvent, lastEventIdRef: AtomicReference[Option[String]]): Future[Unit] =
    event.id.filter(_.nonEmpty).foreach(id => lastEventIdRef.set(Some(id)))
    event.data.filter(_.nonEmpty) match
      case Some(data) =>
        ClientTransport.decode(data) match
          case Right(msg) => routeMessage(msg)
          case Left(_)    => Future.unit
      case None => Future.unit

  private def openGetSseStream(lastEventId: Option[String]): Future[Option[Source[ByteString, Any]]] =
    ClientHttpTransport
      .baseGetRequest(uri, protocolVersion, sessionId.get(), lastEventId, headers)
      .response(asStreamUnsafe(PekkoStreams))
      .send(backend)
      .flatMap: response =>
        response.code match
          case StatusCode.Ok =>
            response.body match
              case Right(stream) => Future.successful(Some(stream))
              case Left(err)     =>
                log.warn(s"GET SSE stream returned non-stream body: $err")
                Future.successful(None)
          case StatusCode.MethodNotAllowed =>
            serverSupportsGet.set(false)
            drainBody(response).map: _ =>
              log.info("Server does not support GET SSE stream")
              None
          case other =>
            serverSupportsGet.set(false)
            drainBody(response).map: _ =>
              log.warn(s"GET SSE stream returned HTTP ${other.code}; not reconnecting")
              None

  private def nextBackoff(backoff: FiniteDuration): FiniteDuration = (backoff * 2).min(reconnectSettings.maxBackoff)

  private def cancelled(id: RequestId): JSONRPCMessage.Error =
    JSONRPCMessage.Error(id = id, error = JSONRPCErrorObject(code = JSONRPCErrorCodes.InvocationError.code, message = "Request cancelled"))

  private def sseEnded(id: RequestId): JSONRPCMessage.Error =
    JSONRPCMessage.Error(
      id = id,
      error = JSONRPCErrorObject(code = JSONRPCErrorCodes.InvocationError.code, message = "SSE stream ended before response")
    )

object PekkoClientHttpTransport:

  val defaultReconnectSettings: RestartSettings =
    RestartSettings(minBackoff = 100.millis, maxBackoff = 30.seconds, randomFactor = 0.2)

  /** Creates a transport and starts listening on the GET Server-Sent Event stream.
    *
    * @param backend
    *   The sttp backend used to send HTTP requests; must support Pekko streams.
    * @param uri
    *   The MCP endpoint URI.
    * @param protocolVersion
    *   Protocol version advertised via the `MCP-Protocol-Version` header; defaults to the latest version supported by chimp.
    * @param timeout
    *   How long to wait for a response to a request sent to the server.
    * @param reconnectSettings
    *   Backoff used when a Server-Sent Event stream must be re-opened.
    * @param headers
    *   Extra headers sent with each request to the server.
    */
  def apply(
      backend: StreamBackend[Future, PekkoStreams],
      uri: Uri,
      protocolVersion: ProtocolVersion = ProtocolVersion.Latest,
      timeout: FiniteDuration = ClientTransport.defaultTimeout,
      reconnectSettings: RestartSettings = defaultReconnectSettings,
      headers: Seq[Header] = Nil
  )(using Materializer): PekkoClientHttpTransport =
    val transport = new PekkoClientHttpTransport(
      backend,
      uri,
      protocolVersion,
      timeout,
      headers,
      reconnectSettings,
      PekkoPendingRequests(),
      ConcurrentHashMap.newKeySet[KillSwitch]()
    )
    transport.startGetListener()
    transport
