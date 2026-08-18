package chimp.client.transport.pekko

import chimp.client.transport.ClientHttpTransport.HttpOutcome
import chimp.client.transport.pekko.internal.{PekkoPendingRequests, StateActor}
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
    pending: PekkoPendingRequests
)(using mat: Materializer)
    extends ClientStreamingHttpTransport[Future, PekkoStreams](backend, uri, PekkoStreams, headers):

  private val log = LoggerFactory.getLogger(classOf[PekkoClientHttpTransport])

  private given ExecutionContext = mat.executionContext

  private final class State:
    var sessionId: Option[String] = None
    var incoming: JSONRPCMessage => Future[Unit] = _ => Future.unit
    var closing: Boolean = false
    var serverSupportsGet: Boolean = true

    private var openStreams: Set[KillSwitch] = Set.empty
    private var streams: Long = 0
    private var lastEventIds: Map[Long, String] = Map.empty
    private var initialStreams: Map[Long, Source[ByteString, Any]] = Map.empty

    def beginClosing(): Boolean =
      if closing then false
      else
        closing = true
        true

    def captureSessionId(id: Option[String]): Option[String] =
      id.foreach(value => sessionId = Some(value))
      sessionId

    def takeSessionId(): Option[String] =
      val id = sessionId
      sessionId = None
      id

    def addStream(killSwitch: KillSwitch): Unit = openStreams += killSwitch

    def removeStream(killSwitch: KillSwitch): Unit = openStreams -= killSwitch

    def takeStreams(): Set[KillSwitch] =
      val all = openStreams
      openStreams = Set.empty
      all

    def nextStream(initial: Option[Source[ByteString, Any]]): Long =
      streams += 1
      initial.foreach(stream => initialStreams += streams -> stream)
      streams

    def takeInitialStream(stream: Long): Option[Source[ByteString, Any]] =
      val initial = initialStreams.get(stream)
      initialStreams -= stream
      initial

    def lastEventId(stream: Long): Option[String] = lastEventIds.get(stream)

    def setLastEventId(stream: Long, eventId: String): Unit = lastEventIds += stream -> eventId

    def forgetStream(stream: Long): Unit =
      lastEventIds -= stream
      initialStreams -= stream

  private val state = StateActor(new State, "chimp-mcp-client-http-transport")
  private val sessionReady = Promise[Unit]()

  override given monad: MonadError[Future] = backend.monad

  override def send(msg: JSONRPCMessage): Future[Option[JSONRPCMessage]] =
    state
      .ask(_.closing)
      .flatMap:
        case true  => Future.failed(McpTransportException("HTTP transport is closed"))
        case false =>
          msg match
            case request: JSONRPCMessage.Request =>
              pending
                .register(request.id, timeout)
                .flatMap(await => sendRequest(request, await))
                .andThen { case _ => pending.complete(request.id, cancelled(request.id)) }
            case other => sendNonRequest(other)

  override def onIncoming(handler: JSONRPCMessage => Future[Unit]): Future[Unit] =
    state.tell(_.incoming = handler)
    Future.unit

  override def close(): Future[Unit] =
    state
      .ask(_.beginClosing())
      .flatMap:
        case false => Future.unit
        case true  =>
          val _ = sessionReady.trySuccess(())
          for
            openStreams <- state.ask(_.takeStreams())
            _ = openStreams.foreach(_.shutdown())
            session <- state.ask(_.takeSessionId())
            _ <- session.fold(Future.unit)(deleteSession)
            _ <- pending.closeAll("Transport closed")
          yield
            pending.stop()
            state.stopWhenIdle()

  private def deleteSession(id: String): Future[Unit] =
    ClientHttpTransport
      .baseDeleteRequest(uri, protocolVersion, id, headers)
      .response(asStreamUnsafe(PekkoStreams))
      .send(backend)
      .flatMap(drainBody)
      .recover { case NonFatal(_) => () }

  private def sendRequest(request: JSONRPCMessage.Request, await: () => Future[JSONRPCMessage]): Future[Option[JSONRPCMessage]] =
    post(request).flatMap: response =>
      captureSession(response).flatMap: session =>
        ClientHttpTransport.resolveResponse(response, session) match
          case Left(err: McpSessionNotFoundException) =>
            state.tell(_.sessionId = None)
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
      captureSession(response).flatMap: session =>
        ClientHttpTransport.resolveResponse(response, session) match
          case Left(err: McpSessionNotFoundException) =>
            state.tell(_.sessionId = None)
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
    state
      .ask(_.sessionId)
      .flatMap: session =>
        ClientHttpTransport
          .basePostRequest(uri, protocolVersion, session, ClientTransport.encode(msg), headers)
          .response(asStreamUnsafe(PekkoStreams))
          .send(backend)

  private def captureSession(response: Response[?]): Future[Option[String]] =
    state
      .ask(_.captureSessionId(response.header("Mcp-Session-Id")))
      .map: session =>
        val _ = sessionReady.trySuccess(())
        session

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
    case other                             => state.ask(_.incoming).flatMap(handler => handler(other))

  private def forkSseDrain(stream: Source[ByteString, Any], requestId: Option[RequestId]): Unit =
    val shouldResume: () => Future[Boolean] = requestId match
      case Some(id) => () => stillPending(id)
      case None     => () => Future.successful(false)
    val drained = state
      .ask(_.nextStream(Some(stream)))
      .flatMap(stream => drainSse(stream, shouldResume).andThen { case _ => state.tell(_.forgetStream(stream)) })
    drained.onComplete: _ =>
      requestId.foreach(id => pending.complete(id, sseEnded(id)))

  private def stillPending(requestId: RequestId): Future[Boolean] =
    state
      .ask(_.closing)
      .flatMap:
        case true  => Future.successful(false)
        case false => pending.isPending(requestId)

  private[pekko] def startGetListener(): Unit =
    val listener = sessionReady.future
      .flatMap(_ => state.ask(_.closing))
      .flatMap:
        case true  => Future.unit
        case false => state.ask(_.nextStream(None)).flatMap(stream => drainSse(stream, () => state.ask(!_.closing)))
    listener.failed.foreach(t => log.warn(s"GET SSE listener failed: ${t.getMessage}"))

  private def drainSse(stream: Long, shouldResume: () => Future[Boolean]): Future[Unit] =
    val drained = Promise[Unit]()

    def loop(backoff: FiniteDuration): Unit =
      runSse(resumableSse(stream, shouldResume), stream)
        .recover:
          case NonFatal(t) => log.warn(s"SSE drain error: ${t.getMessage}")
        .flatMap(_ => shouldResume())
        .flatMap:
          case false => Future.successful(false)
          case true  => state.ask(_.serverSupportsGet)
        .onComplete:
          case Success(true) => val _ = mat.scheduleOnce(backoff, () => loop(nextBackoff(backoff)))
          case _             => val _ = drained.trySuccess(())

    loop(reconnectSettings.minBackoff)
    drained.future

  private def resumableSse(stream: Long, shouldResume: () => Future[Boolean]): Source[ServerSentEvent, NotUsed] =
    RestartSource.onFailuresWithBackoff(reconnectSettings): () =>
      Source.futureSource(nextSseStream(stream, shouldResume)).via(PekkoHttpServerSentEvents.parse)

  private def nextSseStream(stream: Long, shouldResume: () => Future[Boolean]): Future[Source[ByteString, Any]] =
    state
      .ask(_.takeInitialStream(stream))
      .flatMap:
        case Some(initial) => Future.successful(initial)
        case None          =>
          shouldResume().flatMap:
            case false => Future.successful(Source.empty[ByteString])
            case true  => state.ask(_.lastEventId(stream)).flatMap(openGetSseStream).map(_.getOrElse(Source.empty[ByteString]))

  private def runSse(source: Source[ServerSentEvent, NotUsed], stream: Long): Future[Unit] =
    val (killSwitch, done) = source
      .viaMat(KillSwitches.single)(Keep.right)
      .mapAsync(1)(event => dispatch(event, stream))
      .toMat(Sink.ignore)(Keep.both)
      .run()
    state.tell(_.addStream(killSwitch))
    done
      .map(_ => ())
      .andThen { case _ => state.tell(_.removeStream(killSwitch)) }

  private def dispatch(event: ServerSentEvent, stream: Long): Future[Unit] =
    event.id.filter(_.nonEmpty).foreach(id => state.tell(_.setLastEventId(stream, id)))
    event.data.filter(_.nonEmpty) match
      case Some(data) =>
        ClientTransport.decode(data) match
          case Right(msg) => routeMessage(msg)
          case Left(_)    => Future.unit
      case None => Future.unit

  private def openGetSseStream(lastEventId: Option[String]): Future[Option[Source[ByteString, Any]]] =
    state
      .ask(_.sessionId)
      .flatMap: session =>
        ClientHttpTransport
          .baseGetRequest(uri, protocolVersion, session, lastEventId, headers)
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
                state.tell(_.serverSupportsGet = false)
                drainBody(response).map: _ =>
                  log.info("Server does not support GET SSE stream")
                  None
              case other =>
                state.tell(_.serverSupportsGet = false)
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
      PekkoPendingRequests()
    )
    transport.startGetListener()
    transport
