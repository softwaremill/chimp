package chimp.client.transport.ox

import chimp.client.internal.SyncPendingRequests
import chimp.client.transport.ClientHttpTransport.HttpOutcome
import chimp.client.transport.{ClientBidirectionalTransport, ClientHttpTransport, ClientTransport}
import chimp.client.{McpProtocolException, McpSessionNotFoundException, McpTransportException}
import chimp.protocol.{JSONRPCErrorCodes, JSONRPCErrorObject, JSONRPCMessage, ProtocolVersion, RequestId}
import org.slf4j.LoggerFactory
import ox.*
import ox.channels.{Actor, ActorRef}
import ox.resilience.{retry, RetryConfig}
import ox.scheduling.Schedule
import sttp.client4.{asInputStreamUnsafe, basicRequest, Response, SyncBackend}
import sttp.model.sse.ServerSentEvent
import sttp.model.{MediaType, StatusCode, Uri}
import sttp.monad.{IdentityMonad, MonadError}
import sttp.shared.Identity

import java.io.{BufferedReader, InputStream, InputStreamReader}
import java.nio.charset.StandardCharsets
import java.util.concurrent.{ConcurrentHashMap, CountDownLatch}
import scala.concurrent.duration.{FiniteDuration, *}

final class OxClientHttpTransport private (
    backend: SyncBackend,
    uri: Uri,
    protocolVersion: ProtocolVersion,
    timeout: FiniteDuration,
    pending: SyncPendingRequests,
    sessionReady: CountDownLatch,
    openStreams: java.util.Set[InputStream]
)(using Ox)
    extends ClientBidirectionalTransport[Identity]:

  private val log = LoggerFactory.getLogger(classOf[OxClientHttpTransport])

  private final class State:
    var sessionId: Option[String] = None
    var incoming: JSONRPCMessage => Unit = _ => ()
    var lastEventId: Option[String] = None
    var closing: Boolean = false

    def beginClosing(): Boolean =
      if closing then false
      else
        closing = true
        true

    def takeSessionId(): Option[String] =
      val id = sessionId
      sessionId = None
      id

  private val state: ActorRef[State] = Actor.create(new State)

  given monad: MonadError[Identity] = IdentityMonad

  override def send(msg: JSONRPCMessage): Identity[Option[JSONRPCMessage]] =
    if state.ask(_.closing) then throw McpTransportException("HTTP transport is closed")
    msg match
      case request: JSONRPCMessage.Request =>
        val await = pending.register(request.id, timeout)
        try sendRequest(request, await)
        finally { val _ = pending.complete(request.id, cancelled(request.id)) }
      case other => sendNonRequest(other)

  override def onIncoming(handler: JSONRPCMessage => Identity[Unit]): Identity[Unit] = state.tell(_.incoming = handler)

  override def close(): Identity[Unit] =
    if state.ask(_.beginClosing()) then
      sessionReady.countDown()
      openStreams.forEach(closeQuietly)
      state.ask(_.takeSessionId()) match
        case Some(id) =>
          try drainBody(ClientHttpTransport.baseDeleteRequest(uri, protocolVersion, id).response(asInputStreamUnsafe).send(backend))
          catch case _: Exception => ()
        case None => ()
      pending.closeAll("Transport closed")

  private def sendRequest(request: JSONRPCMessage.Request, await: () => JSONRPCMessage): Option[JSONRPCMessage] =
    val response = post(request)
    captureSession(response)
    ClientHttpTransport.resolveResponse(response, state.ask(_.sessionId)) match
      case Left(err: McpSessionNotFoundException) => state.tell(_.sessionId = None); throw err
      case Left(err)                              => throw err
      case Right(HttpOutcome.NoBody)              =>
        drainBody(response)
        throw McpProtocolException("Server returned 202 Accepted for a Request")
      case Right(HttpOutcome.JsonBody) =>
        val message = decode(collectBody(response))
        routeMessage(message)
        Some(await())
      case Right(HttpOutcome.SseBody) =>
        response.body match
          case Right(stream) => forkSseDrain(stream, Some(request.id)); Some(await())
          case Left(err)     => throw McpProtocolException(s"Expected SSE stream, got: $err")

  private def sendNonRequest(msg: JSONRPCMessage): Option[JSONRPCMessage] =
    val response = post(msg)
    captureSession(response)
    ClientHttpTransport.resolveResponse(response, state.ask(_.sessionId)) match
      case Left(err: McpSessionNotFoundException) => state.tell(_.sessionId = None); throw err
      case Left(err)                              => throw err
      case Right(HttpOutcome.NoBody)              => drainBody(response); None
      case Right(HttpOutcome.JsonBody)            => drainBody(response); None
      case Right(HttpOutcome.SseBody)             =>
        response.body match
          case Right(stream) => forkSseDrain(stream, None); None
          case Left(_)       => None

  private def post(msg: JSONRPCMessage): Response[Either[String, InputStream]] =
    ClientHttpTransport
      .basePostRequest(uri, protocolVersion, state.ask(_.sessionId), ClientTransport.encode(msg))
      .response(asInputStreamUnsafe)
      .send(backend)

  private def captureSession(response: Response[?]): Unit =
    response.header("Mcp-Session-Id").foreach(id => state.tell(_.sessionId = Some(id)))
    sessionReady.countDown()

  private def collectBody(response: Response[Either[String, InputStream]]): String =
    response.body match
      case Right(stream) =>
        try String(stream.readAllBytes(), StandardCharsets.UTF_8)
        finally closeQuietly(stream)
      case Left(err) => throw McpProtocolException(s"HTTP 200 with non-stream body: $err")

  private def drainBody(response: Response[Either[String, InputStream]]): Unit =
    response.body match
      case Right(stream) =>
        try { val _ = stream.readAllBytes() }
        catch case _: Exception => ()
        finally closeQuietly(stream)
      case Left(_) => ()

  private def decode(body: String): JSONRPCMessage =
    ClientTransport.decode(body) match
      case Right(message) => message
      case Left(err)      => throw McpProtocolException(s"Failed to decode response body: ${err.getMessage}, payload $body")

  private def forkSseDrain(stream: InputStream, requestId: Option[RequestId]): Unit =
    track(stream)
    forkDiscard:
      try drainSse(stream, _ => ())
      catch case e: Exception => if !state.ask(_.closing) then log.warn(s"SSE drain error: ${e.getMessage}")
      finally
        requestId.foreach { id =>
          val _ = pending.complete(id, sseEnded(id))
        }
        untrack(stream)

  private def routeMessage(msg: JSONRPCMessage): Unit = msg match
    case response: JSONRPCMessage.Response => val _ = pending.complete(response.id, response)
    case err: JSONRPCMessage.Error         => val _ = pending.complete(err.id, err)
    case other                             => state.ask(_.incoming)(other)

  private[ox] def startGetListener(): Unit = forkDiscard(getListenerLoop())

  private def getListenerLoop(): Unit =
    sessionReady.await()
    val onFailure: (Int, Either[Throwable, Unit]) => Unit = (_, result) =>
      result match
        case Left(t)  => if !state.ask(_.closing) then log.warn(s"GET SSE listener error: ${t.getMessage}")
        case Right(_) => ()
    val schedule = Schedule.exponentialBackoff(100.millis).jitter().maxInterval(30.seconds)
    retry(RetryConfig[Throwable, Unit](schedule).afterAttempt(onFailure)):
      if !state.ask(_.closing) then
        openGetSseStream(state.ask(_.lastEventId)).foreach: stream =>
          try drainSse(stream, id => state.tell(_.lastEventId = Some(id)))
          finally untrack(stream)

  private def openGetSseStream(lastEvent: Option[String]): Option[InputStream] =
    val base = basicRequest
      .get(uri)
      .header("Accept", MediaType.TextEventStream.toString)
      .header("MCP-Protocol-Version", protocolVersion.name)
      .response(asInputStreamUnsafe)
    val withSession = state.ask(_.sessionId).fold(base)(s => base.header("Mcp-Session-Id", s))
    val withLastEvent = lastEvent.fold(withSession)(id => withSession.header("Last-Event-ID", id))
    val response = withLastEvent.send(backend)
    response.code match
      case StatusCode.Ok =>
        response.body match
          case Right(stream) => track(stream); Some(stream)
          case Left(err)     => log.warn(s"GET SSE stream returned non-stream body: $err"); None
      case StatusCode.MethodNotAllowed =>
        drainBody(response)
        log.info("Server does not support GET SSE stream")
        None
      case other =>
        drainBody(response)
        log.warn(s"GET SSE stream returned HTTP ${other.code}; not reconnecting")
        None

  private def drainSse(stream: InputStream, onEventId: String => Unit): Unit =
    val reader = BufferedReader(InputStreamReader(stream, StandardCharsets.UTF_8))
    var buffered = List.empty[String]
    var line = reader.readLine()
    while line != null do
      if line.isEmpty then
        if buffered.nonEmpty then
          dispatchEvent(ServerSentEvent.parse(buffered.reverse), onEventId)
          buffered = Nil
      else buffered = line :: buffered
      line = reader.readLine()
    if buffered.nonEmpty then dispatchEvent(ServerSentEvent.parse(buffered.reverse), onEventId)

  private def dispatchEvent(event: ServerSentEvent, onEventId: String => Unit): Unit =
    event.id.filter(_.nonEmpty).foreach(onEventId)
    event.data
      .filter(_.nonEmpty)
      .foreach: data =>
        ClientTransport.decode(data) match
          case Right(message) => routeMessage(message)
          case Left(_)        => ()

  private def track(stream: InputStream): Unit = { val _ = openStreams.add(stream) }

  private def untrack(stream: InputStream): Unit =
    if openStreams.remove(stream) then closeQuietly(stream)

  private def closeQuietly(stream: InputStream): Unit =
    try stream.close()
    catch case _: Exception => ()

  private def cancelled(id: RequestId): JSONRPCMessage.Error =
    JSONRPCMessage.Error(id = id, error = JSONRPCErrorObject(code = JSONRPCErrorCodes.InvocationError.code, message = "Request cancelled"))

  private def sseEnded(id: RequestId): JSONRPCMessage.Error =
    JSONRPCMessage.Error(
      id = id,
      error = JSONRPCErrorObject(code = JSONRPCErrorCodes.InvocationError.code, message = "SSE stream ended before response")
    )

object OxClientHttpTransport:

  def apply(
      backend: SyncBackend,
      uri: Uri,
      protocolVersion: ProtocolVersion = ProtocolVersion.Latest,
      timeout: FiniteDuration = ClientTransport.defaultTimeout
  )(using Ox): OxClientHttpTransport =
    val transport = new OxClientHttpTransport(
      backend,
      uri,
      protocolVersion,
      timeout,
      SyncPendingRequests(),
      CountDownLatch(1),
      ConcurrentHashMap.newKeySet[InputStream]()
    )
    transport.startGetListener()
    transport
