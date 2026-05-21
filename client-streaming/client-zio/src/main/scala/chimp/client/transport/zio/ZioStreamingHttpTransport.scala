package chimp.client.transport.zio

import chimp.client.transport.HttpTransport.HttpOutcome
import chimp.client.transport.{HttpTransport, StreamingHttpTransport, Transport}
import chimp.client.{McpProtocolException, McpSessionNotFoundException}
import chimp.protocol.{JSONRPCErrorObject, JSONRPCMessage, ProtocolVersion, RequestId}
import org.slf4j.LoggerFactory
import sttp.capabilities.zio.ZioStreams
import sttp.client4.{asStreamUnsafe, basicRequest, Response, StreamBackend}
import sttp.model.sse.ServerSentEvent
import sttp.model.{MediaType, StatusCode, Uri}
import sttp.monad.MonadError
import zio.stream.{Stream, ZPipeline, ZStream}
import zio.{Exit, Promise, Ref, Scope, Task, ZIO, ZLayer}

import scala.util.chaining.*

final class ZioStreamingHttpTransport private (
    backend: StreamBackend[Task, ZioStreams],
    uri: Uri,
    protocolVersion: ProtocolVersion,
    scope: Scope.Closeable,
    sessionRef: Ref[Option[String]],
    sessionReady: Promise[Nothing, Unit],
    pending: ZioPendingRequests,
    incomingRef: Ref[JSONRPCMessage => Task[Unit]]
) extends StreamingHttpTransport[Task, ZioStreams](backend, uri, ZioStreams):

  private val log = LoggerFactory.getLogger(classOf[ZioStreamingHttpTransport])

  override given monad: MonadError[Task] = backend.monad

  override def send(msg: JSONRPCMessage): Task[Option[JSONRPCMessage]] =
    msg match
      case request: JSONRPCMessage.Request =>
        pending
          .register(request.id)
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
    deleteSession *> scope.close(Exit.unit).ignore

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
                _ <- pending.complete(request.id, msg)
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
      case Right(stream) => stream.via(ZPipeline.utf8Decode).runFold("")(_ + _)

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
        val drain = parseSseEvents(stream)
          .mapZIO(dispatch)
          .runDrain
          .catchAll(t => ZIO.succeed(log.warn(s"SSE drain failed: ${t.getMessage}")))
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
        if line.isEmpty then (Nil, Some(ServerSentEvent.parse(buffered)))
        else (buffered :+ line, None)
      .collect { case Some(event) => event }

  private def dispatch(event: ServerSentEvent): Task[Unit] =
    event.data match
      case Some(data) if data.nonEmpty =>
        Transport.decode(data) match
          case Right(msg) =>
            msg match
              case response: JSONRPCMessage.Response => pending.complete(response.id, response).unit
              case err: JSONRPCMessage.Error         => pending.complete(err.id, err).unit
              case other                             => incomingRef.get.flatMap(_(other))
          case Left(_) => ZIO.unit
      case _ => ZIO.unit

  private[zio] def startGetListener: Task[Unit] =
    val listener = sessionReady.await *> runGetStream
    listener.catchAll(t => ZIO.succeed(log.warn(s"GET listener failed: ${t.getMessage}"))).forkIn(scope).unit

  private def runGetStream: Task[Unit] =
    sessionRef.get.flatMap: session =>
      basicRequest
        .get(uri)
        .header("Accept", MediaType.TextEventStream.toString)
        .header("MCP-Protocol-Version", protocolVersion.name)
        .response(asStreamUnsafe(ZioStreams))
        .pipe { request =>
          session match
            case Some(sessionId) => request.header("Mcp-Session-Id", sessionId)
            case _               => request
        }
        .send(backend)
        .flatMap: response =>
          if response.code == StatusCode.MethodNotAllowed then ZIO.unit
          else
            response.body match
              case Left(_)       => ZIO.unit
              case Right(stream) => parseSseEvents(stream).mapZIO(dispatch).runDrain

  private def cancelled(id: RequestId): JSONRPCMessage.Error =
    JSONRPCMessage.Error(id = id, error = JSONRPCErrorObject(code = -32000, message = "request cancelled"))

  private def sseEnded(id: RequestId): JSONRPCMessage.Error =
    JSONRPCMessage.Error(id = id, error = JSONRPCErrorObject(code = -32000, message = "SSE stream ended before response"))

object ZioStreamingHttpTransport:
  def apply(
      backend: StreamBackend[Task, ZioStreams],
      uri: Uri,
      protocolVersion: ProtocolVersion = ProtocolVersion.Latest
  ): Task[ZioStreamingHttpTransport] =
    for
      scope <- Scope.make
      sessionRef <- Ref.make(Option.empty[String])
      sessionReady <- Promise.make[Nothing, Unit]
      pending <- ZioPendingRequests.make
      incomingRef <- Ref.make[JSONRPCMessage => Task[Unit]](_ => ZIO.unit)
      transport = new ZioStreamingHttpTransport(backend, uri, protocolVersion, scope, sessionRef, sessionReady, pending, incomingRef)
      _ <- transport.startGetListener
    yield transport

  def scoped(
      backend: StreamBackend[Task, ZioStreams],
      uri: Uri,
      protocolVersion: ProtocolVersion = ProtocolVersion.Latest
  ): ZIO[Scope, Throwable, ZioStreamingHttpTransport] =
    ZIO.acquireRelease(apply(backend, uri, protocolVersion))(_.close().ignore)

  def layer(
      backend: StreamBackend[Task, ZioStreams],
      uri: Uri,
      protocolVersion: ProtocolVersion = ProtocolVersion.Latest
  ): ZLayer[Any, Throwable, ZioStreamingHttpTransport] =
    ZLayer.scoped(scoped(backend, uri, protocolVersion))
