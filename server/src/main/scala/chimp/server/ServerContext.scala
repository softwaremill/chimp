package chimp.server

import chimp.protocol.*
import io.circe.Json
import io.circe.syntax.*
import sttp.monad.MonadError

/** What a request tells about itself. A modern (2026-07-28+) request carries all of it in `_meta`. A legacy request gives only the protocol
  * version (from the `MCP-Protocol-Version` header or `_meta`) and the progress token; the server keeps no state from `initialize`.
  */
final case class RequestInfo(
    protocolVersion: Option[ProtocolVersion] = None,
    clientCapabilities: Option[ClientCapabilities] = None,
    clientInfo: Option[Implementation] = None,
    logLevel: Option[LoggingLevel] = None,
    progressToken: Option[ProgressToken] = None
):
  /** A modern request gets `notifications/message` only at or above its `logLevel`, and none without one. A legacy request gets all. */
  private[server] def allowsLog(level: LoggingLevel): Boolean =
    if protocolVersion.exists(_.isModern) then logLevel.exists(minimum => level.ordinal >= minimum.ordinal) else true

object RequestInfo:
  val empty: RequestInfo = RequestInfo()

trait ServerContext[F[_]]:
  def request: RequestInfo

object ServerContext:
  def noop[F[_]]: ServerContext[F] = apply(RequestInfo.empty)

  private[server] def apply[F[_]](info: RequestInfo): ServerContext[F] = new ServerContext[F]:
    def request: RequestInfo = info

/** A context which also gives the principal that the security logic of a [[SecuredMcpServer]] made from the request. */
trait SecuredServerContext[F[_], +P] extends ServerContext[F]:
  def principal: P

object SecuredServerContext:
  def apply[F[_], P](value: P, info: RequestInfo = RequestInfo.empty): SecuredServerContext[F, P] = new SecuredServerContext[F, P]:
    def principal: P = value
    def request: RequestInfo = info

trait StreamingServerContext[F[_]] extends ServerContext[F]:
  def reportProgress(progress: Double, total: Option[Double] = None, message: Option[String] = None): F[Unit]

  /** Sends `notifications/message`. For a modern request it sends only at or above the request's `logLevel`, and nothing without one. */
  def log(level: LoggingLevel, data: Json, logger: Option[String] = None): F[Unit]

private[server] final class SinkStreamingServerContext[F[_]](sink: OutboundSink[F], val request: RequestInfo)(using
    m: MonadError[F]
) extends StreamingServerContext[F]:
  def reportProgress(progress: Double, total: Option[Double] = None, message: Option[String] = None): F[Unit] =
    request.progressToken match
      case Some(token) =>
        sink.send(
          JSONRPCMessage.Notification(
            method = "notifications/progress",
            params = Some(ProgressParams(token, progress, total, message).asJson)
          )
        )
      case None => m.unit(())

  def log(level: LoggingLevel, data: Json, logger: Option[String] = None): F[Unit] =
    if request.allowsLog(level) then
      sink.send(
        JSONRPCMessage.Notification(method = "notifications/message", params = Some(LoggingMessageParams(level, data, logger).asJson))
      )
    else m.unit(())

/** A context which gives both the principal that the security logic of a [[SecuredMcpServer]] made from the request, and the streaming
  * capabilities of a [[StreamingServerContext]].
  */
trait SecuredStreamingServerContext[F[_], +P] extends SecuredServerContext[F, P] with StreamingServerContext[F]

private[server] final class SinkSecuredStreamingServerContext[F[_], P](
    sink: OutboundSink[F],
    val request: RequestInfo,
    val principal: P
)(using MonadError[F])
    extends SecuredStreamingServerContext[F, P]:
  private val delegate = SinkStreamingServerContext[F](sink, request)
  def reportProgress(progress: Double, total: Option[Double] = None, message: Option[String] = None): F[Unit] =
    delegate.reportProgress(progress, total, message)
  def log(level: LoggingLevel, data: Json, logger: Option[String] = None): F[Unit] = delegate.log(level, data, logger)
