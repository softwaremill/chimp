package chimp.server

import chimp.protocol.*
import io.circe.Json
import io.circe.syntax.*
import sttp.monad.MonadError

trait ServerContext[F[_]]

object ServerContext:
  def noop[F[_]]: ServerContext[F] = new ServerContext[F] {}

/** A context which also gives the principal that the security logic of a [[SecuredMcpServer]] made from the request. */
trait SecuredServerContext[F[_], +P] extends ServerContext[F]:
  def principal: P

object SecuredServerContext:
  def apply[F[_], P](value: P): SecuredServerContext[F, P] = new SecuredServerContext[F, P]:
    def principal: P = value

trait StreamingServerContext[F[_]] extends ServerContext[F]:
  def reportProgress(progress: Double, total: Option[Double] = None, message: Option[String] = None): F[Unit]
  def log(level: LoggingLevel, data: Json, logger: Option[String] = None): F[Unit]

private[server] final class SinkStreamingServerContext[F[_]](sink: OutboundSink[F], progressToken: Option[ProgressToken])(using
    m: MonadError[F]
) extends StreamingServerContext[F]:
  def reportProgress(progress: Double, total: Option[Double] = None, message: Option[String] = None): F[Unit] =
    progressToken match
      case Some(token) =>
        sink.send(
          JSONRPCMessage.Notification(
            method = "notifications/progress",
            params = Some(ProgressParams(token, progress, total, message).asJson)
          )
        )
      case None => m.unit(())

  def log(level: LoggingLevel, data: Json, logger: Option[String] = None): F[Unit] =
    sink.send(
      JSONRPCMessage.Notification(method = "notifications/message", params = Some(LoggingMessageParams(level, data, logger).asJson))
    )
