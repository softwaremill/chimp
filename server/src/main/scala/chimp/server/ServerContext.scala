package chimp.server

import chimp.protocol.*
import io.circe.Json
import io.circe.syntax.*
import sttp.monad.MonadError

trait ServerContext[F[_]]:
  def isCancelled: F[Boolean]
  def onCancel(action: F[Unit]): F[Unit]

object ServerContext:
  def noop[F[_]](using m: MonadError[F]): ServerContext[F] = new ServerContext[F]:
    def isCancelled: F[Boolean] = m.unit(false)
    def onCancel(action: F[Unit]): F[Unit] = m.unit(())

trait StreamingServerContext[F[_]] extends ServerContext[F]:
  def reportProgress(progress: Double, total: Option[Double] = None, message: Option[String] = None): F[Unit]
  def log(level: LoggingLevel, data: Json, logger: Option[String] = None): F[Unit]
  def sample(params: CreateMessageParams): F[CreateMessageResult]
  def elicit(params: ElicitParams): F[ElicitResult]

private[server] final class SinkStreamingServerContext[F[_]](sink: OutboundSink[F], progressToken: Option[ProgressToken])(using
    m: MonadError[F]
) extends StreamingServerContext[F]:
  def isCancelled: F[Boolean] = m.unit(false)
  def onCancel(action: F[Unit]): F[Unit] = m.unit(())

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

  def sample(params: CreateMessageParams): F[CreateMessageResult] =
    m.error(UnsupportedOperationException("server client sampling is not yet supported"))

  def elicit(params: ElicitParams): F[ElicitResult] =
    m.error(UnsupportedOperationException("server client elicitation is not yet supported"))
