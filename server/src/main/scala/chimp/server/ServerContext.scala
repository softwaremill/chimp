package chimp.server

import chimp.protocol.{CreateMessageParams, CreateMessageResult, ElicitParams, ElicitResult, LoggingLevel}
import io.circe.Json
import sttp.monad.MonadError

trait ServerContext[F[_]]:
  def isCancelled: F[Boolean]
  def onCancel(action: F[Unit]): F[Unit]

object ServerContext:
  def noOp[F[_]](using m: MonadError[F]): ServerContext[F] = new ServerContext[F]:
    def isCancelled: F[Boolean] = m.unit(false)
    def onCancel(action: F[Unit]): F[Unit] = m.unit(())

/** Adds the server→client interactions that require a live streaming connection: emitting progress and log notifications, and issuing
  * sampling and elicitation requests. Tool logic using these is accepted only by the streaming endpoint; the request/response endpoint
  * rejects it at compile time.
  */
trait StreamingServerContext[F[_]] extends ServerContext[F]:
  def reportProgress(progress: Double, total: Option[Double] = None, message: Option[String] = None): F[Unit]
  def log(level: LoggingLevel, data: Json, logger: Option[String] = None): F[Unit]
  def sample(params: CreateMessageParams): F[CreateMessageResult]
  def elicit(params: ElicitParams): F[ElicitResult]
