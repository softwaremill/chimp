package chimp.server

import chimp.protocol.*
import io.circe.Json
import io.circe.syntax.*
import sttp.monad.MonadError

trait ServerContext[F[_]]

object ServerContext:
  def noop[F[_]]: ServerContext[F] = new ServerContext[F] {}

/** A [[ServerContext]] for a tool running as a task, letting it request input from the client mid-execution (Tasks extension,
  * experimental). Requesting input moves the task to `input_required`, surfacing `key -> request` via `tasks/get`; the tool resumes with
  * the response once the client answers with `tasks/update`.
  */
trait TaskContext[F[_]] extends ServerContext[F]:
  def requestInput(key: String, request: Json): F[Json]

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
