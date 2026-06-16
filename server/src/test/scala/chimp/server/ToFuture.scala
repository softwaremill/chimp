package chimp.server

import sttp.monad.MonadError
import sttp.monad.syntax.*

import scala.concurrent.Future

trait ToFuture[F[_]]:
  given monad: MonadError[F]
  def toFuture[A](fa: F[A]): Future[A]
  def sleep(millis: Long): F[Unit]

  def waitUntil(condition: => Boolean, attempts: Int = 100, intervalMs: Long = 20): F[Unit] =
    if condition || attempts <= 0 then monad.unit(())
    else sleep(intervalMs).flatMap(_ => waitUntil(condition, attempts - 1, intervalMs))
