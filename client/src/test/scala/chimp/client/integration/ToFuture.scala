package chimp.client.integration

import sttp.monad.MonadError
import sttp.monad.syntax.*

import scala.concurrent.Future

trait ToFuture[F[_]]:
  given monad: MonadError[F]
  def toFuture[A](fa: F[A]): Future[A]

  def waitUntil(condition: => Boolean, attempts: Int, intervalMs: Long): F[Unit] =
    if condition || attempts <= 0 then monad.unit(())
    else monad.eval { Thread.sleep(intervalMs); () }.flatMap(_ => waitUntil(condition, attempts - 1, intervalMs))
