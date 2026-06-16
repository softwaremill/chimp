package chimp.server

import sttp.monad.MonadError
import sttp.monad.syntax.*

import scala.concurrent.Future

/** Bridges an effect `F` to `scala.concurrent.Future` so the same effect-polymorphic test bodies can run under `AsyncFlatSpec` for any
  * effect (e.g. `Identity`, `Task`). Mirrors the client test suite's `ToFuture`.
  */
trait ToFuture[F[_]]:
  given monad: MonadError[F]
  def toFuture[A](fa: F[A]): Future[A]
  def sleep(millis: Long): F[Unit]

  /** Re-checks `condition` (which is expected to flip via a side effect) until it holds or attempts run out. */
  def waitUntil(condition: => Boolean, attempts: Int = 100, intervalMs: Long = 20): F[Unit] =
    if condition || attempts <= 0 then monad.unit(())
    else sleep(intervalMs).flatMap(_ => waitUntil(condition, attempts - 1, intervalMs))
