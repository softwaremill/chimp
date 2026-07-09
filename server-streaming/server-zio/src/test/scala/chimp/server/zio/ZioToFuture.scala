package chimp.server.zio

import sttp.client4.impl.zio.RIOMonadAsyncError
import sttp.monad.MonadError
import zio.{Duration, Runtime, Task, Unsafe, ZIO}

import scala.concurrent.Future

trait ZioToFuture extends chimp.server.ToFuture[Task]:
  override given monad: MonadError[Task] = new RIOMonadAsyncError[Any]

  private val runtime: Runtime[Any] = Runtime.default

  override def toFuture[A](fa: Task[A]): Future[A] =
    Unsafe.unsafe { implicit u =>
      runtime.unsafe.runToFuture(fa)
    }

  override def sleep(millis: Long): Task[Unit] = ZIO.sleep(Duration.fromMillis(millis))
