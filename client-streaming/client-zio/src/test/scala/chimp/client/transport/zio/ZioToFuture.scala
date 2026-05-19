package chimp.client.transport.zio

import chimp.client.integration.ToFuture
import sttp.client4.impl.zio.RIOMonadAsyncError
import sttp.monad.MonadError
import zio.{Runtime, Task, Unsafe}

import scala.concurrent.Future

trait ZioToFuture extends ToFuture[Task]:
  override given monad: MonadError[Task] = new RIOMonadAsyncError[Any]

  private val runtime: Runtime[Any] = Runtime.default

  override def toFuture[A](fa: Task[A]): Future[A] =
    Unsafe.unsafe { implicit u =>
      runtime.unsafe.runToFuture(fa)
    }
