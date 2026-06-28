package chimp.server

import sttp.monad.{IdentityMonad, MonadError}
import sttp.shared.Identity

import scala.concurrent.Future

trait SyncToFuture extends ToFuture[Identity]:
  override given monad: MonadError[Identity] = IdentityMonad
  override def toFuture[A](fa: Identity[A]): Future[A] = Future.successful(fa)
  override def sleep(millis: Long): Identity[Unit] = Thread.sleep(millis)
