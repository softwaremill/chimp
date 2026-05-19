package chimp.client.integration

import sttp.monad.{IdentityMonad, MonadError}
import sttp.shared.Identity

import scala.concurrent.Future

trait SyncFutureFixtures extends FutureFixtures[Identity]:
  override given monad: MonadError[Identity] = IdentityMonad
  override def toFuture[A](fa: Identity[A]): Future[A] = Future.successful(fa)
