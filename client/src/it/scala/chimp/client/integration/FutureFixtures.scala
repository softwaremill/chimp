package chimp.client.integration

import sttp.monad.MonadError

import scala.concurrent.Future

trait FutureFixtures[F[_]]:
  given monad: MonadError[F]
  def toFuture[A](fa: F[A]): Future[A]
