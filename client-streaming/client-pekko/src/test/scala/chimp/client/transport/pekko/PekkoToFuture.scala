package chimp.client.transport.pekko

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.pattern.after
import org.scalatest.{BeforeAndAfterAll, Suite}
import sttp.monad.{FutureMonad, MonadError}

import scala.concurrent.duration.{DurationInt, DurationLong}
import scala.concurrent.{Await, Future}

trait PekkoToFuture extends chimp.client.integration.ToFuture[Future] with BeforeAndAfterAll:
  this: Suite =>

  protected given actorSystem: ActorSystem = ActorSystem("chimp-client-pekko-test")

  override given monad: MonadError[Future] = FutureMonad()(using actorSystem.dispatcher)

  override def toFuture[A](fa: Future[A]): Future[A] = fa

  override def sleep(millis: Long): Future[Unit] = after(millis.millis)(Future.unit)

  override def afterAll(): Unit =
    try
      val _ = Await.result(actorSystem.terminate(), 30.seconds)
    finally super.afterAll()
