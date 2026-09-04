package chimp.client.transport.pekko

import chimp.client.FakeNotificationClient
import chimp.client.notifications.ServerNotification
import chimp.protocol.{ProgressParams, ProgressToken}
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.Materializer
import org.apache.pekko.stream.scaladsl.Sink
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.monad.{FutureMonad, MonadError}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

class PekkoServerNotificationsSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll:

  private given system: ActorSystem = ActorSystem("chimp-client-pekko-notif-test")
  private given Materializer = Materializer.matFromSystem
  private given MonadError[Future] = FutureMonad()(using system.dispatcher)

  override def afterAll(): Unit =
    val _ = Await.result(system.terminate(), 30.seconds)

  private def progress(p: Double): ServerNotification =
    ServerNotification.Progress(ProgressParams(progressToken = ProgressToken("t"), progress = p))

  private def awaitCondition(cond: => Boolean): Unit =
    val deadline = System.currentTimeMillis + 5000
    while !cond && System.currentTimeMillis < deadline do Thread.sleep(5)

  it should "emit notifications pushed by the server and remove the listener when done" in:
    val client = FakeNotificationClient[Future]()
    val n1 = progress(0.1)
    val n2 = progress(0.2)
    val n3 = progress(0.3)

    val collecting: Future[Seq[ServerNotification]] = client.serverNotifications().take(3).runWith(Sink.seq)
    awaitCondition(client.listenerCount > 0)
    val _ = client.emit(n1)
    val _ = client.emit(n2)
    val _ = client.emit(n3)

    Await.result(collecting, 5.seconds) shouldBe Seq(n1, n2, n3)

    awaitCondition(client.listenerCount == 0)
    client.listenerCount shouldBe 0
