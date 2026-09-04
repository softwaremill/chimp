package chimp.client.transport.ox

import chimp.client.FakeNotificationClient
import chimp.client.notifications.ServerNotification
import chimp.protocol.{ProgressParams, ProgressToken}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ox.*
import sttp.monad.{IdentityMonad, MonadError}
import sttp.shared.Identity

class OxServerNotificationsSpec extends AnyFlatSpec with Matchers:

  private given MonadError[Identity] = IdentityMonad

  private def progress(p: Double): ServerNotification =
    ServerNotification.Progress(ProgressParams(progressToken = ProgressToken("t"), progress = p))

  it should "emit notifications pushed by the server and remove the listener when done" in:
    val client = FakeNotificationClient[Identity]()
    val n1 = progress(0.1)
    val n2 = progress(0.2)
    val n3 = progress(0.3)

    val result = supervised:
      val collecting = fork(client.serverNotifications.take(3).runToList())
      while client.listenerCount == 0 do Thread.sleep(5)
      client.emit(n1)
      client.emit(n2)
      client.emit(n3)
      collecting.join()

    result shouldBe List(n1, n2, n3)
    client.listenerCount shouldBe 0
