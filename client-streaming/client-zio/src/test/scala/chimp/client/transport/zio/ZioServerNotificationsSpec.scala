package chimp.client.transport.zio

import chimp.client.FakeNotificationClient
import chimp.client.notifications.ServerNotification
import chimp.protocol.{ProgressParams, ProgressToken}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.client4.impl.zio.RIOMonadAsyncError
import sttp.monad.MonadError
import zio.{Chunk, Runtime, Task, Unsafe, ZIO}

class ZioServerNotificationsSpec extends AnyFlatSpec with Matchers:

  private given MonadError[Task] = new RIOMonadAsyncError[Any]
  private val runtime: Runtime[Any] = Runtime.default

  private def run[A](task: Task[A]): A =
    Unsafe.unsafe(implicit u => runtime.unsafe.run(task).getOrThrowFiberFailure())

  private def progress(p: Double): ServerNotification =
    ServerNotification.Progress(ProgressParams(progressToken = ProgressToken("t"), progress = p))

  it should "emit notifications pushed by the server and remove the listener when done" in:
    val client = FakeNotificationClient[Task]()
    val n1 = progress(0.1)
    val n2 = progress(0.2)
    val n3 = progress(0.3)

    val program =
      for
        collecting <- client.serverNotifications.take(3).runCollect.fork
        _ <- ZIO.succeed(client.listenerCount).repeatUntil(_ > 0)
        _ <- client.emit(n1) *> client.emit(n2) *> client.emit(n3)
        result <- collecting.join
      yield result

    run(program) shouldBe Chunk(n1, n2, n3)
    client.listenerCount shouldBe 0
