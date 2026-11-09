package chimp.server.zio

import chimp.server.OutboundSink
import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import zio.{Duration, Runtime, Task, Unsafe, ZIO}

import scala.concurrent.duration.*

class ZioServerKeepAliveSpec extends AnyFlatSpec with Matchers:

  private val runtime: Runtime[Any] = Runtime.default

  private def run[A](t: Task[A]): A =
    Unsafe.unsafe(implicit u => runtime.unsafe.run(t).getOrThrowFiberFailure())

  it should "emit data-less ping events while a tool call is in flight" in:
    val transport = ZioServerHttpTransport(List("mcp"), keepAlive = Some(50.millis))
    val handle: OutboundSink[Task] => Task[Option[Json]] = _ => ZIO.sleep(Duration.fromMillis(300)).as(None)

    val events = run(transport.eventStream(handle).flatMap(_.take(1).runCollect))

    events.size shouldBe 1
    events.head.eventType shouldBe Some("ping")
    events.head.data shouldBe None
