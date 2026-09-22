package chimp.server.ox

import chimp.server.OutboundSink
import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ox.supervised
import sttp.shared.Identity

import scala.concurrent.duration.*

class OxServerKeepAliveSpec extends AnyFlatSpec with Matchers:

  it should "emit data-less ping events while a tool call is in flight" in:
    val transport = OxServerHttpTransport(List("mcp"), keepAlive = Some(50.millis))
    val handle: OutboundSink[Identity] => Option[Json] = _ =>
      Thread.sleep(300)
      None

    val events = supervised(transport.eventStream(handle).take(1).runToList())

    events should have size 1
    events.head.eventType shouldBe Some("ping")
    events.head.data shouldBe None
