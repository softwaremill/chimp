package chimp.server.pekko

import chimp.server.OutboundSink
import io.circe.Json
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.Materializer
import org.apache.pekko.stream.scaladsl.Sink
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.model.sse.ServerSentEvent

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}

class PekkoServerKeepAliveSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll:

  private given system: ActorSystem = ActorSystem("chimp-server-pekko-keepalive-test")
  private given ExecutionContext = system.dispatcher
  private given Materializer = Materializer.matFromSystem

  override def afterAll(): Unit =
    val _ = Await.result(system.terminate(), 30.seconds)

  it should "emit data-less ping events while a tool call is in flight" in:
    val transport = PekkoServerHttpTransport(List("mcp"), keepAlive = Some(50.millis))
    val handle: OutboundSink[Future] => Future[Option[Json]] = _ => Future { Thread.sleep(300); None }

    val events: Seq[ServerSentEvent] =
      Await.result(transport.eventStream(handle).flatMap(_.take(1).runWith(Sink.seq)), 5.seconds)

    events should have size 1
    events.head.eventType shouldBe Some("ping")
    events.head.data shouldBe None
