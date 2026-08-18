package chimp.client.transport.pekko.internal

import chimp.client.McpTransportException
import chimp.client.transport.pekko.PekkoToFuture
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.{DurationInt, FiniteDuration}

class StateActorSpec extends AsyncFlatSpec with Matchers with PekkoToFuture:

  private class Counter:
    var value: Int = 0

    def increment(): Int =
      value += 1
      value

  private def counterActor(idleTimeout: FiniteDuration = 30.seconds): StateActor[Counter] =
    StateActor(new Counter, "state-actor-spec", idleTimeout)

  "a state actor" should "read the state which a modification changed" in:
    val state = counterActor()
    state.tell(_.value = 7)
    state.ask(_.value).map(_ shouldBe 7)

  it should "read and modify the state in a single step" in:
    val state = counterActor()
    for
      first <- state.ask(_.increment())
      second <- state.ask(_.increment())
    yield (first, second) shouldBe (1, 2)

  it should "apply all modifications sent from many threads" in:
    val state = counterActor()
    val threads = (1 to 4).map(_ =>
      Thread(() =>
        (1 to 250).foreach { _ =>
          state.tell { counter =>
            val _ = counter.increment()
          }
        }
      )
    )
    threads.foreach(_.start())
    threads.foreach(_.join())
    state.ask(_.value).map(_ shouldBe 1000)

  it should "keep the state when a modification fails" in:
    val state = counterActor()
    state.tell(_.value = 5)
    state.tell(_ => throw RuntimeException("state modification failed"))
    state.ask(_.value).map(_ shouldBe 5)

  it should "use a unique actor name for each instance" in:
    val first = counterActor()
    val second = counterActor()
    first.tell(_.value = 1)
    second.tell(_.value = 2)
    for
      firstValue <- first.ask(_.value)
      secondValue <- second.ask(_.value)
    yield (firstValue, secondValue) shouldBe (1, 2)

  it should "answer reads after it is asked to stop, and fail them once it stopped" in:
    val state = counterActor(idleTimeout = 200.millis)
    state.tell(_.value = 3)
    state.stopWhenIdle()
    for
      answered <- state.ask(_.value)
      _ <- sleep(1000)
      failure <- state.ask(_.value).failed
    yield
      answered shouldBe 3
      failure shouldBe a[McpTransportException]
