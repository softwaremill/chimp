package chimp.client.integration

import chimp.client.notifications.{ServerNotification, ServerNotificationListener}
import chimp.client.{BidirectionalMcpClient, McpTimeoutException}
import chimp.protocol.*
import io.circe.Json
import org.scalatest.Assertion
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.monad.syntax.*

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}
import scala.concurrent.Future
import scala.concurrent.duration.{DurationInt, FiniteDuration}

trait BidirectionalHttpMcpClientTests[F[_]] extends AsyncFlatSpec with Matchers:
  this: ToFuture[F] =>

  protected def withProxiedBidirectionalClient(
      samplingHandler: Option[CreateMessageRequest => F[CreateMessageResult]] = None,
      timeout: FiniteDuration = 60.seconds
  )(test: (MCPProxyContainer, BidirectionalMcpClient[F]) => F[Assertion]): Future[Assertion]

  "GET SSE stream" should "resume delivering notifications after the underlying connection is cut" in:
    val logCount = AtomicInteger(0)
    val listener = loggingCounter(logCount)
    withProxiedBidirectionalClient(): (proxy, client) =>
      for
        _ <- client.onServerNotification(listener)
        _ <- client.setLoggingLevel(LoggingLevel.Debug)
        _ <- client.callTool("toggle-simulated-logging", Json.obj())
        _ <- waitUntil(logCount.get() >= 2, attempts = 300, intervalMs = 100)
        beforeCut = logCount.get()
        _ <- monad.eval(proxy.cutConnections())
        _ <- sleep(500)
        _ <- monad.eval(proxy.restoreConnections())
        _ <- waitUntil(logCount.get() > beforeCut, attempts = 400, intervalMs = 100)
      yield logCount.get() should be > beforeCut

  it should "survive multiple successive disconnects" in:
    val logCount = AtomicInteger(0)
    val listener = loggingCounter(logCount)
    withProxiedBidirectionalClient(): (proxy, client) =>
      def cycle(prev: Int): F[Int] =
        for
          _ <- monad.eval(proxy.cutConnections())
          _ <- sleep(500)
          _ <- monad.eval(proxy.restoreConnections())
          _ <- waitUntil(logCount.get() > prev, attempts = 400, intervalMs = 100)
        yield logCount.get()
      for
        _ <- client.onServerNotification(listener)
        _ <- client.setLoggingLevel(LoggingLevel.Debug)
        _ <- client.callTool("toggle-simulated-logging", Json.obj())
        _ <- waitUntil(logCount.get() >= 2, attempts = 300, intervalMs = 100)
        first = logCount.get()
        second <- cycle(first)
        third <- cycle(second)
      yield
        second should be > first
        third should be > second

  "a server initiated request" should "fail with a timeout when the response is delayed beyond the configured timeout" in:
    val samplingInvoked = AtomicBoolean(false)
    val sampling: CreateMessageRequest => F[CreateMessageResult] = _ =>
      samplingInvoked.set(true)
      monad.unit(
        CreateMessageResult(
          role = Role.Assistant,
          content = ToolContent.Text(text = "ok"),
          model = "test-model",
          stopReason = Some("endTurn")
        )
      )
    val failure = AtomicReference[Option[Throwable]](None)
    withProxiedBidirectionalClient(samplingHandler = Some(sampling), timeout = 100.millis): (proxy, client) =>
      proxy.addLatencyMs(500)
      val attempt = client
        .callTool("trigger-sampling-request", Json.obj("prompt" -> Json.fromString("hi"), "maxTokens" -> Json.fromInt(8)))
        .map(_ => ())
        .handleError:
          case t =>
            failure.set(Some(t))
            monad.unit(())
      for _ <- attempt
      yield
        samplingInvoked.get() shouldBe true
        failure.get().exists(_.isInstanceOf[McpTimeoutException]) shouldBe true

  private def loggingCounter(counter: AtomicInteger): ServerNotificationListener[F] = notification =>
    notification match
      case _: ServerNotification.LoggingMessage => val _ = counter.incrementAndGet()
      case _                                    => ()
    monad.unit(())
