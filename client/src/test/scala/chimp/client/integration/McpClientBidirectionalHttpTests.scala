package chimp.client.integration

import chimp.client.notifications.{ServerNotification, ServerNotificationListener}
import chimp.client.transport.ClientTransport
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

trait McpClientBidirectionalHttpTests[F[_]] extends AsyncFlatSpec with Matchers:
  this: ToFuture[F] =>

  protected def withProxiedBidirectionalClient(
      samplingHandler: Option[CreateMessageRequest => F[CreateMessageResult]] = None,
      timeout: FiniteDuration = ClientTransport.defaultTimeout
  )(test: (McpToxiproxyContainer, BidirectionalMcpClient[F]) => F[Assertion]): Future[Assertion]

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

  it should "leave the client session usable after the connection is dropped" in:
    val logCount = AtomicInteger(0)
    val listener = loggingCounter(logCount)
    withProxiedBidirectionalClient(): (proxy, client) =>
      for
        _ <- client.onServerNotification(listener)
        _ <- client.setLoggingLevel(LoggingLevel.Debug)
        _ <- client.callTool("toggle-simulated-logging", Json.obj())
        _ <- waitUntil(logCount.get() >= 2, attempts = 300, intervalMs = 100)
        _ <- monad.eval(proxy.dropConnections())
        _ <- sleep(500)
        _ <- monad.eval(proxy.clearToxics())
        _ <- client.ping()
      yield succeed

  it should "leave the client session usable across successive disconnects" in:
    val logCount = AtomicInteger(0)
    val listener = loggingCounter(logCount)
    withProxiedBidirectionalClient(): (proxy, client) =>
      def cycle: F[Unit] =
        for
          _ <- monad.eval(proxy.dropConnections())
          _ <- sleep(500)
          _ <- monad.eval(proxy.clearToxics())
          _ <- client.ping()
        yield ()
      for
        _ <- client.onServerNotification(listener)
        _ <- client.setLoggingLevel(LoggingLevel.Debug)
        _ <- client.callTool("toggle-simulated-logging", Json.obj())
        _ <- waitUntil(logCount.get() >= 2, attempts = 300, intervalMs = 100)
        _ <- cycle
        _ <- cycle
        _ <- cycle
      yield succeed

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
