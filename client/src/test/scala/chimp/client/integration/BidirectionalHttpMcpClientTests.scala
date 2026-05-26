package chimp.client.integration

import chimp.client.BidirectionalMcpClient
import chimp.client.notifications.{ServerNotification, ServerNotificationListener}
import chimp.protocol.*
import io.circe.Json
import org.scalatest.Assertion
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.monad.syntax.*

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.Future

trait BidirectionalHttpMcpClientTests[F[_]] extends AsyncFlatSpec with Matchers:
  this: ToFuture[F] =>

  protected def withProxiedBidirectionalClient(test: (MCPProxyContainer, BidirectionalMcpClient[F]) => F[Assertion]): Future[Assertion]

  "GET SSE stream" should "resume delivering notifications after the underlying connection is cut" in:
    val logCount = AtomicInteger(0)
    val listener: ServerNotificationListener[F] = notification =>
      notification match
        case _: ServerNotification.LoggingMessage => val _ = logCount.incrementAndGet()
        case _                                    => ()
      monad.unit(())

    withProxiedBidirectionalClient: (proxy, client) =>
      for
        _ <- client.onServerNotification(listener)
        _ <- client.setLoggingLevel(LoggingLevel.Debug)
        _ <- client.callTool("toggle-simulated-logging", Json.obj())
        _ <- waitUntil(logCount.get() >= 2, attempts = 300, intervalMs = 100)
        beforeCut = logCount.get()
        _ <- monad.eval { proxy.cutConnections(); () }
        _ <- sleep(2000)
        _ <- monad.eval { proxy.restoreConnections(); () }
        _ <- waitUntil(logCount.get() > beforeCut, attempts = 400, intervalMs = 100)
      yield logCount.get() should be > beforeCut
