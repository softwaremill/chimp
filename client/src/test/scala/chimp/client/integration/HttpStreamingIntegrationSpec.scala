package chimp.client.integration

import chimp.client.{BidirectionalMcpClient, McpClient}
import chimp.client.notifications.{ServerNotification, ServerNotificationListener}
import chimp.client.transport.{BidirectionalTransport, Transport}
import chimp.protocol.*
import io.circe.Json
import org.scalatest.Assertion
import sttp.model.Uri
import sttp.monad.syntax.*

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.concurrent.Future

abstract class HttpStreamingIntegrationSpec[F[_], B] extends HttpIntegrationSpec[F, B]:
  this: ToFuture[F] =>

  def usingBidirectionalTransport[A](b: B, uri: Uri)(use: BidirectionalTransport[F] => F[A]): F[A]

  override def usingTransport[A](backend: B, uri: Uri)(use: Transport[F] => F[A]): F[A] =
    usingBidirectionalTransport(backend, uri)(use)

  private val clientInfo = Implementation(name = "chimp-integration", version = "0.0.1")

  "a streaming HTTP transport" should "invoke the sampling handler when the server requests sampling" in:
    val invoked = AtomicBoolean(false)
    val sampling: CreateMessageRequest => F[CreateMessageResult] = _ =>
      invoked.set(true)
      monad.unit(
        CreateMessageResult(
          role = Role.Assistant,
          content = ToolContent.Text(text = "synthetic sample"),
          model = "test-model",
          stopReason = Some("endTurn")
        )
      )
    withBidirectionalClient(samplingHandler = Some(sampling)): client =>
      client
        .callTool("sampleLLM", Json.obj("prompt" -> Json.fromString("hi"), "maxTokens" -> Json.fromInt(8)))
        .map: _ =>
          invoked.get() shouldBe true

  it should "deliver log notifications to registered listeners after setLoggingLevel" in:
    val received = AtomicReference[Option[ServerNotification]](None)
    val listener: ServerNotificationListener[F] = notification =>
      notification match
        case _: ServerNotification.LoggingMessage => received.set(Some(notification))
        case _                                    => ()
      monad.unit(())
    withBidirectionalClient(): client =>
      for
        _ <- client.onServerNotification(listener)
        _ <- client.setLoggingLevel(LoggingLevel.Debug)
        _ <- waitUntil(received.get().isDefined, attempts = 50, intervalMs = 100)
      yield received.get().isDefined shouldBe true

  protected def withBidirectionalClient(
      rootsHandler: Option[() => F[ListRootsResult]] = None,
      samplingHandler: Option[CreateMessageRequest => F[CreateMessageResult]] = None,
      elicitationHandler: Option[ElicitRequest => F[ElicitResult]] = None
  )(test: BidirectionalMcpClient[F] => F[Assertion]): Future[Assertion] =
    toFuture(
      usingBackend: backend =>
        usingBidirectionalTransport(backend, container.mcpUri): transport =>
          McpClient[F](transport, clientInfo, rootsHandler, samplingHandler, elicitationHandler, ProtocolVersion.Latest)
            .flatMap: client =>
              test(client).flatMap(assertion => client.close().map(_ => assertion))
    )

  private def waitUntil(condition: => Boolean, attempts: Int, intervalMs: Long): F[Unit] =
    if condition || attempts <= 0 then monad.unit(())
    else monad.eval { Thread.sleep(intervalMs); () }.flatMap(_ => waitUntil(condition, attempts - 1, intervalMs))
