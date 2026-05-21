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
        .callTool("trigger-sampling-request", Json.obj("prompt" -> Json.fromString("hi"), "maxTokens" -> Json.fromInt(8)))
        .map: _ =>
          invoked.get() shouldBe true

  it should "invoke the elicitation handler when the server requests elicitation" in:
    val invoked = AtomicBoolean(false)
    val elicitation: ElicitRequest => F[ElicitResult] = _ =>
      invoked.set(true)
      monad.unit(ElicitResult(action = ElicitAction.Cancel))
    withBidirectionalClient(elicitationHandler = Some(elicitation)): client =>
      client
        .callTool("trigger-elicitation-request", Json.obj())
        .map: result =>
          invoked.get() shouldBe true
          result.isError shouldBe false

  it should "invoke the roots handler when get-roots-list is called" in:
    val invoked = AtomicBoolean(false)
    val roots: () => F[ListRootsResult] = () =>
      invoked.set(true)
      monad.unit(ListRootsResult(roots = List(Root(uri = "file:///chimp-test", name = Some("test")))))
    withBidirectionalClient(rootsHandler = Some(roots)): client =>
      client
        .callTool("get-roots-list", Json.obj())
        .map: result =>
          invoked.get() shouldBe true
          result.isError shouldBe false

  it should "deliver resource update notifications after toggling subscriber updates" in:
    val received = AtomicReference[Option[ServerNotification]](None)
    val listener: ServerNotificationListener[F] = notification =>
      notification match
        case _: ServerNotification.ResourceUpdated => received.set(Some(notification))
        case _                                     => ()
      monad.unit(())
    withBidirectionalClient(): client =>
      for
        _         <- client.onServerNotification(listener)
        resources <- client.listResources()
        first = resources.resources.head
        _ <- client.subscribeResource(first.uri)
        _ <- client.callTool("toggle-subscriber-updates", Json.obj())
        _ <- waitUntil(received.get().isDefined, attempts = 120, intervalMs = 100)
      yield received.get().isDefined shouldBe true

  it should "deliver log notifications to registered listeners after enabling simulated logging" in:
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
        _ <- client.callTool("toggle-simulated-logging", Json.obj())
        _ <- waitUntil(received.get().isDefined, attempts = 120, intervalMs = 100)
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
