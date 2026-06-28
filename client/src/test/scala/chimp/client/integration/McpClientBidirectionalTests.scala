package chimp.client.integration

import chimp.client.BidirectionalMcpClient
import chimp.client.notifications.{ServerNotification, ServerNotificationListener}
import chimp.protocol.*
import io.circe.Json
import org.scalatest.Assertion
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.monad.syntax.*

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.concurrent.Future

trait McpClientBidirectionalTests[F[_]] extends AsyncFlatSpec with Matchers:
  this: ToFuture[F] =>

  protected def withBidirectionalClient(
      rootsHandler: Option[() => F[ListRootsResult]] = None,
      samplingHandler: Option[CreateMessageRequest => F[CreateMessageResult]] = None,
      elicitationHandler: Option[ElicitRequest => F[ElicitResult]] = None
  )(test: BidirectionalMcpClient[F] => F[Assertion]): Future[Assertion]

  "a bidirectional MCP client" should "invoke the sampling handler when the server requests sampling" in:
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
        _ <- client.onServerNotification(listener)
        resources <- client.listResources()
        first = resources.resources.head
        _ <- client.subscribeResource(first.uri)
        _ <- client.callTool("toggle-subscriber-updates", Json.obj())
        _ <- waitUntil(received.get().isDefined, attempts = 120, intervalMs = 100)
      yield received.get().isDefined shouldBe true

  it should "deliver log notifications after enabling simulated logging" in:
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
