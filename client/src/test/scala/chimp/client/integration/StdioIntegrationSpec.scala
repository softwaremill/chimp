package chimp.client.integration

import chimp.client.notifications.{ServerNotification, ServerNotificationListener}
import chimp.client.transport.BidirectionalTransport
import chimp.client.{BidirectionalMcpClient, McpClient}
import chimp.protocol.*
import io.circe.Json
import org.scalatest.Assertion
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.monad.syntax.*

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.concurrent.Future

abstract class StdioIntegrationSpec[F[_]] extends AsyncFlatSpec with Matchers with IntegrationSpec:
  this: ToFuture[F] =>

  protected val everythingServerCommand: List[String] =
    List("npx", "-y", "@modelcontextprotocol/server-everything")

  def usingTransport[A](command: List[String])(use: BidirectionalTransport[F] => F[A]): F[A]

  private val clientInfo = Implementation(name = "chimp-integration", version = "0.0.1")

  "a stdio transport" should "expose server info from initialize" in withClient(): client =>
    monad.unit(client.serverInfo.name should not be empty)

  it should "negotiate tools capability" in withClient(): client =>
    monad.unit(client.serverCapabilities.tools.isDefined shouldBe true)

  it should "list tools" in withClient(): client =>
    client.listTools().map(_.tools should not be empty)

  it should "call the echo tool" in withClient(): client =>
    val arguments = Json.obj("message" -> Json.fromString("hello chimp"))
    client
      .callTool("echo", arguments)
      .map: result =>
        result.isError shouldBe false
        result.content should not be empty

  it should "call the get-tiny-image tool and receive image content" in withClient(): client =>
    client
      .callTool("get-tiny-image", Json.obj())
      .map: result =>
        result.isError shouldBe false
        result.content.collect { case image: ToolContent.Image => image } should not be empty

  it should "call the get-structured-content tool and receive structured content" in withClient(): client =>
    val arguments = Json.obj("location" -> Json.fromString("New York"))
    client
      .callTool("get-structured-content", arguments)
      .map: result =>
        result.isError shouldBe false
        result.structuredContent.isDefined shouldBe true

  it should "call the get-resource-links tool and receive resource links" in withClient(): client =>
    val arguments = Json.obj("count" -> Json.fromInt(3))
    client
      .callTool("get-resource-links", arguments)
      .map: result =>
        result.isError shouldBe false
        result.content.collect { case link: ToolContent.ResourceLink => link } should not be empty

  it should "call the get-resource-reference tool" in withClient(): client =>
    val arguments = Json.obj("resourceType" -> Json.fromString("Text"), "resourceId" -> Json.fromInt(1))
    client
      .callTool("get-resource-reference", arguments)
      .map: result =>
        result.isError shouldBe false
        result.content should not be empty

  it should "list prompts" in withClient(): client =>
    client.listPrompts().map(_.prompts should not be empty)

  it should "list resources" in withClient(): client =>
    client.listResources().map(_.resources should not be empty)

  it should "read the first listed resource" in withClient(): client =>
    client.listResources().flatMap: resources =>
      val first = resources.resources.head
      client
        .readResource(first.uri)
        .map: result =>
          result.contents should not be empty

  it should "list resource templates" in withClient(): client =>
    client.listResourceTemplates().map(_.resourceTemplates should not be empty)

  it should "ping" in withClient(): client =>
    client.ping().map(_ => succeed)

  it should "invoke the sampling handler when the server requests sampling" in:
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
    withClient(samplingHandler = Some(sampling)): client =>
      client
        .callTool("trigger-sampling-request", Json.obj("prompt" -> Json.fromString("hi"), "maxTokens" -> Json.fromInt(8)))
        .map: _ =>
          invoked.get() shouldBe true

  it should "invoke the elicitation handler when the server requests elicitation" in:
    val invoked = AtomicBoolean(false)
    val elicitation: ElicitRequest => F[ElicitResult] = _ =>
      invoked.set(true)
      monad.unit(ElicitResult(action = ElicitAction.Cancel))
    withClient(elicitationHandler = Some(elicitation)): client =>
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
    withClient(rootsHandler = Some(roots)): client =>
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
    withClient(): client =>
      for
        _         <- client.onServerNotification(listener)
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
    withClient(): client =>
      for
        _ <- client.onServerNotification(listener)
        _ <- client.setLoggingLevel(LoggingLevel.Debug)
        _ <- client.callTool("toggle-simulated-logging", Json.obj())
        _ <- waitUntil(received.get().isDefined, attempts = 120, intervalMs = 100)
      yield received.get().isDefined shouldBe true

  protected def withClient(
      rootsHandler: Option[() => F[ListRootsResult]] = None,
      samplingHandler: Option[CreateMessageRequest => F[CreateMessageResult]] = None,
      elicitationHandler: Option[ElicitRequest => F[ElicitResult]] = None
  )(test: BidirectionalMcpClient[F] => F[Assertion]): Future[Assertion] =
    toFuture(
      usingTransport(everythingServerCommand): transport =>
        McpClient[F](transport, clientInfo, rootsHandler, samplingHandler, elicitationHandler, ProtocolVersion.Latest).flatMap: client =>
          test(client).flatMap(assertion => client.close().map(_ => assertion))
    )

  private def waitUntil(condition: => Boolean, attempts: Int, intervalMs: Long): F[Unit] =
    if condition || attempts <= 0 then monad.unit(())
    else monad.eval { Thread.sleep(intervalMs); () }.flatMap(_ => waitUntil(condition, attempts - 1, intervalMs))
