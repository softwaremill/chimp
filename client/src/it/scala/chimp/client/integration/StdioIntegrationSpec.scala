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

abstract class StdioIntegrationSpec[F[_]] extends AsyncFlatSpec with Matchers:
  this: FutureFixtures[F] =>

  protected val everythingServerCommand: List[String] =
    List("npx", "-y", "@modelcontextprotocol/server-everything")

  def usingTransport[A](command: List[String])(use: BidirectionalTransport[F] => F[A]): F[A]

  private val clientInfo = Implementation(name = "chimp-integration", version = "0.0.1")

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

  "a stdio transport" should "expose server info from initialize" in withClient(): client =>
    monad.unit(client.serverInfo.name should not be empty)

  it should "list tools" in withClient(): client =>
    client.listTools().map(_.tools should not be empty)

  it should "call the echo tool" in withClient(): client =>
    val arguments = Json.obj("message" -> Json.fromString("hello chimp"))
    client
      .callTool("echo", arguments)
      .map: result =>
        result.isError shouldBe false
        result.content should not be empty

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
        .callTool("sampleLLM", Json.obj("prompt" -> Json.fromString("hi"), "maxTokens" -> Json.fromInt(8)))
        .map: _ =>
          invoked.get() shouldBe true

  it should "deliver log notifications after setLoggingLevel" in:
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
        _ <- waitUntil(received.get().isDefined, attempts = 50, intervalMs = 100)
      yield received.get().isDefined shouldBe true

  private def waitUntil(condition: => Boolean, attempts: Int, intervalMs: Long): F[Unit] =
    if condition || attempts <= 0 then monad.unit(())
    else monad.eval { Thread.sleep(intervalMs); () }.flatMap(_ => waitUntil(condition, attempts - 1, intervalMs))
