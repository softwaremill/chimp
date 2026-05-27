package chimp.client.integration

import chimp.client.transport.{BidirectionalTransport, Transport}
import chimp.client.{BidirectionalMcpClient, McpClient, McpTimeoutException}
import chimp.protocol.*
import org.scalatest.Assertion
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.monad.syntax.*

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.Future
import scala.concurrent.duration.{DurationInt, FiniteDuration}

abstract class StdioIntegrationSpec[F[_]]
    extends AsyncFlatSpec
    with Matchers
    with IntegrationSpec
    with McpClientTests[F]
    with BidirectionalMcpClientTests[F]:
  this: ToFuture[F] =>

  protected val everythingServerCommand: List[String] =
    List("npx", "-y", "@modelcontextprotocol/server-everything@2026.1.26")

  def usingTransport[A](command: List[String], timeout: FiniteDuration)(use: BidirectionalTransport[F] => F[A]): F[A]

  private val clientInfo = Implementation(name = "chimp-integration", version = "0.0.1")

  override protected def withClient(test: McpClient[F] => F[Assertion]): Future[Assertion] =
    withBidirectionalClient()(test)

  override protected def withBidirectionalClient(
      rootsHandler: Option[() => F[ListRootsResult]] = None,
      samplingHandler: Option[CreateMessageRequest => F[CreateMessageResult]] = None,
      elicitationHandler: Option[ElicitRequest => F[ElicitResult]] = None
  )(test: BidirectionalMcpClient[F] => F[Assertion]): Future[Assertion] =
    toFuture(
      usingTransport(everythingServerCommand, Transport.defaultTimeout): transport =>
        McpClient
          .bidirectional[F](transport, clientInfo, rootsHandler, samplingHandler, elicitationHandler, ProtocolVersion.Latest)
          .flatMap: client =>
            test(client).flatMap(assertion => client.close().map(_ => assertion))
    )

  "a request" should "fail with McpTimeoutException when the subprocess does not respond within the configured timeout" in:
    val failure = AtomicReference[Option[Throwable]](None)
    toFuture(
      usingTransport(List("sleep", "30"), timeout = 200.millis): transport =>
        val request: JSONRPCMessage = JSONRPCMessage.Request(method = "ping", params = None, id = RequestId(1))
        transport
          .send(request)
          .map(_ => ())
          .handleError:
            case t =>
              failure.set(Some(t))
              monad.unit(())
    ).map: _ =>
      failure.get().exists(_.isInstanceOf[McpTimeoutException]) shouldBe true
