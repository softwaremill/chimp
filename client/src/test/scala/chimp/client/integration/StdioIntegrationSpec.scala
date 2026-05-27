package chimp.client.integration

import chimp.client.transport.BidirectionalTransport
import chimp.client.{BidirectionalMcpClient, McpClient}
import chimp.protocol.*
import org.scalatest.Assertion
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.monad.syntax.*

import scala.concurrent.Future

abstract class StdioIntegrationSpec[F[_]]
    extends AsyncFlatSpec
    with Matchers
    with IntegrationSpec
    with McpClientTests[F]
    with BidirectionalMcpClientTests[F]:
  this: ToFuture[F] =>

  protected val everythingServerCommand: List[String] =
    List("npx", "-y", "@modelcontextprotocol/server-everything@2026.1.26")

  def usingTransport[A](command: List[String])(use: BidirectionalTransport[F] => F[A]): F[A]

  private val clientInfo = Implementation(name = "chimp-integration", version = "0.0.1")

  override protected def withClient(test: McpClient[F] => F[Assertion]): Future[Assertion] =
    withBidirectionalClient()(test)

  override protected def withBidirectionalClient(
      rootsHandler: Option[() => F[ListRootsResult]] = None,
      samplingHandler: Option[CreateMessageRequest => F[CreateMessageResult]] = None,
      elicitationHandler: Option[ElicitRequest => F[ElicitResult]] = None
  )(test: BidirectionalMcpClient[F] => F[Assertion]): Future[Assertion] =
    toFuture(
      usingTransport(everythingServerCommand): transport =>
        McpClient
          .bidirectional[F](transport, clientInfo, rootsHandler, samplingHandler, elicitationHandler, ProtocolVersion.Latest)
          .flatMap: client =>
            test(client).flatMap(assertion => client.close().map(_ => assertion))
    )
