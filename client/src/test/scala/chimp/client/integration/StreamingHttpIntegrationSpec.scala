package chimp.client.integration

import chimp.client.transport.{BidirectionalTransport, Transport}
import chimp.client.{BidirectionalMcpClient, McpClient}
import chimp.protocol.*
import org.scalatest.Assertion
import sttp.model.Uri
import sttp.monad.syntax.*

import scala.concurrent.Future
import scala.concurrent.duration.{DurationInt, FiniteDuration}

abstract class StreamingHttpIntegrationSpec[F[_], B]
    extends HttpIntegrationSpec[F, B]
    with BidirectionalMcpClientTests[F]
    with BidirectionalHttpMcpClientTests[F]:
  this: ToFuture[F] =>

  private val proxyContainer: MCPProxyContainer = new MCPProxyContainer(network, mcpEverythingContainer.alias, 3001)

  override def beforeAll(): Unit =
    super.beforeAll()
    proxyContainer.start()

  override def afterAll(): Unit =
    try proxyContainer.stop()
    finally super.afterAll()

  def usingBidirectionalTransport[A](b: B, uri: Uri, timeout: FiniteDuration)(use: BidirectionalTransport[F] => F[A]): F[A]

  override def usingTransport[A](backend: B, uri: Uri)(use: Transport[F] => F[A]): F[A] =
    usingBidirectionalTransport(backend, uri, 60.seconds)(use)

  private val clientInfo = Implementation(name = "chimp-integration", version = "0.0.1")

  override protected def withBidirectionalClient(
      rootsHandler: Option[() => F[ListRootsResult]] = None,
      samplingHandler: Option[CreateMessageRequest => F[CreateMessageResult]] = None,
      elicitationHandler: Option[ElicitRequest => F[ElicitResult]] = None
  )(test: BidirectionalMcpClient[F] => F[Assertion]): Future[Assertion] =
    toFuture(
      usingBackend: backend =>
        usingBidirectionalTransport(backend, mcpEverythingContainer.mcpUri, 60.seconds): transport =>
          McpClient[F](transport, clientInfo, rootsHandler, samplingHandler, elicitationHandler, ProtocolVersion.Latest)
            .flatMap: client =>
              test(client).flatMap(assertion => client.close().map(_ => assertion))
    )

  override protected def withProxiedBidirectionalClient(
      samplingHandler: Option[CreateMessageRequest => F[CreateMessageResult]] = None,
      timeout: FiniteDuration = 60.seconds
  )(test: (MCPProxyContainer, BidirectionalMcpClient[F]) => F[Assertion]): Future[Assertion] =
    proxyContainer.restoreConnections()
    proxyContainer.clearToxics()
    toFuture(
      usingBackend: backend =>
        usingBidirectionalTransport(backend, proxyContainer.mcpUri, timeout): transport =>
          McpClient[F](transport, clientInfo, None, samplingHandler, None, ProtocolVersion.Latest)
            .flatMap: client =>
              test(proxyContainer, client).flatMap(assertion => client.close().map(_ => assertion))
    )
