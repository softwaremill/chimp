package chimp.client.integration

import chimp.client.transport.{ClientBidirectionalTransport, ClientTransport}
import chimp.client.{BidirectionalMcpClient, McpClient}
import chimp.protocol.*
import org.scalatest.Assertion
import sttp.model.Uri
import sttp.monad.syntax.*

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

abstract class McpClientStreamingHttpIntegrationSpec[F[_], B]
    extends McpClientHttpIntegrationSpec[F, B]
    with McpClientBidirectionalTests[F]
    with McpClientBidirectionalHttpTests[F]:
  this: ToFuture[F] =>

  private val proxyContainer: McpToxiproxyContainer = new McpToxiproxyContainer(network, mcpEverythingContainer.alias, 3001)

  override def beforeAll(): Unit =
    super.beforeAll()
    proxyContainer.start()

  override def afterAll(): Unit =
    try proxyContainer.stop()
    finally super.afterAll()

  def usingBidirectionalTransport[A](b: B, uri: Uri, timeout: FiniteDuration)(use: ClientBidirectionalTransport[F] => F[A]): F[A]

  override def usingTransport[A](backend: B, uri: Uri)(use: ClientTransport[F] => F[A]): F[A] =
    usingBidirectionalTransport(backend, uri, ClientTransport.defaultTimeout)(use)

  private val clientInfo = Implementation(name = "chimp-integration", version = "0.0.1")

  override protected def withBidirectionalClient(
      rootsHandler: Option[() => F[ListRootsResult]] = None,
      samplingHandler: Option[CreateMessageRequest => F[CreateMessageResult]] = None,
      elicitationHandler: Option[ElicitRequest => F[ElicitResult]] = None
  )(test: BidirectionalMcpClient[F] => F[Assertion]): Future[Assertion] =
    toFuture(
      usingBackend: backend =>
        usingBidirectionalTransport(backend, mcpEverythingContainer.mcpUri, ClientTransport.defaultTimeout): transport =>
          McpClient
            .bidirectional[F](transport, clientInfo, rootsHandler, samplingHandler, elicitationHandler, ProtocolVersion.Latest)
            .flatMap: client =>
              test(client).flatMap(assertion => client.close().map(_ => assertion))
    )

  override protected def withProxiedBidirectionalClient(
      samplingHandler: Option[CreateMessageRequest => F[CreateMessageResult]] = None,
      timeout: FiniteDuration = ClientTransport.defaultTimeout
  )(test: (McpToxiproxyContainer, BidirectionalMcpClient[F]) => F[Assertion]): Future[Assertion] =
    proxyContainer.restoreConnections()
    proxyContainer.clearToxics()
    toFuture(
      usingBackend: backend =>
        usingBidirectionalTransport(backend, proxyContainer.mcpUri, timeout): transport =>
          McpClient
            .bidirectional[F](transport, clientInfo, None, samplingHandler, None, ProtocolVersion.Latest)
            .flatMap: client =>
              test(proxyContainer, client).flatMap(assertion => client.close().map(_ => assertion))
    )
