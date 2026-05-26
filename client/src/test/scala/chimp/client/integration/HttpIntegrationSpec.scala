package chimp.client.integration

import chimp.client.McpClient
import chimp.client.transport.Transport
import chimp.protocol.{Implementation, ProtocolVersion}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Assertion, BeforeAndAfterAll}
import org.testcontainers.containers.Network
import sttp.model.Uri
import sttp.monad.syntax.*

import scala.concurrent.Future

abstract class HttpIntegrationSpec[F[_], B]
    extends AsyncFlatSpec
    with Matchers
    with BeforeAndAfterAll
    with IntegrationSpec
    with McpClientTests[F]:
  this: ToFuture[F] =>

  protected val network: Network = Network.newNetwork()
  protected val mcpEverythingContainer: MCPEverythingContainer = new MCPEverythingContainer(network = Some(network))

  override def beforeAll(): Unit =
    super.beforeAll()
    mcpEverythingContainer.start()

  override def afterAll(): Unit =
    try mcpEverythingContainer.stop()
    finally
      try network.close()
      finally super.afterAll()

  def usingBackend[A](use: B => F[A]): F[A]
  def usingTransport[A](backend: B, uri: Uri)(use: Transport[F] => F[A]): F[A]

  private val clientInfo = Implementation(name = "chimp-integration", version = "0.0.1")

  override protected def withClient(test: McpClient[F] => F[Assertion]): Future[Assertion] =
    toFuture(
      usingBackend: backend =>
        usingTransport(backend, mcpEverythingContainer.mcpUri): transport =>
          McpClient(transport, clientInfo, ProtocolVersion.Latest).flatMap: client =>
            test(client).flatMap(assertion => client.close().map(_ => assertion))
    )
