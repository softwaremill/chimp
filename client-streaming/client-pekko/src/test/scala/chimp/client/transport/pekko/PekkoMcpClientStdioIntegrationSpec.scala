package chimp.client.transport.pekko

import chimp.client.integration.McpClientStdioIntegrationSpec
import chimp.client.transport.ClientBidirectionalTransport

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

class PekkoMcpClientStdioIntegrationSpec extends McpClientStdioIntegrationSpec[Future] with PekkoToFuture:

  override def usingTransport[A](command: List[String], timeout: FiniteDuration)(
      use: ClientBidirectionalTransport[Future] => Future[A]
  ): Future[A] =
    val transport = PekkoClientStdioTransport(command, timeout = timeout)
    monad.ensure2(use(transport), transport.close())
