package chimp.client.transport.ox

import chimp.client.integration.{McpClientStdioIntegrationSpec, SyncToFuture}
import chimp.client.transport.ClientBidirectionalTransport
import ox.supervised
import sttp.shared.Identity

import scala.concurrent.duration.FiniteDuration

class OxMcpClientStdioIntegrationSpec extends McpClientStdioIntegrationSpec[Identity] with SyncToFuture:

  override def usingTransport[A](command: List[String], timeout: FiniteDuration)(
      use: ClientBidirectionalTransport[Identity] => A
  ): A =
    supervised:
      val transport = OxClientStdioTransport(command, timeout = timeout)
      try use(transport)
      finally transport.close()
