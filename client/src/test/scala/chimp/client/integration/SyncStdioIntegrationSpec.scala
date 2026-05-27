package chimp.client.integration

import chimp.client.transport.{BidirectionalTransport, StdioTransport}
import sttp.shared.Identity

import scala.concurrent.duration.FiniteDuration

class SyncStdioIntegrationSpec extends StdioIntegrationSpec[Identity] with SyncToFuture:

  override def usingTransport[A](command: List[String], timeout: FiniteDuration)(
      use: BidirectionalTransport[Identity] => Identity[A]
  ): Identity[A] =
    val transport = StdioTransport(command, timeout = timeout)
    try use(transport)
    finally transport.close()
