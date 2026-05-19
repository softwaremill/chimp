package chimp.client.integration

import chimp.client.transport.{BidirectionalTransport, StdioTransport}
import sttp.shared.Identity

class SyncStdioIntegrationSpec extends StdioIntegrationSpec[Identity] with SyncToFuture:

  override def usingTransport[A](command: List[String])(use: BidirectionalTransport[Identity] => Identity[A]): Identity[A] =
    val transport = StdioTransport(command)
    try use(transport)
    finally transport.close()
