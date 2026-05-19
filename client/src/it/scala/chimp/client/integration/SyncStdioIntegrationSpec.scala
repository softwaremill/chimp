package chimp.client.integration

import chimp.client.transport.{BidirectionalTransport, StdioTransport}
import sttp.shared.Identity

class SyncStdioIntegrationSpec extends StdioIntegrationSpec[Identity] with SyncFutureFixtures:

  override def usingTransport[A](command: List[String])(use: BidirectionalTransport[Identity] => Identity[A]): Identity[A] =
    val t = StdioTransport(command)
    try use(t)
    finally t.close()
