package chimp.client.transport.zio

import chimp.client.integration.StdioIntegrationSpec
import chimp.client.transport.BidirectionalTransport
import zio.{Task, ZIO}

class ZioStdioIntegrationSpec extends StdioIntegrationSpec[Task] with ZioToFuture:

  override def usingTransport[A](command: List[String])(use: BidirectionalTransport[Task] => Task[A]): Task[A] =
    ZIO.scoped(ZioStreamingStdioTransport.scoped(command).flatMap(use))
