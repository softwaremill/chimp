package chimp.client.transport.zio

import chimp.client.integration.StdioIntegrationSpec
import chimp.client.transport.BidirectionalTransport
import zio.{Task, ZIO}

import scala.concurrent.duration.FiniteDuration

class ZioStdioIntegrationSpec extends StdioIntegrationSpec[Task] with ZioToFuture:

  override def usingTransport[A](command: List[String], timeout: FiniteDuration)(use: BidirectionalTransport[Task] => Task[A]): Task[A] =
    ZIO.scoped(ZioStreamingStdioTransport.scoped(command, timeout = timeout).flatMap(use))
