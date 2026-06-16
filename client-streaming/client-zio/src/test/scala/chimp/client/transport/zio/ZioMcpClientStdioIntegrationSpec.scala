package chimp.client.transport.zio

import chimp.client.integration.McpClientStdioIntegrationSpec
import chimp.client.transport.BidirectionalTransport
import zio.{Task, ZIO}

import scala.concurrent.duration.FiniteDuration

class ZioMcpClientStdioIntegrationSpec extends McpClientStdioIntegrationSpec[Task] with ZioToFuture:

  override def usingTransport[A](command: List[String], timeout: FiniteDuration)(use: BidirectionalTransport[Task] => Task[A]): Task[A] =
    ZIO.scoped(ZioStreamingStdioTransport.scoped(command, timeout = timeout).flatMap(use))
