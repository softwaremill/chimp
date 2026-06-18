package chimp.client.transport.zio

import chimp.client.integration.McpClientStdioIntegrationSpec
import chimp.client.transport.ClientBidirectionalTransport
import zio.{Task, ZIO}

import scala.concurrent.duration.FiniteDuration

class ZioMcpClientStdioIntegrationSpec extends McpClientStdioIntegrationSpec[Task] with ZioToFuture:

  override def usingTransport[A](command: List[String], timeout: FiniteDuration)(
      use: ClientBidirectionalTransport[Task] => Task[A]
  ): Task[A] =
    ZIO.scoped(ZioClientStdioTransport.scoped(command, timeout = timeout).flatMap(use))
