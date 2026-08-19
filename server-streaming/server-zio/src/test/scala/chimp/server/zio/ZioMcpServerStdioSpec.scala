package chimp.server.zio

import chimp.server.{ServerStdioTransportTests, StreamingMcpServer}
import zio.Task

import java.io.{InputStream, OutputStream}
import scala.concurrent.Future

class ZioMcpServerStdioSpec extends ServerStdioTransportTests[Task] with ZioToFuture:

  override protected def runStdioServer(
      server: StreamingMcpServer[Task],
      in: InputStream,
      out: OutputStream,
      maxLineLength: Int
  ): Future[Unit] =
    toFuture(ZioServerStdioTransport(in, out, maxLineLength).serve(server))
