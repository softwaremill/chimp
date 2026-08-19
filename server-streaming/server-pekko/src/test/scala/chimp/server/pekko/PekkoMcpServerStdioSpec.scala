package chimp.server.pekko

import chimp.server.{ServerStdioTransportTests, StreamingMcpServer}

import java.io.{InputStream, OutputStream}
import scala.concurrent.Future

class PekkoMcpServerStdioSpec extends ServerStdioTransportTests[Future] with PekkoToFuture:

  override protected def runStdioServer(
      server: StreamingMcpServer[Future],
      in: InputStream,
      out: OutputStream,
      maxLineLength: Int
  ): Future[Unit] =
    PekkoServerStdioTransport(in, out, maxLineLength).serve(server)
