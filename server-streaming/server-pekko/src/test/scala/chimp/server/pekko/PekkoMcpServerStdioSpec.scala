package chimp.server.pekko

import chimp.server.{ServerStdioTransportTests, StreamingMcpServer}

import java.io.{InputStream, OutputStream}
import scala.concurrent.Future

class PekkoMcpServerStdioSpec extends ServerStdioTransportTests[Future] with PekkoToFuture:

  override protected def runStdioServer(server: StreamingMcpServer[Future], in: InputStream, out: OutputStream): Unit =
    val _ = PekkoServerStdioTransport(in, out).serve(server)
