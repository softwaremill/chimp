package chimp.server.ox

import chimp.server.{ServerStdioTransportTests, StreamingMcpServer, SyncToFuture}
import sttp.shared.Identity

import java.io.{InputStream, OutputStream}
import scala.concurrent.{Future, Promise}
import scala.util.Try

class OxMcpServerStdioSpec extends ServerStdioTransportTests[Identity] with SyncToFuture:
  override protected def runStdioServer(
      server: StreamingMcpServer[Identity],
      in: InputStream,
      out: OutputStream,
      maxLineLength: Int
  ): Future[Unit] =
    val served = Promise[Unit]()
    val thread = Thread { () =>
      val _ = served.complete(Try(OxServerStdioTransport(in, out, maxLineLength).serve(server)))
    }
    thread.setDaemon(true)
    thread.start()
    served.future
