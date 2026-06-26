package chimp.server.ox

import chimp.server.{ServerStdioTransportTests, StreamingMcpServer, SyncToFuture}
import sttp.shared.Identity

import java.io.{InputStream, OutputStream}

class OxMcpServerStdioSpec extends ServerStdioTransportTests[Identity] with SyncToFuture:
  override protected def runStdioServer(server: StreamingMcpServer[Identity], in: InputStream, out: OutputStream): Unit =
    val thread = Thread(() => OxServerStdioTransport(in, out).serve(server))
    thread.setDaemon(true)
    thread.start()
