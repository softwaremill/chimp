package chimp.server

import chimp.server.transport.ServerStdioTransport
import sttp.shared.Identity

import java.io.{InputStream, OutputStream}

class ServerStdioTransportSpec extends ServerStdioTransportTests[Identity] with SyncToFuture:
  override protected def runStdioServer(server: StreamingMcpServer[Identity], in: InputStream, out: OutputStream): Unit =
    val thread = Thread(() => ServerStdioTransport(in, out).serve(server))
    thread.setDaemon(true)
    thread.start()
