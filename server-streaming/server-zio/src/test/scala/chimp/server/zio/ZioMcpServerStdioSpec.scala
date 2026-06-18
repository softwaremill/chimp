package chimp.server.zio

import chimp.server.{ServerStdioTransportTests, StreamingMcpServer}
import zio.{Runtime, Task, Unsafe}

import java.io.{InputStream, OutputStream}

class ZioMcpServerStdioSpec extends ServerStdioTransportTests[Task] with ZioToFuture:
  private val runtime = Runtime.default

  override protected def runStdioServer(server: StreamingMcpServer[Task], in: InputStream, out: OutputStream): Unit =
    val thread = Thread(() =>
      Unsafe.unsafe { implicit u =>
        runtime.unsafe.run(ZioServerStdioTransport(in, out).serve(server)).getOrThrowFiberFailure()
      }
    )
    thread.setDaemon(true)
    thread.start()
