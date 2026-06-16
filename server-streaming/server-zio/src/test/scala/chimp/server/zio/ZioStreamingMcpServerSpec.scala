package chimp.server.zio

import chimp.client.transport.Transport
import chimp.client.transport.zio.ZioStreamingHttpTransport
import chimp.client.{BidirectionalMcpClient, McpClient}
import chimp.protocol.{Implementation, ProtocolVersion}
import chimp.server.{McpServer, McpServerTests, StreamingMcpServer, StreamingMcpServerTests}
import org.scalatest.Assertion
import sttp.client4.*
import sttp.client4.httpclient.zio.HttpClientZioBackend
import sttp.tapir.server.ziohttp.ZioHttpInterpreter
import zio.http.Server
import zio.{Scope, Task, ZIO}

import scala.concurrent.Future

/** Runs the generic server tests — including the streaming ones — against a zio-http server hosting chimp's SSE endpoint, driven by the chimp
  * ZIO streaming client.
  */
class ZioStreamingMcpServerSpec extends McpServerTests[Task] with StreamingMcpServerTests[Task] with ZioToFuture:
  private val clientInfo = Implementation("chimp-server-test", "0.0.1")

  override protected def withServer(server: McpServer[Task])(test: McpClient[Task] => Task[Assertion]): Future[Assertion] =
    withStreamingServer(server.streaming)(test)

  override protected def withStreamingServer(
      server: StreamingMcpServer[Task]
  )(test: BidirectionalMcpClient[Task] => Task[Assertion]): Future[Assertion] =
    toFuture:
      val routes = ZioHttpInterpreter().toHttp(server.streamingEndpoint(List("mcp"), ZioMcpStreaming))
      ZIO.scoped:
        (for
          port <- Server.install(routes)
          result <- HttpClientZioBackend().flatMap: backend =>
            ZioStreamingHttpTransport
              .scoped(backend, uri"http://localhost:$port/mcp", ProtocolVersion.Latest, Transport.defaultTimeout)
              .flatMap(transport => McpClient.bidirectional(transport, clientInfo))
              .flatMap(client => test(client))
              .ensuring(backend.close().ignore)
        yield result).provideSome[Scope](Server.defaultWithPort(0))
