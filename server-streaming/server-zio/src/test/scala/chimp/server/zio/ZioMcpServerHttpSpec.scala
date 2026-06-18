package chimp.server.zio

import chimp.client.transport.ClientTransport
import chimp.client.transport.zio.ZioClientHttpTransport
import chimp.client.{BidirectionalMcpClient, McpClient}
import chimp.protocol.{Implementation, ProtocolVersion}
import chimp.server.{McpServer, McpServerStreamingTests, McpServerTests, StreamingMcpServer}
import org.scalatest.Assertion
import sttp.client4.*
import sttp.client4.httpclient.zio.HttpClientZioBackend
import sttp.tapir.server.ziohttp.ZioHttpInterpreter
import zio.http.Server
import zio.{Scope, Task, ZIO}

import scala.concurrent.Future

class ZioMcpServerHttpSpec extends McpServerTests[Task] with McpServerStreamingTests[Task] with ZioToFuture:
  private val clientInfo = Implementation("chimp-server-test", "0.0.1")

  override protected def withServer(server: McpServer[Task])(test: McpClient[Task] => Task[Assertion]): Future[Assertion] =
    withStreamingServer(server.streaming)(test)

  override protected def withStreamingServer(
      server: StreamingMcpServer[Task]
  )(test: BidirectionalMcpClient[Task] => Task[Assertion]): Future[Assertion] =
    toFuture:
      val routes = ZioHttpInterpreter().toHttp(ZioServerHttpTransport(List("mcp")).serve(server))
      ZIO.scoped:
        (for
          port <- Server.install(routes)
          result <- HttpClientZioBackend().flatMap: backend =>
            ZioClientHttpTransport
              .scoped(backend, uri"http://localhost:$port/mcp", ProtocolVersion.Latest, ClientTransport.defaultTimeout)
              .flatMap(transport => McpClient.bidirectional(transport, clientInfo))
              .flatMap(client => test(client))
              .ensuring(backend.close().ignore)
        yield result).provideSome[Scope](Server.defaultWithPort(0))
