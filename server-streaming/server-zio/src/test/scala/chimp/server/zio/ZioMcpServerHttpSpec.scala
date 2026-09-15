package chimp.server.zio

import chimp.client.transport.ClientTransport
import chimp.client.transport.zio.ZioClientHttpTransport
import chimp.client.{BidirectionalMcpClient, McpClient}
import chimp.protocol.{Implementation, ProtocolVersion}
import chimp.server.transport.SecuredServerStreamingHttpTransport
import chimp.server.{
  McpServer,
  McpServerStreamingTests,
  McpServerTests,
  SecuredMcpServer,
  SecuredMcpServerStreamingTests,
  SecuredMcpServerTests,
  SecuredStreamingMcpServer,
  StreamingMcpServer
}
import org.scalatest.Assertion
import sttp.capabilities.WebSockets
import sttp.capabilities.zio.ZioStreams
import sttp.client4.*
import sttp.client4.httpclient.zio.HttpClientZioBackend
import sttp.model.Header
import sttp.tapir.server.ziohttp.ZioHttpInterpreter
import sttp.tapir.server.ServerEndpoint
import zio.http.Server
import zio.{Scope, Task, ZIO}

import scala.concurrent.Future

class ZioMcpServerHttpSpec
    extends McpServerTests[Task]
    with McpServerStreamingTests[Task]
    with SecuredMcpServerTests[Task]
    with SecuredMcpServerStreamingTests[Task]
    with ZioToFuture:
  private val clientInfo = Implementation("chimp-server-test", "0.0.1")

  override protected def withServer(server: McpServer[Task])(test: McpClient[Task] => Task[Assertion]): Future[Assertion] =
    withStreamingServer(server.streaming)(test)

  override protected def withStreamingServer(
      server: StreamingMcpServer[Task]
  )(test: BidirectionalMcpClient[Task] => Task[Assertion]): Future[Assertion] =
    withHttpServer(ZioServerHttpTransport(List("mcp")).serve(server), Nil): transport =>
      McpClient.bidirectional(transport, clientInfo).flatMap(test)

  override protected def withSecuredServer(
      server: SecuredMcpServer[Task, String, String, User],
      token: String
  )(test: McpClient[Task] => Task[Assertion]): Future[Assertion] =
    withHttpServer(server.endpoint(List("mcp")), List(Header.authorization("Bearer", token))): transport =>
      McpClient(transport, clientInfo).flatMap(test)

  override protected def withSecuredStreamingServer(
      server: SecuredStreamingMcpServer[Task, String, String, User],
      token: String
  )(test: BidirectionalMcpClient[Task] => Task[Assertion]): Future[Assertion] =
    val endpoint = SecuredServerStreamingHttpTransport(List("mcp"), ZioServerHttpTransport(List("mcp"))).serve(server)
    withHttpServer(endpoint, List(Header.authorization("Bearer", token))): transport =>
      McpClient.bidirectional(transport, clientInfo).flatMap(test)

  private def withHttpServer(
      endpoint: ServerEndpoint[ZioStreams & WebSockets, Task],
      headers: List[Header]
  )(test: ZioClientHttpTransport => Task[Assertion]): Future[Assertion] =
    toFuture:
      val routes = ZioHttpInterpreter().toHttp(endpoint)
      ZIO.scoped:
        (for
          port <- Server.install(routes)
          result <- HttpClientZioBackend().flatMap: backend =>
            ZioClientHttpTransport
              .scoped(
                backend,
                uri"http://localhost:$port/mcp",
                ProtocolVersion.Latest,
                ClientTransport.defaultTimeout,
                headers = headers
              )
              .flatMap(test)
              .ensuring(backend.close().ignore)
        yield result).provideSome[Scope](Server.defaultWithPort(0))
