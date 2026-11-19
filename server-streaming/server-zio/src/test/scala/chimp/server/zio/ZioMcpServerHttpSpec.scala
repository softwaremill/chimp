package chimp.server.zio

import chimp.client.transport.ClientTransport
import chimp.client.transport.zio.ZioClientHttpTransport
import chimp.client.{BidirectionalMcpClient, McpClient}
import chimp.protocol.{Implementation, LoggingLevel, ProtocolVersion}
import chimp.server.transport.SecuredServerStreamingHttpTransport
import chimp.server.{
  McpServer,
  McpServerHeaderHttpTests,
  McpServerInputRequiredHttpTests,
  McpServerStreamingTests,
  McpServerSubscriptionHttpTests,
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

import java.net.URI
import scala.concurrent.Future

class ZioMcpServerHttpSpec
    extends McpServerTests[Task]
    with McpServerStreamingTests[Task]
    with McpServerSubscriptionHttpTests[Task]
    with McpServerInputRequiredHttpTests[Task]
    with McpServerHeaderHttpTests[Task]
    with SecuredMcpServerTests[Task]
    with SecuredMcpServerStreamingTests[Task]
    with ZioToFuture:
  private val clientInfo = Implementation("chimp-server-test", "0.0.1")
  override protected val clientProtocolVersion: ProtocolVersion = ProtocolVersion.Latest

  override protected def withServer(server: McpServer[Task])(test: McpClient[Task] => Task[Assertion]): Future[Assertion] =
    withStreamingServer(server.streaming)(test)

  override protected def withStreamingServer(
      server: StreamingMcpServer[Task]
  )(test: BidirectionalMcpClient[Task] => Task[Assertion]): Future[Assertion] =
    withHttpServer(ZioServerHttpTransport(List("mcp")).serve(server), Nil): transport =>
      McpClient.bidirectional(transport, clientInfo, protocolVersion = clientProtocolVersion).flatMap(test)

  override protected def withLoggingStreamingServer(server: StreamingMcpServer[Task], logLevel: LoggingLevel)(
      test: BidirectionalMcpClient[Task] => Task[Assertion]
  ): Future[Assertion] =
    withHttpServer(ZioServerHttpTransport(List("mcp")).serve(server), Nil): transport =>
      McpClient.bidirectional(transport, clientInfo, protocolVersion = clientProtocolVersion, logLevel = Some(logLevel)).flatMap(test)

  override protected def withStreamingHttpEndpoint(server: StreamingMcpServer[Task])(test: URI => Assertion): Future[Assertion] =
    withEndpointUri(ZioServerHttpTransport(List("mcp")).serve(server))(test)

  override protected def withHttpEndpoint(server: McpServer[Task])(test: URI => Assertion): Future[Assertion] =
    withEndpointUri(server.endpoint(List("mcp")))(test)

  private def withEndpointUri(endpoint: ServerEndpoint[ZioStreams & WebSockets, Task])(test: URI => Assertion): Future[Assertion] =
    toFuture:
      val routes = ZioHttpInterpreter().toHttp(endpoint)
      (for
        port <- Server.install(routes)
        result <- ZIO.attemptBlocking(test(URI.create(s"http://localhost:$port/mcp")))
      yield result).provide(Server.defaultWithPort(0))

  override protected def withSecuredServer(
      server: SecuredMcpServer[Task, String, String, User],
      token: String
  )(test: McpClient[Task] => Task[Assertion]): Future[Assertion] =
    withHttpServer(server.endpoint(List("mcp")), List(Header.authorization("Bearer", token))): transport =>
      McpClient(transport, clientInfo, clientProtocolVersion).flatMap(test)

  override protected def withSecuredStreamingServer(
      server: SecuredStreamingMcpServer[Task, String, String, User],
      token: String
  )(test: BidirectionalMcpClient[Task] => Task[Assertion]): Future[Assertion] =
    val endpoint = SecuredServerStreamingHttpTransport(List("mcp"), ZioServerHttpTransport(List("mcp"))).serve(server)
    withHttpServer(endpoint, List(Header.authorization("Bearer", token))): transport =>
      McpClient.bidirectional(transport, clientInfo, protocolVersion = clientProtocolVersion).flatMap(test)

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
                ClientTransport.defaultTimeout,
                headers = headers
              )
              .flatMap(test)
              .ensuring(backend.close().ignore)
        yield result).provideSome[Scope](Server.defaultWithPort(0))
