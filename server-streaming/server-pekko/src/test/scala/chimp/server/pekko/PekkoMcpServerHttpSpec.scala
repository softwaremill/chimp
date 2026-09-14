package chimp.server.pekko

import chimp.client.transport.pekko.PekkoClientHttpTransport
import chimp.client.{BidirectionalMcpClient, McpClient}
import chimp.protocol.Implementation
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
import org.apache.pekko.http.scaladsl.Http
import org.scalatest.Assertion
import sttp.capabilities.WebSockets
import sttp.capabilities.pekko.PekkoStreams
import sttp.client4.pekkohttp.PekkoHttpBackend
import sttp.model.Header
import sttp.model.Uri.UriContext
import sttp.tapir.server.pekkohttp.PekkoHttpServerInterpreter
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

class PekkoMcpServerHttpSpec
    extends McpServerTests[Future]
    with McpServerStreamingTests[Future]
    with SecuredMcpServerTests[Future]
    with SecuredMcpServerStreamingTests[Future]
    with PekkoToFuture:
  private val clientInfo = Implementation("chimp-server-test", "0.0.1")

  override protected def withServer(server: McpServer[Future])(test: McpClient[Future] => Future[Assertion]): Future[Assertion] =
    withStreamingServer(server.streaming)(test)

  override protected def withStreamingServer(
      server: StreamingMcpServer[Future]
  )(test: BidirectionalMcpClient[Future] => Future[Assertion]): Future[Assertion] =
    withHttpServer(PekkoServerHttpTransport(List("mcp")).serve(server), Nil): transport =>
      McpClient.bidirectional(transport, clientInfo).flatMap(test)

  override protected def withSecuredServer(
      server: SecuredMcpServer[Future, String, String, User],
      token: String
  )(test: McpClient[Future] => Future[Assertion]): Future[Assertion] =
    withHttpServer(server.endpoint(List("mcp")), List(Header.authorization("Bearer", token))): transport =>
      McpClient(transport, clientInfo).flatMap(test)

  override protected def withSecuredStreamingServer(
      server: SecuredStreamingMcpServer[Future, String, String, User],
      token: String
  )(test: BidirectionalMcpClient[Future] => Future[Assertion]): Future[Assertion] =
    val endpoint = SecuredServerStreamingHttpTransport(List("mcp"), PekkoServerHttpTransport(List("mcp"))).serve(server)
    withHttpServer(endpoint, List(Header.authorization("Bearer", token))): transport =>
      McpClient.bidirectional(transport, clientInfo).flatMap(test)

  private def withHttpServer(
      endpoint: ServerEndpoint[PekkoStreams & WebSockets, Future],
      headers: List[Header]
  )(test: PekkoClientHttpTransport => Future[Assertion]): Future[Assertion] =
    given ExecutionContext = actorSystem.dispatcher
    Http()
      .newServerAt("localhost", 0)
      .bind(PekkoHttpServerInterpreter().toRoute(endpoint))
      .flatMap: binding =>
        val backend = PekkoHttpBackend.usingActorSystem(actorSystem)
        val transport =
          PekkoClientHttpTransport(backend, uri"http://localhost:${binding.localAddress.getPort}/mcp", headers = headers)
        test(transport)
          .transformWith: result =>
            transport
              .close()
              .transformWith(_ => backend.close())
              .transformWith(_ => binding.terminate(5.seconds))
              .transform(_ => result)
