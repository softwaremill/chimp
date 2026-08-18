package chimp.server.pekko

import chimp.client.transport.pekko.PekkoClientHttpTransport
import chimp.client.{BidirectionalMcpClient, McpClient}
import chimp.protocol.Implementation
import chimp.server.{McpServer, McpServerStreamingTests, McpServerTests, StreamingMcpServer}
import org.apache.pekko.http.scaladsl.Http
import org.scalatest.Assertion
import sttp.client4.pekkohttp.PekkoHttpBackend
import sttp.model.Uri.UriContext
import sttp.tapir.server.pekkohttp.PekkoHttpServerInterpreter

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

class PekkoMcpServerHttpSpec extends McpServerTests[Future] with McpServerStreamingTests[Future] with PekkoToFuture:
  private val clientInfo = Implementation("chimp-server-test", "0.0.1")

  override protected def withServer(server: McpServer[Future])(test: McpClient[Future] => Future[Assertion]): Future[Assertion] =
    withStreamingServer(server.streaming)(test)

  override protected def withStreamingServer(
      server: StreamingMcpServer[Future]
  )(test: BidirectionalMcpClient[Future] => Future[Assertion]): Future[Assertion] =
    given ExecutionContext = actorSystem.dispatcher
    val endpoint = PekkoServerHttpTransport(List("mcp")).serve(server)
    Http()
      .newServerAt("localhost", 0)
      .bind(PekkoHttpServerInterpreter().toRoute(endpoint))
      .flatMap: binding =>
        val backend = PekkoHttpBackend.usingActorSystem(actorSystem)
        val transport = PekkoClientHttpTransport(backend, uri"http://localhost:${binding.localAddress.getPort}/mcp")
        McpClient
          .bidirectional(transport, clientInfo)
          .flatMap(test)
          .transformWith: result =>
            transport
              .close()
              .transformWith(_ => backend.close())
              .transformWith(_ => binding.terminate(5.seconds))
              .transform(_ => result)
