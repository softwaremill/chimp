package chimp.server.ox

import chimp.client.transport.{ClientHttpTransport, ClientTransport}
import chimp.client.transport.ox.OxClientHttpTransport
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
  StreamingMcpServer,
  SyncToFuture
}
import org.scalatest.Assertion
import ox.{supervised, Ox}
import sttp.capabilities.WebSockets
import sttp.client4.{DefaultSyncBackend, SyncBackend}
import sttp.model.Header
import sttp.model.Uri.UriContext
import sttp.shared.Identity
import sttp.tapir.server.netty.sync.{NettySyncServer, OxStreams}
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

class OxMcpServerHttpSpec
    extends McpServerTests[Identity]
    with McpServerStreamingTests[Identity]
    with SecuredMcpServerTests[Identity]
    with SecuredMcpServerStreamingTests[Identity]
    with SyncToFuture:
  private val clientInfo = Implementation("chimp-server-test", "0.0.1")

  override protected def withServer(server: McpServer[Identity])(test: McpClient[Identity] => Identity[Assertion]): Future[Assertion] =
    withStreamingServer(server.streaming)(test)

  override protected def withStreamingServer(
      server: StreamingMcpServer[Identity]
  )(test: BidirectionalMcpClient[Identity] => Identity[Assertion]): Future[Assertion] =
    withHttpServer(OxServerHttpTransport(List("mcp")).serve(server)): (port, backend, ox) =>
      given Ox = ox
      val transport =
        OxClientHttpTransport(backend, uri"http://localhost:$port/mcp", ProtocolVersion.Latest, ClientTransport.defaultTimeout)
      try test(McpClient.bidirectional(transport, clientInfo))
      finally transport.close()

  override protected def withSecuredServer(
      server: SecuredMcpServer[Identity, String, String, User],
      token: String
  )(test: McpClient[Identity] => Identity[Assertion]): Future[Assertion] =
    withHttpServer(server.endpoint(List("mcp"))): (port, backend, _) =>
      val transport = ClientHttpTransport[Identity](
        backend,
        uri"http://localhost:$port/mcp",
        headers = List(Header.authorization("Bearer", token))
      )
      try test(McpClient(transport, clientInfo))
      finally transport.close()

  override protected def withSecuredStreamingServer(
      server: SecuredStreamingMcpServer[Identity, String, String, User],
      token: String
  )(test: BidirectionalMcpClient[Identity] => Identity[Assertion]): Future[Assertion] =
    val endpoint = SecuredServerStreamingHttpTransport(List("mcp"), OxServerHttpTransport(List("mcp"))).serve(server)
    withHttpServer(endpoint): (port, backend, ox) =>
      given Ox = ox
      val transport = OxClientHttpTransport(
        backend,
        uri"http://localhost:$port/mcp",
        ProtocolVersion.Latest,
        ClientTransport.defaultTimeout,
        headers = List(Header.authorization("Bearer", token))
      )
      try test(McpClient.bidirectional(transport, clientInfo))
      finally transport.close()

  private def withHttpServer(
      endpoint: ServerEndpoint[OxStreams & WebSockets, Identity]
  )(test: (Int, SyncBackend, Ox) => Assertion): Future[Assertion] =
    toFuture:
      supervised:
        val binding = NettySyncServer().port(0).addEndpoint(endpoint).start()
        try
          val backend = DefaultSyncBackend()
          try test(binding.port, backend, summon[Ox])
          finally backend.close()
        finally binding.stop()
