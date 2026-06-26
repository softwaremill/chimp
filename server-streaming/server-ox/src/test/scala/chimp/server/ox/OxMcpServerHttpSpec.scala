package chimp.server.ox

import chimp.client.transport.ClientTransport
import chimp.client.transport.ox.OxClientHttpTransport
import chimp.client.{BidirectionalMcpClient, McpClient}
import chimp.protocol.{Implementation, ProtocolVersion}
import chimp.server.{McpServer, McpServerStreamingTests, McpServerTests, StreamingMcpServer, SyncToFuture}
import org.scalatest.Assertion
import ox.supervised
import sttp.client4.DefaultSyncBackend
import sttp.model.Uri.UriContext
import sttp.shared.Identity
import sttp.tapir.server.netty.sync.NettySyncServer

import scala.concurrent.Future

class OxMcpServerHttpSpec extends McpServerTests[Identity] with McpServerStreamingTests[Identity] with SyncToFuture:
  private val clientInfo = Implementation("chimp-server-test", "0.0.1")

  override protected def withServer(server: McpServer[Identity])(test: McpClient[Identity] => Identity[Assertion]): Future[Assertion] =
    withStreamingServer(server.streaming)(test)

  override protected def withStreamingServer(
      server: StreamingMcpServer[Identity]
  )(test: BidirectionalMcpClient[Identity] => Identity[Assertion]): Future[Assertion] =
    toFuture:
      supervised:
        val endpoint = OxServerHttpTransport(List("mcp")).serve(server)
        val binding = NettySyncServer().port(0).addEndpoint(endpoint).start()
        try
          val backend = DefaultSyncBackend()
          try
            val transport =
              OxClientHttpTransport(
                backend,
                uri"http://localhost:${binding.port}/mcp",
                ProtocolVersion.Latest,
                ClientTransport.defaultTimeout
              )
            try test(McpClient.bidirectional(transport, clientInfo))
            finally transport.close()
          finally backend.close()
        finally binding.stop()
