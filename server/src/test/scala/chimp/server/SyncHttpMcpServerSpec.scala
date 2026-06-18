package chimp.server

import chimp.client.McpClient
import chimp.client.transport.ClientHttpTransport
import chimp.protocol.Implementation
import org.scalatest.Assertion
import ox.supervised
import sttp.client4.*
import sttp.shared.Identity
import sttp.tapir.server.netty.sync.NettySyncServer

import scala.concurrent.Future

class SyncHttpMcpServerSpec extends McpServerTests[Identity] with SyncToFuture:
  private val clientInfo = Implementation("chimp-server-test", "0.0.1")

  override protected def withServer(server: McpServer[Identity])(test: McpClient[Identity] => Identity[Assertion]): Future[Assertion] =
    toFuture:
      supervised:
        val binding = NettySyncServer().port(0).addEndpoint(server.endpoint(List("mcp"))).start()
        try
          val backend = DefaultSyncBackend()
          try
            val transport = ClientHttpTransport[Identity](backend, uri"http://localhost:${binding.port}/mcp")
            try test(McpClient(transport, clientInfo))
            finally transport.close()
          finally backend.close()
        finally binding.stop()
