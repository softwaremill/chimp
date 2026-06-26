package chimp.client.transport.ox

import chimp.client.integration.{McpClientStreamingHttpIntegrationSpec, SyncToFuture}
import chimp.client.transport.ClientBidirectionalTransport
import ox.supervised
import sttp.client4.{DefaultSyncBackend, SyncBackend}
import sttp.model.Uri
import sttp.shared.Identity

import scala.concurrent.duration.FiniteDuration

class OxMcpClientHttpIntegrationSpec extends McpClientStreamingHttpIntegrationSpec[Identity, SyncBackend] with SyncToFuture:

  override def usingBackend[A](use: SyncBackend => Identity[A]): Identity[A] =
    val backend = DefaultSyncBackend()
    try use(backend)
    finally backend.close()

  override def usingBidirectionalTransport[A](b: SyncBackend, uri: Uri, timeout: FiniteDuration)(
      use: ClientBidirectionalTransport[Identity] => Identity[A]
  ): Identity[A] =
    supervised:
      val transport = OxClientHttpTransport(b, uri, timeout = timeout)
      try use(transport)
      finally transport.close()
