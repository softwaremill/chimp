package chimp.client.transport.ox

import chimp.client.integration.SyncToFuture
import chimp.client.transport.{ClientHttpTransportHeaderTests, ClientTransport}
import ox.supervised
import sttp.client4.GenericRequest
import sttp.client4.testing.{RecordingBackend, ResponseStub, SyncBackendStub}
import sttp.model.{Header, Method, StatusCode}
import sttp.shared.Identity

import scala.concurrent.Future

class OxClientHttpTransportHeaderSpec extends ClientHttpTransportHeaderTests[Identity] with SyncToFuture:

  override protected def expectedMethods: Seq[Method] = List(Method.POST, Method.GET, Method.DELETE)

  override protected def withRecordedRequests(
      headers: Seq[Header]
  )(use: ClientTransport[Identity] => Identity[Unit]): Future[List[GenericRequest[?, ?]]] =
    val backend = RecordingBackend(
      SyncBackendStub
        .whenRequestMatches(_.method == Method.GET)
        .thenRespondAdjust("", StatusCode.MethodNotAllowed)
        .whenAnyRequest
        .thenRespond(ResponseStub.adjust(responseBody, StatusCode.Ok, List(Header("Mcp-Session-Id", "s-1"))))
    )
    toFuture:
      supervised:
        val transport = OxClientHttpTransport(backend, mcpUri, headers = headers)
        use(transport)
        waitUntil(backend.allInteractions.exists(_._1.method == Method.GET), attempts = 250, intervalMs = 20)
        transport.close()
      backend.allInteractions.map(_._1)
