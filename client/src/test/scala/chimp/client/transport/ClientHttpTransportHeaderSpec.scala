package chimp.client.transport

import chimp.client.integration.SyncToFuture
import sttp.client4.GenericRequest
import sttp.client4.testing.{RecordingBackend, ResponseStub, SyncBackendStub}
import sttp.model.{Header, StatusCode}
import sttp.shared.Identity

import scala.concurrent.Future

class ClientHttpTransportHeaderSpec extends ClientHttpTransportHeaderTests[Identity] with SyncToFuture:

  override protected def withRecordedRequests(
      headers: Seq[Header]
  )(use: ClientTransport[Identity] => Identity[Unit]): Future[List[GenericRequest[?, ?]]] =
    val backend = RecordingBackend(
      SyncBackendStub.whenAnyRequest.thenRespond(ResponseStub.adjust(responseBody, StatusCode.Ok, List(Header("Mcp-Session-Id", "s-1"))))
    )
    toFuture:
      val transport = ClientHttpTransport[Identity](backend, mcpUri, headers = headers)
      use(transport)
      transport.close()
      backend.allInteractions.map(_._1)
