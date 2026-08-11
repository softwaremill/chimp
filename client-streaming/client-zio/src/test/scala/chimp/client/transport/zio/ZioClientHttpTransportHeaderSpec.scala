package chimp.client.transport.zio

import chimp.client.transport.{ClientHttpTransportHeaderTests, ClientTransport}
import sttp.capabilities.zio.ZioStreams
import sttp.client4.GenericRequest
import sttp.client4.testing.{RecordingBackend, ResponseStub, StreamBackendStub}
import sttp.model.{Header, Method, StatusCode}
import zio.Task
import zio.stream.ZStream

import java.nio.charset.StandardCharsets
import scala.concurrent.Future

class ZioClientHttpTransportHeaderSpec extends ClientHttpTransportHeaderTests[Task] with ZioToFuture:

  override protected def expectedMethods: Seq[Method] = List(Method.POST, Method.GET, Method.DELETE)

  override protected def withRecordedRequests(
      headers: Seq[Header]
  )(use: ClientTransport[Task] => Task[Unit]): Future[List[GenericRequest[?, ?]]] =
    val backend = RecordingBackend(
      StreamBackendStub[Task, ZioStreams](monad)
        .whenRequestMatches(_.method == Method.GET)
        .thenRespondAdjust("", StatusCode.MethodNotAllowed)
        .whenAnyRequest
        .thenRespond(
          ResponseStub.adjust(
            ZStream.fromIterable(responseBody.getBytes(StandardCharsets.UTF_8)),
            StatusCode.Ok,
            List(Header("Mcp-Session-Id", "s-1"))
          )
        )
    )
    toFuture:
      for
        transport <- ZioClientHttpTransport(backend, mcpUri, headers = headers)
        _ <- use(transport)
        _ <- waitUntil(backend.allInteractions.exists(_._1.method == Method.GET), attempts = 250, intervalMs = 20)
        _ <- transport.close()
      yield backend.allInteractions.map(_._1)
