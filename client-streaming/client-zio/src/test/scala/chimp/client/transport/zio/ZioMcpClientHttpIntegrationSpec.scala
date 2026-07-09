package chimp.client.transport.zio

import chimp.client.integration.McpClientStreamingHttpIntegrationSpec
import chimp.client.transport.ClientBidirectionalTransport
import chimp.protocol.ProtocolVersion
import sttp.capabilities.zio.ZioStreams
import sttp.client4.StreamBackend
import sttp.client4.httpclient.zio.HttpClientZioBackend
import sttp.model.Uri
import zio.{Task, ZIO}

import scala.concurrent.duration.FiniteDuration

class ZioMcpClientHttpIntegrationSpec extends McpClientStreamingHttpIntegrationSpec[Task, StreamBackend[Task, ZioStreams]] with ZioToFuture:

  override def usingBackend[A](use: StreamBackend[Task, ZioStreams] => Task[A]): Task[A] =
    HttpClientZioBackend().flatMap: b =>
      use(b).ensuring(b.close().orDie)

  override def usingBidirectionalTransport[A](b: StreamBackend[Task, ZioStreams], uri: Uri, timeout: FiniteDuration)(
      use: ClientBidirectionalTransport[Task] => Task[A]
  ): Task[A] =
    ZIO.scoped(ZioClientHttpTransport.scoped(b, uri, ProtocolVersion.Latest, timeout).flatMap(use))
