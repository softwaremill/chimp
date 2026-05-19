package chimp.client.transport.zio

import chimp.client.integration.StreamingHttpIntegrationSpec
import chimp.client.transport.BidirectionalTransport
import sttp.capabilities.zio.ZioStreams
import sttp.client4.StreamBackend
import sttp.client4.httpclient.zio.HttpClientZioBackend
import sttp.model.Uri
import zio.{Task, ZIO}

class ZioHttpStreamingIntegrationSpec
    extends StreamingHttpIntegrationSpec[Task, StreamBackend[Task, ZioStreams]]
    with ZioFutureFixtures:

  override def usingBackend[A](use: StreamBackend[Task, ZioStreams] => Task[A]): Task[A] =
    HttpClientZioBackend().flatMap: b =>
      use(b).ensuring(b.close().orDie)

  override def usingBidirectionalTransport[A](b: StreamBackend[Task, ZioStreams], uri: Uri)(
      use: BidirectionalTransport[Task] => Task[A]
  ): Task[A] =
    ZIO.scoped(ZioStreamingHttpTransport.make(b, uri).flatMap(use))
