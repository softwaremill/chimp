package chimp.client.transport.pekko

import chimp.client.integration.McpClientStreamingHttpIntegrationSpec
import chimp.client.transport.ClientBidirectionalTransport
import chimp.protocol.ProtocolVersion
import sttp.capabilities.pekko.PekkoStreams
import sttp.client4.StreamBackend
import sttp.client4.pekkohttp.PekkoHttpBackend
import sttp.model.Uri

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

class PekkoMcpClientHttpIntegrationSpec
    extends McpClientStreamingHttpIntegrationSpec[Future, StreamBackend[Future, PekkoStreams]]
    with PekkoToFuture:

  override def usingBackend[A](use: StreamBackend[Future, PekkoStreams] => Future[A]): Future[A] =
    val backend = PekkoHttpBackend.usingActorSystem(actorSystem)
    monad.ensure2(use(backend), backend.close())

  override def usingBidirectionalTransport[A](b: StreamBackend[Future, PekkoStreams], uri: Uri, timeout: FiniteDuration)(
      use: ClientBidirectionalTransport[Future] => Future[A]
  ): Future[A] =
    val transport = PekkoClientHttpTransport(b, uri, ProtocolVersion.Latest, timeout)
    monad.ensure2(use(transport), transport.close())
