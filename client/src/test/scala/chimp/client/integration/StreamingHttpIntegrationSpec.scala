package chimp.client.integration

import chimp.client.{BidirectionalMcpClient, McpClient}
import chimp.client.transport.{BidirectionalTransport, Transport}
import chimp.protocol.*
import org.scalatest.Assertion
import sttp.model.Uri
import sttp.monad.syntax.*

import scala.concurrent.Future

abstract class StreamingHttpIntegrationSpec[F[_], B] extends HttpIntegrationSpec[F, B] with BidirectionalMcpClientTests[F]:
  this: ToFuture[F] =>

  def usingBidirectionalTransport[A](b: B, uri: Uri)(use: BidirectionalTransport[F] => F[A]): F[A]

  override def usingTransport[A](backend: B, uri: Uri)(use: Transport[F] => F[A]): F[A] =
    usingBidirectionalTransport(backend, uri)(use)

  private val clientInfo = Implementation(name = "chimp-integration", version = "0.0.1")

  override protected def withBidirectionalClient(
      rootsHandler: Option[() => F[ListRootsResult]] = None,
      samplingHandler: Option[CreateMessageRequest => F[CreateMessageResult]] = None,
      elicitationHandler: Option[ElicitRequest => F[ElicitResult]] = None
  )(test: BidirectionalMcpClient[F] => F[Assertion]): Future[Assertion] =
    toFuture(
      usingBackend: backend =>
        usingBidirectionalTransport(backend, container.mcpUri): transport =>
          McpClient[F](transport, clientInfo, rootsHandler, samplingHandler, elicitationHandler, ProtocolVersion.Latest)
            .flatMap: client =>
              test(client).flatMap(assertion => client.close().map(_ => assertion))
    )
