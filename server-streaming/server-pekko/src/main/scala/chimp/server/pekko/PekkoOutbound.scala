package chimp.server.pekko

import org.apache.pekko.stream.QueueOfferResult
import org.apache.pekko.stream.scaladsl.SourceQueueWithComplete
import org.slf4j.LoggerFactory

import scala.concurrent.{ExecutionContext, Future}

private[pekko] object PekkoOutbound:
  private val log = LoggerFactory.getLogger("chimp.server.pekko.PekkoOutbound")

  val defaultBufferSize: Int = 64
  val defaultMaxConcurrentSends: Int = 64

  def offer[A](queue: SourceQueueWithComplete[A], message: A)(using ExecutionContext): Future[Unit] =
    queue.offer(message).flatMap {
      case QueueOfferResult.Enqueued =>
        Future.unit
      case QueueOfferResult.Dropped =>
        log.warn("An outbound JSON-RPC message was dropped")
        Future.unit
      case QueueOfferResult.QueueClosed =>
        log.debug("An outbound JSON-RPC message was not sent, the outbound stream is already closed")
        Future.unit
      case QueueOfferResult.Failure(t) =>
        Future.failed(t)
    }
