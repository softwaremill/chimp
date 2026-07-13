package chimp.server.pekko

import chimp.protocol.JSONRPCMessage
import chimp.server.OutboundSink
import chimp.server.transport.ServerStreamingHttpTransport
import io.circe.Json
import io.circe.syntax.*
import org.apache.pekko.stream.OverflowStrategy.backpressure
import org.apache.pekko.stream.scaladsl.{Keep, Sink, Source}
import org.apache.pekko.stream.{Materializer, OverflowStrategy, QueueOfferResult}
import org.slf4j.LoggerFactory
import sttp.capabilities.pekko.PekkoStreams
import sttp.model.sse.ServerSentEvent
import sttp.tapir.server.pekkohttp.PekkoServerSentEvents
import sttp.tapir.{streamTextBody, CodecFormat, StreamBodyIO}

import java.nio.charset.StandardCharsets
import scala.concurrent.Future

final class PekkoServerHttpTransport(path: List[String])(using mat: Materializer)
    extends ServerStreamingHttpTransport[Future, PekkoStreams](path):

  private val log = LoggerFactory.getLogger(classOf[PekkoServerHttpTransport])

  override val streams: PekkoStreams = PekkoStreams

  override type EventStream = Source[ServerSentEvent, Any]

  override def sseBody: StreamBodyIO[streams.BinaryStream, EventStream, PekkoStreams] =
    streamTextBody(PekkoStreams)(CodecFormat.TextEventStream(), Some(StandardCharsets.UTF_8))
      .map(PekkoServerSentEvents.parseBytesToSSE)(PekkoServerSentEvents.serialiseSSEToBytes)

  override def emptyStream: EventStream = Source.empty[ServerSentEvent]

  override def eventStream(handle: OutboundSink[Future] => Future[Option[Json]]): Future[EventStream] =
    val queue = Source.queue[Json](1024, backpressure).toMat(Sink.ignore)(Keep.left).run()
    val sink = new OutboundSink[Future]:
      def send(message: JSONRPCMessage): Future[Unit] =
        queue
          .offer(message.asJson)
          .flatMap:
            case QueueOfferResult.Enqueued => Future.unit
            case QueueOfferResult.Dropped  =>
              log.warn("Outbound JSON-RPC message was dropped")
              Future.unit
            case QueueOfferResult.Failure(ex) => Future.failed(ex)
            case QueueOfferResult.QueueClosed => Future.unit
    handle(sink)
      .flatMap:
        case Some(response) =>
          queue
            .offer(response)
            .flatMap:
              case QueueOfferResult.Enqueued => Future.unit
              case QueueOfferResult.Dropped  =>
                log.warn("Outbound JSON-RPC message was dropped")
                Future.unit
              case QueueOfferResult.Failure(ex) => Future.failed(ex)
              case QueueOfferResult.QueueClosed => Future.unit
        case _ => Future.unit
      .recover:
        case NonFatal(_) => Future.unit
      .andThen:
        case _ => queue.complete()
         
        
