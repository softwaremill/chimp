package chimp.server.pekko

import chimp.protocol.JSONRPCMessage
import chimp.server.OutboundSink
import chimp.server.transport.ServerStreamingHttpTransport
import io.circe.Json
import io.circe.syntax.*
import org.apache.pekko.NotUsed
import org.apache.pekko.stream.scaladsl.Source
import org.apache.pekko.stream.{Materializer, OverflowStrategy}
import org.slf4j.LoggerFactory
import sttp.capabilities.pekko.PekkoStreams
import sttp.model.sse.ServerSentEvent
import sttp.tapir.server.pekkohttp.PekkoServerSentEvents
import sttp.tapir.{streamTextBody, CodecFormat, StreamBodyIO}

import java.nio.charset.StandardCharsets
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

final class PekkoServerHttpTransport(
    path: List[String],
    outboundBufferSize: Int = PekkoOutbound.defaultBufferSize,
    maxConcurrentSends: Int = PekkoOutbound.defaultMaxConcurrentSends
)(using mat: Materializer)
    extends ServerStreamingHttpTransport[Future, PekkoStreams](path):

  private val log = LoggerFactory.getLogger(classOf[PekkoServerHttpTransport])

  private given ExecutionContext = mat.executionContext

  val streams: PekkoStreams = PekkoStreams

  type EventStream = Source[ServerSentEvent, Any]

  val sseBody: StreamBodyIO[streams.BinaryStream, EventStream, PekkoStreams] =
    streamTextBody(PekkoStreams)(CodecFormat.TextEventStream(), Some(StandardCharsets.UTF_8))
      .map(PekkoServerSentEvents.parseBytesToSSE)(PekkoServerSentEvents.serialiseSSEToBytes)

  val emptyStream: EventStream = Source.empty[ServerSentEvent]

  def eventStream(handle: OutboundSink[Future] => Future[Option[Json]]): Future[EventStream] =
    Future.successful:
      Source
        .queue[Json](outboundBufferSize, OverflowStrategy.backpressure, maxConcurrentSends)
        .mapMaterializedValue: queue =>
          val sink = new OutboundSink[Future]:
            def send(message: JSONRPCMessage): Future[Unit] = PekkoOutbound.offer(queue, message.asJson.deepDropNullValues)
          Try(handle(sink))
            .fold(Future.failed, identity)
            .onComplete:
              case Success(response) =>
                val lastMessage = response.fold(Future.unit)(json => PekkoOutbound.offer(queue, json))
                lastMessage.onComplete(_ => queue.complete())
              case Failure(t) =>
                log.warn(s"Failed to handle the JSON-RPC message: ${t.getMessage}")
                queue.complete()
          NotUsed
        .map(json => ServerSentEvent(data = Some(json.noSpaces)))
