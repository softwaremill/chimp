package chimp.server.pekko

import chimp.protocol.JSONRPCMessage
import chimp.server.OutboundSink
import chimp.server.transport.ServerStreamingHttpTransport
import io.circe.Json
import io.circe.syntax.*
import org.apache.pekko.NotUsed
import org.apache.pekko.stream.scaladsl.{Flow, Source}
import org.apache.pekko.stream.{Materializer, OverflowStrategy}
import org.slf4j.LoggerFactory
import sttp.capabilities.pekko.PekkoStreams
import sttp.model.sse.ServerSentEvent
import sttp.tapir.server.pekkohttp.PekkoServerSentEvents
import sttp.tapir.{streamTextBody, CodecFormat, StreamBodyIO}

import java.nio.charset.StandardCharsets
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/** @param keepAlive
  *   If set, a data-less `ping` Server-Sent Event is emitted on the response stream at this interval, to keep idle connections open through
  *   proxies. The events carry no data and are ignored by MCP clients.
  */
final class PekkoServerHttpTransport(
    path: List[String],
    outboundBufferSize: Int = PekkoOutbound.defaultBufferSize,
    maxConcurrentSends: Int = PekkoOutbound.defaultMaxConcurrentSends,
    keepAlive: Option[FiniteDuration] = None
)(using mat: Materializer)
    extends ServerStreamingHttpTransport[Future, PekkoStreams](path):

  private val log = LoggerFactory.getLogger(classOf[PekkoServerHttpTransport])

  private given ExecutionContext = mat.executionContext

  private val pingEvent = ServerSentEvent(eventType = Some("ping"))

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
                log.warn("Failed to handle the JSON-RPC message", t)
                queue.complete()
          NotUsed
        .map(json => ServerSentEvent(data = Some(json.noSpaces)))
        .via(keepAlive.fold(Flow[ServerSentEvent])(interval => Flow[ServerSentEvent].keepAlive(interval, () => pingEvent)))
