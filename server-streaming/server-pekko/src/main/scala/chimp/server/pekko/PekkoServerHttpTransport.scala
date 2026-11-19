package chimp.server.pekko

import chimp.protocol.JSONRPCMessage
import chimp.server.{ListenRequest, McpResponse, OutboundSink, SubscriptionOutput}
import chimp.server.transport.{plainJsonError, ServerStreamingHttpTransport}
import io.circe.Json
import io.circe.syntax.*
import org.apache.pekko.NotUsed
import org.apache.pekko.stream.scaladsl.{Sink, Source}
import org.apache.pekko.stream.{Materializer, OverflowStrategy}
import org.slf4j.LoggerFactory
import sttp.capabilities.pekko.PekkoStreams
import sttp.model.StatusCode
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

  def eventStream(handle: OutboundSink[Future] => Future[McpResponse]): Future[Either[(StatusCode, Json), EventStream]] =
    val (queue, outbound) =
      Source.queue[Outbound](outboundBufferSize, OverflowStrategy.backpressure, maxConcurrentSends).preMaterialize()
    val sink = new OutboundSink[Future]:
      def send(message: JSONRPCMessage): Future[Unit] = PekkoOutbound.offer(queue, Outbound.Message(message.asJson.deepDropNullValues))
    Try(handle(sink))
      .fold(Future.failed, identity)
      .onComplete:
        case Success(response) => PekkoOutbound.offer(queue, Outbound.Final(response)).onComplete(_ => queue.complete())
        case Failure(t)        =>
          log.warn("Failed to handle the JSON-RPC message", t)
          queue.complete()
    outbound
      .prefixAndTail(1)
      .runWith(Sink.head)
      .map: (first, rest) =>
        val stream = (Source(first) ++ rest)
          .mapConcat:
            case Outbound.Message(json)   => List(json)
            case Outbound.Final(response) => response.body.toList
          .map(json => ServerSentEvent(data = Some(json.noSpaces)))
        first.headOption match
          case Some(Outbound.Final(response)) =>
            plainJsonError(response) match
              case Some(error) =>
                val _ = rest.runWith(Sink.ignore)
                Left(error)
              case None => Right(stream)
          case _ => Right(stream)

  private[server] def subscriptionStream(listen: ListenRequest): EventStream =
    Source
      .queue[Json](outboundBufferSize, OverflowStrategy.backpressure, maxConcurrentSends)
      .mapMaterializedValue: queue =>
        listen.open(new SubscriptionOutput:
          def send(message: Json): Unit =
            val _ = PekkoOutbound.offer(queue, message)
          def close(): Unit = queue.complete())
      .watchTermination(): (subscription, done) =>
        done.onComplete(_ => subscription.cancel())
        NotUsed
      .map(json => ServerSentEvent(data = Some(json.noSpaces)))

  private enum Outbound:
    case Message(json: Json)
    case Final(response: McpResponse)
