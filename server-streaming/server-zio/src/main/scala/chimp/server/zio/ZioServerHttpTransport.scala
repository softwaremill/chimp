package chimp.server.zio

import chimp.protocol.JSONRPCMessage
import chimp.server.OutboundSink
import chimp.server.transport.ServerStreamingHttpTransport
import io.circe.Json
import io.circe.syntax.*
import sttp.capabilities.zio.ZioStreams
import sttp.model.sse.ServerSentEvent
import sttp.tapir.*
import sttp.tapir.ztapir.ZioServerSentEvents
import zio.stream.{Stream, ZStream}
import zio.{Duration, Queue, Schedule, Task, ZIO}

import java.nio.charset.StandardCharsets
import scala.concurrent.duration.FiniteDuration

/** @param keepAlive
  *   If set, a data-less `ping` Server-Sent Event is emitted on the response stream at this interval, to keep idle connections open through
  *   proxies. The events carry no data and are ignored by MCP clients.
  */
final class ZioServerHttpTransport(path: List[String], keepAlive: Option[FiniteDuration] = None)
    extends ServerStreamingHttpTransport[Task, ZioStreams](path):
  val streams: ZioStreams = ZioStreams

  type EventStream = Stream[Throwable, ServerSentEvent]

  val sseBody: StreamBodyIO[streams.BinaryStream, EventStream, ZioStreams] =
    streamTextBody(ZioStreams)(CodecFormat.TextEventStream(), Some(StandardCharsets.UTF_8))
      .map(ZioServerSentEvents.parseBytesToSSE)(ZioServerSentEvents.serialiseSSEToBytes)

  val emptyStream: EventStream = ZStream.empty

  private val pingEvent = ServerSentEvent(eventType = Some("ping"))

  def eventStream(handle: OutboundSink[Task] => Task[Option[Json]]): Task[EventStream] =
    ZIO.succeed {
      ZStream.unwrapScoped {
        for
          queue <- Queue.unbounded[Outbound]
          sink = new OutboundSink[Task]:
            def send(message: JSONRPCMessage): Task[Unit] =
              queue.offer(Outbound.Message(message.asJson)).unit
          _ <- handle(sink)
            .flatMap(response => ZIO.foreachDiscard(response)(json => queue.offer(Outbound.Message(json))))
            .ensuring(queue.offer(Outbound.Close))
            .catchAllCause(_ => ZIO.unit)
            .forkScoped
        yield
          val messages = ZStream.fromQueue(queue).collectWhile { case Outbound.Message(json) =>
            ServerSentEvent(data = Some(json.noSpaces))
          }
          keepAlive.fold(messages)(interval =>
            messages.mergeHaltLeft(ZStream.fromSchedule(Schedule.spaced(Duration.fromScala(interval))).as(pingEvent))
          )
      }
    }

  private enum Outbound:
    case Message(json: Json)
    case Close
