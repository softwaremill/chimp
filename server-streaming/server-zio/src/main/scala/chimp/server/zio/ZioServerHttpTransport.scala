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
import zio.{Queue, Task, ZIO}

import java.nio.charset.StandardCharsets

/** ZIO implementation of the streaming HTTP server transport: the SSE stream is a `ZStream` of `ServerSentEvent`, and outbound messages are
  * interleaved with the final response through an unbounded queue drained by a daemon fiber.
  */
final class ZioServerHttpTransport(path: List[String]) extends ServerStreamingHttpTransport[Task, ZioStreams](path):
  val streams: ZioStreams = ZioStreams

  type EventStream = Stream[Throwable, ServerSentEvent]

  val sseBody: StreamBodyIO[Stream[Throwable, Byte], EventStream, ZioStreams] =
    streamTextBody(ZioStreams)(CodecFormat.TextEventStream(), Some(StandardCharsets.UTF_8))
      .map(ZioServerSentEvents.parseBytesToSSE)(ZioServerSentEvents.serialiseSSEToBytes)

  val emptyStream: EventStream = ZStream.empty

  def eventStream(handle: OutboundSink[Task] => Task[Option[Json]]): Task[EventStream] =
    ZIO.succeed {
      ZStream.unwrap {
        for
          queue <- Queue.unbounded[Outbound]
          sink = new OutboundSink[Task]:
            def send(message: JSONRPCMessage): Task[Unit] =
              queue.offer(Outbound.Message(message.asJson)).unit
          _ <- handle(sink)
            .flatMap(response => ZIO.foreachDiscard(response)(json => queue.offer(Outbound.Message(json))))
            .ensuring(queue.offer(Outbound.Close))
            .catchAllCause(_ => ZIO.unit)
            .forkDaemon
        yield ZStream.fromQueue(queue).collectWhile { case Outbound.Message(json) => ServerSentEvent(data = Some(json.noSpaces)) }
      }
    }

  private enum Outbound:
    case Message(json: Json)
    case Close
