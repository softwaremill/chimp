package chimp.server.zio

import chimp.protocol.JSONRPCMessage
import chimp.server.{McpServerStreaming, OutboundSink}
import io.circe.Json
import io.circe.syntax.*
import sttp.capabilities.zio.ZioStreams
import sttp.model.sse.ServerSentEvent
import sttp.tapir.*
import sttp.tapir.ztapir.ZioServerSentEvents
import zio.stream.{Stream, ZStream}
import zio.{Queue, Task, ZIO}

import java.nio.charset.StandardCharsets

object ZioMcpServerStreaming extends McpServerStreaming[Task, ZioStreams]:
  val streams: ZioStreams = ZioStreams

  type EventStream = Stream[Throwable, ServerSentEvent]

  val sseBody: StreamBodyIO[Stream[Throwable, Byte], EventStream, ZioStreams] =
    streamTextBody(ZioStreams)(CodecFormat.TextEventStream(), Some(StandardCharsets.UTF_8))
      .map(ZioServerSentEvents.parseBytesToSSE)(ZioServerSentEvents.serialiseSSEToBytes)

  val emptyEvents: EventStream = ZStream.empty

  def eventStream(handle: OutboundSink[Task] => Task[Option[Json]]): Task[EventStream] =
    ZIO.succeed {
      ZStream.unwrap {
        for
          queue <- Queue.unbounded[Option[ServerSentEvent]]
          sink = new OutboundSink[Task]:
            def send(message: JSONRPCMessage): Task[Unit] =
              queue.offer(Some(ServerSentEvent(data = Some(message.asJson.noSpaces)))).unit
          _ <- handle(sink)
            .flatMap { message =>
              ZIO.foreachDiscard(message)(message => queue.offer(Some(ServerSentEvent(data = Some(message.noSpaces))))) *> queue.offer(None)
            }
            .catchAllCause(_ => queue.offer(None).unit)
            .forkDaemon
        yield ZStream.fromQueue(queue).takeWhile(_.isDefined).collectSome
      }
    }
