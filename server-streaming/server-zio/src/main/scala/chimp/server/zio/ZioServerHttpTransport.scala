package chimp.server.zio

import chimp.protocol.JSONRPCMessage
import chimp.server.{ListenRequest, McpResponse, OutboundSink, SubscriptionOutput}
import chimp.server.transport.{plainJsonError, ServerStreamingHttpTransport}
import io.circe.Json
import io.circe.syntax.*
import sttp.capabilities.zio.ZioStreams
import sttp.model.StatusCode
import sttp.model.sse.ServerSentEvent
import sttp.tapir.*
import sttp.tapir.ztapir.ZioServerSentEvents
import zio.stream.{Stream, ZStream}
import zio.{Queue, Task, Unsafe, ZIO}

import java.nio.charset.StandardCharsets

final class ZioServerHttpTransport(path: List[String]) extends ServerStreamingHttpTransport[Task, ZioStreams](path):
  val streams: ZioStreams = ZioStreams

  type EventStream = Stream[Throwable, ServerSentEvent]

  val sseBody: StreamBodyIO[streams.BinaryStream, EventStream, ZioStreams] =
    streamTextBody(ZioStreams)(CodecFormat.TextEventStream(), Some(StandardCharsets.UTF_8))
      .map(ZioServerSentEvents.parseBytesToSSE)(ZioServerSentEvents.serialiseSSEToBytes)

  val emptyStream: EventStream = ZStream.empty

  def eventStream(handle: OutboundSink[Task] => Task[McpResponse]): Task[Either[(StatusCode, Json), EventStream]] =
    for
      queue <- Queue.unbounded[Outbound]
      sink = new OutboundSink[Task]:
        def send(message: JSONRPCMessage): Task[Unit] =
          queue.offer(Outbound.Message(message.asJson)).unit
      worker <- handle(sink)
        .flatMap(response => queue.offer(Outbound.Final(response)))
        .catchAllCause(cause => ZIO.logWarningCause("Failed to handle the JSON-RPC message", cause))
        .ensuring(queue.offer(Outbound.Close))
        .forkDaemon
      first <- queue.take
    yield
      val stream = (ZStream.succeed(first) ++ ZStream.fromQueue(queue))
        .collectWhile:
          case Outbound.Message(json)   => Some(json)
          case Outbound.Final(response) => response.body
        .collectSome
        .map(json => ServerSentEvent(data = Some(json.noSpaces)))
        .ensuring(worker.interrupt)
      first match
        case Outbound.Final(response) => plainJsonError(response).toLeft(stream)
        case _                        => Right(stream)

  private[server] def subscriptionStream(listen: ListenRequest): EventStream =
    ZStream.unwrapScoped {
      for
        queue <- ZIO.acquireRelease(Queue.unbounded[Outbound])(_.shutdown)
        runtime <- ZIO.runtime[Any]
        output = new SubscriptionOutput:
          def send(message: Json): Unit = offer(Outbound.Message(message))
          def close(): Unit = offer(Outbound.Close)
          private def offer(outbound: Outbound): Unit =
            Unsafe.unsafe { implicit unsafe =>
              val _ = runtime.unsafe.run(queue.offer(outbound))
            }
        _ <- ZIO.acquireRelease(ZIO.succeed(listen.open(output)))(subscription => ZIO.succeed(subscription.cancel()))
      yield ZStream.fromQueue(queue).collectWhile { case Outbound.Message(json) => ServerSentEvent(data = Some(json.noSpaces)) }
    }

  private enum Outbound:
    case Message(json: Json)
    case Final(response: McpResponse)
    case Close
