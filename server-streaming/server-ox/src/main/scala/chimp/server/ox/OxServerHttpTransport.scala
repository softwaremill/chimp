package chimp.server.ox

import chimp.protocol.JSONRPCMessage
import chimp.server.OutboundSink
import chimp.server.transport.ServerStreamingHttpTransport
import io.circe.Json
import io.circe.syntax.*
import ox.*
import ox.channels.Channel
import ox.flow.Flow
import sttp.model.sse.ServerSentEvent
import sttp.shared.Identity
import sttp.tapir.StreamBodyIO
import sttp.tapir.server.netty.sync.{serverSentEventsBody, OxStreams}

import scala.concurrent.duration.FiniteDuration

/** @param keepAlive
  *   If set, a data-less `ping` Server-Sent Event is emitted on the response stream at this interval, to keep idle connections open through
  *   proxies. The events carry no data and are ignored by MCP clients.
  */
final class OxServerHttpTransport(path: List[String], keepAlive: Option[FiniteDuration] = None)
    extends ServerStreamingHttpTransport[Identity, OxStreams](path):
  val streams: OxStreams = OxStreams

  type EventStream = Flow[ServerSentEvent]

  val sseBody: StreamBodyIO[streams.BinaryStream, EventStream, OxStreams] = serverSentEventsBody

  val emptyStream: EventStream = Flow.empty

  private val pingEvent = ServerSentEvent(eventType = Some("ping"))

  def eventStream(handle: OutboundSink[Identity] => Option[Json]): Flow[ServerSentEvent] =
    val messages: Flow[ServerSentEvent] = Flow.usingEmit: emit =>
      supervised:
        val outbound = Channel.buffered[Json](64)
        val sink = new OutboundSink[Identity]:
          def send(message: JSONRPCMessage): Unit = outbound.send(message.asJson.deepDropNullValues)
        forkDiscard:
          try handle(sink).foreach(outbound.send)
          finally outbound.done()
        outbound.foreach(json => emit(ServerSentEvent(data = Some(json.noSpaces))))
    keepAlive.fold(messages)(interval => messages.merge[ServerSentEvent](Flow.tick(interval, pingEvent), propagateDoneLeft = true))
