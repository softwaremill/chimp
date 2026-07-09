package chimp.server.ox

import chimp.protocol.JSONRPCMessage
import chimp.server.OutboundSink
import chimp.server.transport.ServerStreamingHttpTransport
import io.circe.Json
import io.circe.syntax.*
import ox.*
import ox.Chunk
import ox.channels.Channel
import ox.flow.Flow
import sttp.model.sse.ServerSentEvent
import sttp.shared.Identity
import sttp.tapir.StreamBodyIO
import sttp.tapir.server.netty.sync.{serverSentEventsBody, OxStreams}

final class OxServerHttpTransport(path: List[String]) extends ServerStreamingHttpTransport[Identity, OxStreams](path):
  val streams: OxStreams = OxStreams

  type EventStream = Flow[ServerSentEvent]

  val sseBody: StreamBodyIO[Flow[Chunk[Byte]], EventStream, OxStreams] = serverSentEventsBody

  val emptyStream: EventStream = Flow.empty

  def eventStream(handle: OutboundSink[Identity] => Option[Json]): Flow[ServerSentEvent] =
    Flow.usingEmit: emit =>
      supervised:
        val outbound = Channel.buffered[Json](64)
        val sink = new OutboundSink[Identity]:
          def send(message: JSONRPCMessage): Unit = outbound.send(message.asJson.deepDropNullValues)
        forkDiscard:
          try handle(sink).foreach(outbound.send)
          finally outbound.done()
        outbound.foreach(json => emit(ServerSentEvent(data = Some(json.noSpaces))))
