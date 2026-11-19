package chimp.server.ox

import chimp.protocol.JSONRPCMessage
import chimp.server.{ListenRequest, McpResponse, OutboundSink, SubscriptionOutput}
import chimp.server.transport.{plainJsonError, ServerStreamingHttpTransport}
import io.circe.Json
import io.circe.syntax.*
import ox.*
import org.slf4j.LoggerFactory
import ox.channels.{Channel, ChannelClosed}
import ox.flow.Flow
import sttp.model.StatusCode
import sttp.model.sse.ServerSentEvent
import sttp.shared.Identity
import sttp.tapir.StreamBodyIO
import sttp.tapir.server.netty.sync.{serverSentEventsBody, OxStreams}

import scala.util.control.NonFatal

final class OxServerHttpTransport(path: List[String]) extends ServerStreamingHttpTransport[Identity, OxStreams](path):
  private val log = LoggerFactory.getLogger(classOf[OxServerHttpTransport])

  val streams: OxStreams = OxStreams

  type EventStream = Flow[ServerSentEvent]

  val sseBody: StreamBodyIO[streams.BinaryStream, EventStream, OxStreams] = serverSentEventsBody

  val emptyStream: EventStream = Flow.empty

  def eventStream(handle: OutboundSink[Identity] => McpResponse): Either[(StatusCode, Json), Flow[ServerSentEvent]] =
    val outbound = Channel.buffered[Outbound](64)
    val sink = new OutboundSink[Identity]:
      def send(message: JSONRPCMessage): Unit = outbound.sendOrClosed(Outbound.Message(message.asJson.deepDropNullValues)).discard
    val worker = oxThreadFactory.newThread: () =>
      try outbound.sendOrClosed(Outbound.Final(handle(sink))).discard
      catch case NonFatal(e) => log.warn("Failed to handle the JSON-RPC message", e)
      finally outbound.doneOrClosed().discard
    worker.start()

    def stream(first: Outbound | ChannelClosed): Flow[ServerSentEvent] =
      Flow.usingEmit: emit =>
        def emitBody(item: Outbound | ChannelClosed): Unit = item match
          case Outbound.Message(json)   => emit(ServerSentEvent(data = Some(json.noSpaces)))
          case Outbound.Final(response) => response.body.foreach(json => emit(ServerSentEvent(data = Some(json.noSpaces))))
          case _: ChannelClosed         => ()
        try
          emitBody(first)
          outbound.foreach(emitBody)
        finally
          outbound.doneOrClosed().discard
          worker.interrupt()

    outbound.receiveOrClosed() match
      case first @ Outbound.Final(response) => plainJsonError(response).toLeft(stream(first))
      case first                            => Right(stream(first))

  private enum Outbound:
    case Message(json: Json)
    case Final(response: McpResponse)

  private[server] def subscriptionStream(listen: ListenRequest): Flow[ServerSentEvent] =
    Flow.usingEmit: emit =>
      val outbound = Channel.unlimited[Json]
      val subscription = listen.open(new SubscriptionOutput:
        def send(message: Json): Unit =
          val _ = outbound.sendOrClosed(message)
        def close(): Unit =
          val _ = outbound.doneOrClosed())
      try outbound.foreach(json => emit(ServerSentEvent(data = Some(json.noSpaces))))
      finally subscription.cancel()
