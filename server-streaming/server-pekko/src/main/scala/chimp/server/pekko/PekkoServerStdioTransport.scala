package chimp.server.pekko

import chimp.protocol.JSONRPCMessage
import chimp.server.transport.ServerStreamingStdioTransport
import chimp.server.{
  McpHandler,
  OutboundSink,
  RequestInfo,
  SinkStreamingServerContext,
  StdioSubscriptions,
  StreamingMcpServer,
  StreamingServerContext
}
import chimp.transport.{McpLineTooLongException, StdioFraming}
import io.circe.syntax.*
import io.circe.{parser, Json}
import org.apache.pekko.stream.scaladsl.{Framing, Keep, Sink, Source, SourceQueueWithComplete, StreamConverters}
import org.apache.pekko.stream.{Materializer, OverflowStrategy}
import org.apache.pekko.util.ByteString
import org.slf4j.LoggerFactory
import sttp.monad.{FutureMonad, MonadError}

import java.io.{InputStream, OutputStream}
import scala.concurrent.{ExecutionContext, Future}

final class PekkoServerStdioTransport(
    in: InputStream = System.in,
    out: OutputStream = System.out,
    maxLineLength: Int = StdioFraming.defaultMaxLineLength
)(using mat: Materializer)
    extends ServerStreamingStdioTransport[Future]:

  private val log = LoggerFactory.getLogger(classOf[PekkoServerStdioTransport])

  private given ExecutionContext = mat.executionContext
  private given MonadError[Future] = FutureMonad()

  def serve(server: StreamingMcpServer[Future]): Future[Unit] =
    val handler = new McpHandler[Future, StreamingServerContext[Future]](server, canListen = true)

    val (outbound, writing) = Source
      .queue[Json](PekkoOutbound.defaultBufferSize, OverflowStrategy.backpressure, PekkoOutbound.defaultMaxConcurrentSends)
      .map(json => ByteString(json.noSpaces + "\n"))
      .toMat(StreamConverters.fromOutputStream(() => out, autoFlush = true))(Keep.both)
      .run()

    val sink = new OutboundSink[Future]:
      def send(message: JSONRPCMessage): Future[Unit] = PekkoOutbound.offer(outbound, message.asJson.deepDropNullValues)

    val makeContext: RequestInfo => StreamingServerContext[Future] =
      info => SinkStreamingServerContext(sink, info)

    val subscriptions = StdioSubscriptions(handler, json => { val _ = PekkoOutbound.offer(outbound, json) })

    val reading = StreamConverters
      .fromInputStream(() => in)
      .via(Framing.delimiter(ByteString("\n"), maximumFrameLength = maxLineLength, allowTruncation = true))
      .mapError { case _: Framing.FramingException => McpLineTooLongException(maxLineLength) }
      .map(_.utf8String.trim)
      .filter(_.nonEmpty)
      .mapAsync(1)(line => handleLine(handler, subscriptions, makeContext, outbound, line))
      .runWith(Sink.ignore)

    reading.transformWith: result =>
      subscriptions.endAll()
      outbound.complete()
      writing.transform(_ => result.map(_ => ()))

  private def handleLine(
      handler: McpHandler[Future, StreamingServerContext[Future]],
      subscriptions: StdioSubscriptions,
      makeContext: RequestInfo => StreamingServerContext[Future],
      outbound: SourceQueueWithComplete[Json],
      line: String
  ): Future[Unit] =
    parser.parse(line) match
      case Right(json) =>
        if subscriptions.accept(json) then Future.unit
        else
          handler
            .handleJsonRpc(json, Nil, makeContext)
            .flatMap(response => response.body.fold(Future.unit)(body => PekkoOutbound.offer(outbound, body)))
      case Left(error) =>
        log.warn(s"Failed to parse JSON-RPC line: ${error.getMessage}; raw: $line")
        Future.unit
