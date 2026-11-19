package chimp.server.zio

import chimp.protocol.JSONRPCMessage
import chimp.server.{
  McpHandler,
  OutboundSink,
  RequestInfo,
  SinkStreamingServerContext,
  StdioSubscriptions,
  StreamingMcpServer,
  StreamingServerContext
}
import chimp.server.transport.ServerStreamingStdioTransport
import chimp.transport.{StdioFraming, StdioLineReader}
import io.circe.syntax.*
import io.circe.{parser, Json}
import org.slf4j.LoggerFactory
import sttp.monad.MonadError
import sttp.tapir.ztapir.RIOMonadError
import zio.{Task, ZIO}

import java.io.{BufferedWriter, InputStream, OutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets

final class ZioServerStdioTransport(
    in: InputStream = System.in,
    out: OutputStream = System.out,
    maxLineLength: Int = StdioFraming.defaultMaxLineLength
) extends ServerStreamingStdioTransport[Task]:
  private val log = LoggerFactory.getLogger(classOf[ZioServerStdioTransport])
  private given MonadError[Task] = new RIOMonadError[Any]

  def serve(server: StreamingMcpServer[Task]): Task[Unit] =
    val handler = new McpHandler[Task, StreamingServerContext[Task]](server, canListen = true)
    val reader = StdioLineReader(in, maxLineLength)
    val writer = BufferedWriter(OutputStreamWriter(out, StandardCharsets.UTF_8))

    def write(json: Json): Unit =
      writer.synchronized:
        writer.write(json.noSpaces)
        writer.newLine()
        writer.flush()

    def writeLine(json: Json): Task[Unit] = ZIO.attemptBlocking(write(json))

    val subscriptions = StdioSubscriptions(handler, write)

    val sink = new OutboundSink[Task]:
      def send(message: JSONRPCMessage): Task[Unit] = writeLine(message.asJson.deepDropNullValues)

    val makeContext: RequestInfo => StreamingServerContext[Task] =
      info => SinkStreamingServerContext(sink, info)

    def loop: Task[Unit] =
      ZIO
        .attemptBlocking(reader.readLine())
        .flatMap:
          case None       => ZIO.unit
          case Some(line) =>
            val handled =
              if line.isEmpty then ZIO.unit
              else
                parser.parse(line) match
                  case Right(json) =>
                    ZIO
                      .attemptBlocking(subscriptions.accept(json))
                      .flatMap: accepted =>
                        if accepted then ZIO.unit
                        else handler.handleJsonRpc(json, Nil, makeContext).flatMap(response => ZIO.foreachDiscard(response.body)(writeLine))
                  case Left(error) =>
                    ZIO.succeed(log.warn(s"Failed to parse JSON-RPC line: ${error.getMessage}; raw: $line"))
            handled *> loop

    loop.ensuring(ZIO.attemptBlocking(subscriptions.endAll()).ignore)
