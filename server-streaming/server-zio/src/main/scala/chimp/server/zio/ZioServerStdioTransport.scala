package chimp.server.zio

import chimp.protocol.{JSONRPCMessage, ProgressToken}
import chimp.server.{McpHandler, OutboundSink, SinkStreamingServerContext, StreamingMcpServer, StreamingServerContext}
import chimp.server.transport.ServerStreamingStdioTransport
import io.circe.syntax.*
import io.circe.{parser, Json}
import org.slf4j.LoggerFactory
import sttp.monad.MonadError
import sttp.tapir.ztapir.RIOMonadError
import zio.{Task, ZIO}

import java.io.{BufferedReader, BufferedWriter, InputStream, InputStreamReader, OutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets

final class ZioServerStdioTransport(in: InputStream = System.in, out: OutputStream = System.out)
    extends ServerStreamingStdioTransport[Task]:
  private val log = LoggerFactory.getLogger(classOf[ZioServerStdioTransport])
  private given MonadError[Task] = new RIOMonadError[Any]

  def serve(server: StreamingMcpServer[Task]): Task[Unit] =
    val handler = new McpHandler[Task, StreamingServerContext[Task]](server)
    val reader = BufferedReader(InputStreamReader(in, StandardCharsets.UTF_8))
    val writer = BufferedWriter(OutputStreamWriter(out, StandardCharsets.UTF_8))

    def writeLine(json: Json): Task[Unit] =
      ZIO.attemptBlocking:
        writer.write(json.noSpaces)
        writer.newLine()
        writer.flush()

    val sink = new OutboundSink[Task]:
      def send(message: JSONRPCMessage): Task[Unit] = writeLine(message.asJson.deepDropNullValues)

    val makeContext: Option[ProgressToken] => StreamingServerContext[Task] =
      token => SinkStreamingServerContext(sink, token)

    def loop: Task[Unit] =
      ZIO
        .attemptBlocking(Option(reader.readLine()))
        .flatMap:
          case None       => ZIO.unit
          case Some(line) =>
            val handled =
              if line.isEmpty then ZIO.unit
              else
                parser.parse(line) match
                  case Right(json) =>
                    handler.handleJsonRpc(json, Nil, makeContext).flatMap(response => ZIO.foreachDiscard(response.body)(writeLine))
                  case Left(error) =>
                    ZIO.succeed(log.warn(s"Failed to parse JSON-RPC line: ${error.getMessage}; raw: $line"))
            handled *> loop

    loop
