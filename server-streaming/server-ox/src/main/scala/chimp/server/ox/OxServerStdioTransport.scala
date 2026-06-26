package chimp.server.ox

import chimp.protocol.{JSONRPCMessage, ProgressToken}
import chimp.server.transport.ServerStreamingStdioTransport
import chimp.server.{McpHandler, OutboundSink, SinkStreamingServerContext, StreamingMcpServer, StreamingServerContext}
import io.circe.syntax.*
import io.circe.{parser, Json}
import org.slf4j.LoggerFactory
import ox.*
import ox.channels.Channel
import sttp.monad.{IdentityMonad, MonadError}
import sttp.shared.Identity

import java.io.{BufferedReader, BufferedWriter, InputStream, InputStreamReader, OutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets

final class OxServerStdioTransport(in: InputStream = System.in, out: OutputStream = System.out)
    extends ServerStreamingStdioTransport[Identity]:
  private val log = LoggerFactory.getLogger(classOf[OxServerStdioTransport])

  def serve(server: StreamingMcpServer[Identity]): Unit =
    given MonadError[Identity] = IdentityMonad
    val handler = new McpHandler[Identity, StreamingServerContext[Identity]](server)
    val reader = BufferedReader(InputStreamReader(in, StandardCharsets.UTF_8))
    val writer = BufferedWriter(OutputStreamWriter(out, StandardCharsets.UTF_8))

    def writeLine(json: Json): Unit =
      writer.write(json.noSpaces)
      writer.newLine()
      writer.flush()

    supervised:
      val outbound = Channel.buffered[Json](64)
      val writerFork = fork(outbound.foreach(writeLine))

      val sink = new OutboundSink[Identity]:
        def send(message: JSONRPCMessage): Unit = outbound.send(message.asJson.deepDropNullValues)
      val makeContext: Option[ProgressToken] => StreamingServerContext[Identity] =
        token => SinkStreamingServerContext(sink, token)

      var line = reader.readLine()
      while line != null do
        if line.nonEmpty then
          parser.parse(line) match
            case Right(json) => handler.handleJsonRpc(json, Nil, makeContext).body.foreach(outbound.send)
            case Left(error) => log.warn(s"Failed to parse JSON-RPC line: ${error.getMessage}; raw: $line")
        line = reader.readLine()

      outbound.done()
      writerFork.join()
