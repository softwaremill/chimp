package chimp.server.ox

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
import chimp.transport.{StdioFraming, StdioLineReader}
import io.circe.syntax.*
import io.circe.{parser, Json}
import org.slf4j.LoggerFactory
import ox.*
import ox.channels.Channel
import sttp.monad.{IdentityMonad, MonadError}
import sttp.shared.Identity

import java.io.{BufferedWriter, InputStream, OutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets

final class OxServerStdioTransport(
    in: InputStream = System.in,
    out: OutputStream = System.out,
    maxLineLength: Int = StdioFraming.defaultMaxLineLength
) extends ServerStreamingStdioTransport[Identity]:
  private val log = LoggerFactory.getLogger(classOf[OxServerStdioTransport])

  def serve(server: StreamingMcpServer[Identity]): Unit =
    given MonadError[Identity] = IdentityMonad
    val handler = new McpHandler[Identity, StreamingServerContext[Identity]](server, canListen = true)
    val reader = StdioLineReader(in, maxLineLength)
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
      val makeContext: RequestInfo => StreamingServerContext[Identity] =
        info => SinkStreamingServerContext(sink, info)

      val subscriptions = StdioSubscriptions(handler, json => { val _ = outbound.sendOrClosed(json) })

      try
        reader.lines
          .filter(_.nonEmpty)
          .foreach: line =>
            parser.parse(line) match
              case Right(json) =>
                if !subscriptions.accept(json) then handler.handleJsonRpc(json, Nil, makeContext).body.foreach(outbound.send)
              case Left(error) => log.warn(s"Failed to parse JSON-RPC line: ${error.getMessage}; raw: $line")
      finally subscriptions.endAll()

      outbound.done()
      writerFork.join()
