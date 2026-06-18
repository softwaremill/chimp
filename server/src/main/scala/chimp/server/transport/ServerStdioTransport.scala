package chimp.server.transport

import chimp.protocol.{JSONRPCMessage, ProgressToken}
import chimp.server.*
import io.circe.parser
import io.circe.syntax.*
import org.slf4j.LoggerFactory
import sttp.monad.{IdentityMonad, MonadError}
import sttp.shared.Identity

import java.io.{BufferedReader, BufferedWriter, InputStream, InputStreamReader, OutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets

/** A synchronous implementation of MCP server using stdio transport. Exchanges line-delimited JSON-RPC messages over its standard input and
  * output.
  *
  * @param in
  *   Server input stream.
  * @param out
  *   Server output stream.
  */
final class ServerStdioTransport(in: InputStream = System.in, out: OutputStream = System.out)
    extends ServerTransport[Identity, Unit]
    with StreamingServerTransport[Identity, Unit]:

  private val log = LoggerFactory.getLogger(classOf[ServerStdioTransport])

  def serve(server: McpServer[Identity]): Unit = serve(server.streaming)

  def serve(server: StreamingMcpServer[Identity]): Unit =
    given MonadError[Identity] = IdentityMonad
    val handler = new McpHandler[Identity, StreamingServerContext[Identity]](server)
    val reader = BufferedReader(InputStreamReader(in, StandardCharsets.UTF_8))
    val writer = BufferedWriter(OutputStreamWriter(out, StandardCharsets.UTF_8))

    def writeLine(json: io.circe.Json): Unit =
      writer.synchronized:
        writer.write(json.noSpaces)
        writer.newLine()
        writer.flush()

    val sink = new OutboundSink[Identity]:
      def send(message: JSONRPCMessage): Identity[Unit] = writeLine(message.asJson.deepDropNullValues)

    val makeContext: Option[ProgressToken] => StreamingServerContext[Identity] =
      token => SinkStreamingServerContext(sink, token)

    var line = reader.readLine()
    while line != null do
      if line.nonEmpty then
        parser.parse(line) match
          case Right(json) => handler.handleJsonRpc(json, Nil, makeContext).body.foreach(writeLine)
          case Left(error) => log.warn(s"Failed to parse JSON-RPC line: ${error.getMessage}; raw: $line")
      line = reader.readLine()
