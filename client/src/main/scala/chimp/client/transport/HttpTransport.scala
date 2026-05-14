package chimp.client.transport

import chimp.client.{McpAuthorizationException, McpProtocolException, McpSessionNotFoundException, McpTransportException}
import chimp.protocol.{JSONRPCMessage, ProtocolVersion}
import io.circe.parser
import io.circe.syntax.*
import sttp.client4.{basicRequest, Backend, Response}
import sttp.model.sse.ServerSentEvent
import sttp.model.{MediaType, StatusCode, Uri}
import sttp.monad.MonadError
import sttp.monad.syntax.*

import java.util.concurrent.atomic.AtomicReference

final class HttpTransport[F[_]](
    backend: Backend[F],
    uri: Uri,
    protocolVersion: ProtocolVersion = ProtocolVersion.Latest
) extends Transport[F]:

  given monad: MonadError[F] = backend.monad

  private val sessionId = AtomicReference[Option[String]](None)

  override def send(msg: JSONRPCMessage): F[Option[JSONRPCMessage]] =
    val body = msg.asJson.deepDropNullValues.noSpaces
    var request = basicRequest
      .post(uri)
      .header("Content-Type", "application/json")
      .header("Accept", s"${MediaType.ApplicationJson.toString}, ${MediaType.TextEventStream.toString()}}")
      .header("MCP-Protocol-Version", protocolVersion.name)
      .body(body)
    sessionId.get().foreach(s => request = request.header("Mcp-Session-Id", s))

    request.send(backend).flatMap(interpret)

  private def interpret(response: Response[Either[String, String]]): F[Option[JSONRPCMessage]] =
    response.header("Mcp-Session-Id").foreach(s => sessionId.set(Some(s)))
    response.code match
      case StatusCode.Ok =>
        response.body match
          case Right(body) =>
            val contentType = response.header("Content-Type").getOrElse("")
            val payload =
              if contentType.contains(MediaType.TextEventStream.toString()) then HttpTransport.extractSingleSseData(body)
              else if body.isEmpty then None
              else Some(body)
            payload match
              case None       => monad.unit(None)
              case Some(json) =>
                parser.decode[JSONRPCMessage](json) match
                  case Right(message) => monad.unit(Some(message))
                  case Left(error)    =>
                    monad.error(McpProtocolException(s"Failed to decode response body: ${error.getMessage}, payload $json"))
          case Left(err) =>
            monad.error(McpTransportException(s"HTTP 200 with empty body: $err"))
      case StatusCode.Accepted =>
        monad.unit(None)
      case StatusCode.Unauthorized =>
        monad.error(McpAuthorizationException(s"Authorization required", response.code.code))
      case StatusCode.Forbidden =>
        monad.error(McpAuthorizationException(s"Forbidden", response.code.code))
      case StatusCode.NotFound if sessionId.get().isDefined =>
        val id = sessionId.get().get
        sessionId.set(None)
        monad.error(McpSessionNotFoundException(id))
      case other =>
        monad.error(McpTransportException(s"Unexpected HTTP response: ${other.code} ${response.body.fold(identity, identity)}"))

  override def onIncoming(handler: JSONRPCMessage => F[Unit]): F[Unit] = monad.unit(())

  override def close(): F[Unit] =
    sessionId.get() match
      case None     => monad.unit(())
      case Some(id) =>
        val req = basicRequest
          .delete(uri)
          .header("Mcp-Session-Id", id)
          .header("MCP-Protocol-Version", protocolVersion.name)
        sessionId.set(None)
        req.send(backend).map(_ => ())

object HttpTransport:
  private[transport] def extractSingleSseData(body: String): Option[String] =
    val blocks: List[String] = body.split("\\r?\\n\\r?\\n", -1).toList
    val events: List[sttp.model.sse.ServerSentEvent] = blocks.map: block =>
      val lines: List[String] = block.split("\\r?\\n", -1).toList
      ServerSentEvent.parse(lines)
    events.flatMap(_.data).find(_.nonEmpty)
