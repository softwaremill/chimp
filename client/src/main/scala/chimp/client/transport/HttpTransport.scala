package chimp.client.transport

import chimp.client.{McpAuthorizationException, McpProtocolException, McpSessionNotFoundException, McpTransportException}
import chimp.protocol.{JSONRPCMessage, ProtocolVersion}
import io.circe.parser
import io.circe.syntax.*
import sttp.client4.{Backend, Response, basicRequest}
import sttp.model.{MediaType, StatusCode, Uri}
import sttp.monad.MonadError
import sttp.monad.syntax.*

import java.util.concurrent.atomic.AtomicReference

/** Non-streaming Streamable HTTP transport. Works against any sttp `Backend[F]`. */
final class HttpTransport[F[_]](
    backend: Backend[F],
    uri: Uri,
    protocolVersion: String = ProtocolVersion.Latest
) extends Transport[F]:

  given monad: MonadError[F] = backend.monad

  private val sessionId = AtomicReference[Option[String]](None)

  override def send(msg: JSONRPCMessage): F[Option[JSONRPCMessage]] =
    val body = msg.asJson.deepDropNullValues.noSpaces
    var req = basicRequest
      .post(uri)
      .header("Content-Type", "application/json")
      .header("Accept", s"${MediaType.ApplicationJson.toString}, text/event-stream")
      .header("MCP-Protocol-Version", protocolVersion)
      .body(body)
    sessionId.get().foreach(s => req = req.header("Mcp-Session-Id", s))

    req.send(backend).flatMap(interpret)

  private def interpret(response: Response[Either[String, String]]): F[Option[JSONRPCMessage]] =
    response.header("Mcp-Session-Id").foreach(s => sessionId.set(Some(s)))
    response.code match
      case StatusCode.Ok =>
        response.body match
          case Right(bodyStr) =>
            parser.decode[JSONRPCMessage](bodyStr) match
              case Right(m) => monad.unit(Some(m))
              case Left(e)  => monad.error(McpProtocolException(s"Failed to decode response body: ${e.getMessage}"))
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
      case None => monad.unit(())
      case Some(id) =>
        val req = basicRequest
          .delete(uri)
          .header("Mcp-Session-Id", id)
          .header("MCP-Protocol-Version", protocolVersion)
        sessionId.set(None)
        req.send(backend).map(_ => ())
