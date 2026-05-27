package chimp.client.transport

import chimp.client.transport.HttpTransport.HttpOutcome
import chimp.client.{McpAuthorizationException, McpProtocolException, McpSessionNotFoundException, McpTransportException}
import chimp.protocol.{JSONRPCMessage, ProtocolVersion}
import sttp.client4.{basicRequest, Backend, Request, Response}
import sttp.model.sse.ServerSentEvent
import sttp.model.{MediaType, StatusCode, Uri}
import sttp.monad.MonadError
import sttp.monad.syntax.*

import java.util.concurrent.atomic.AtomicReference
import scala.util.chaining.*

/** Implementation of unidirectional MCP Streamable HTTP transport that exchanges JSON-RPC messages with an MCP server over HTTP.
  *
  * @param backend
  *   The sttp backend used to send HTTP requests.
  * @param uri
  *   The MCP endpoint URI.
  * @param protocolVersion
  *   Protocol version advertised via the `MCP-Protocol-Version` header; defaults to the latest version supported by chimp.
  */
final class HttpTransport[F[_]](
    backend: Backend[F],
    uri: Uri,
    protocolVersion: ProtocolVersion = ProtocolVersion.Latest
) extends Transport[F]:

  given monad: MonadError[F] = backend.monad

  private val sessionId = AtomicReference[Option[String]](None)

  override def send(msg: JSONRPCMessage): F[Option[JSONRPCMessage]] =
    HttpTransport.basePostRequest(uri, protocolVersion, sessionId.get(), Transport.encode(msg)).send(backend).flatMap(interpret)

  private def interpret(response: Response[Either[String, String]]): F[Option[JSONRPCMessage]] =
    response.header("Mcp-Session-Id").foreach(s => sessionId.set(Some(s)))
    HttpTransport.resolveResponse(response, sessionId.get()) match
      case Left(error: McpSessionNotFoundException) =>
        sessionId.set(None)
        monad.error(error)
      case Left(error)               => monad.error(error)
      case Right(HttpOutcome.NoBody) => monad.unit(None)
      case Right(kind)               =>
        response.body match
          case Left(err)   => monad.error(McpTransportException(s"HTTP 200 with empty body: $err"))
          case Right(body) =>
            val payload = kind match
              case HttpOutcome.JsonBody => if body.isEmpty then None else Some(body)
              case HttpOutcome.SseBody  => HttpTransport.extractSingleSseData(body)
              case HttpOutcome.NoBody   => None
            payload match
              case None       => monad.unit(None)
              case Some(json) =>
                Transport.decode(json) match
                  case Right(message) => monad.unit(Some(message))
                  case Left(error)    =>
                    monad.error(McpProtocolException(s"Failed to decode response body: ${error.getMessage}, payload $json"))

  override def close(): F[Unit] =
    sessionId.get() match
      case None     => monad.unit(())
      case Some(id) =>
        sessionId.set(None)
        HttpTransport.baseDeleteRequest(uri, protocolVersion, id).send(backend).map(_ => ())

object HttpTransport:
  enum HttpOutcome:
    case NoBody
    case JsonBody
    case SseBody

  private[transport] val AcceptHeader: String =
    s"${MediaType.ApplicationJson.toString}, ${MediaType.TextEventStream.toString}"

  private[transport] def basePostRequest(
      uri: Uri,
      protocolVersion: ProtocolVersion,
      sessionId: Option[String],
      body: String
  ): Request[Either[String, String]] =
    basicRequest
      .post(uri)
      .header("Content-Type", MediaType.ApplicationJson.toString)
      .header("Accept", AcceptHeader)
      .header("MCP-Protocol-Version", protocolVersion.name)
      .body(body)
      .pipe { request =>
        sessionId match
          case Some(sessionId) => request.header("Mcp-Session-Id", sessionId)
          case _               => request
      }

  private[transport] def baseDeleteRequest(
      uri: Uri,
      protocolVersion: ProtocolVersion,
      sessionId: String
  ): Request[Either[String, String]] =
    basicRequest
      .delete(uri)
      .header("Mcp-Session-Id", sessionId)
      .header("MCP-Protocol-Version", protocolVersion.name)

  private[transport] def resolveResponse(
      response: Response[?],
      currentSession: Option[String]
  ): Either[McpTransportException, HttpOutcome] =
    response.code match
      case StatusCode.Ok =>
        if isSseResponse(response) then Right(HttpOutcome.SseBody)
        else Right(HttpOutcome.JsonBody)
      case StatusCode.Accepted     => Right(HttpOutcome.NoBody)
      case StatusCode.Unauthorized => Left(McpAuthorizationException("Authorization required", StatusCode.Unauthorized.code))
      case StatusCode.Forbidden    => Left(McpAuthorizationException("Forbidden", StatusCode.Forbidden.code))
      case StatusCode.NotFound     =>
        currentSession match
          case Some(id) => Left(McpSessionNotFoundException(id))
          case None     => Left(McpTransportException("404 Not Found"))
      case other => Left(McpTransportException(s"Unexpected HTTP response: ${other.code}"))

  private def isSseResponse(response: Response[?]): Boolean =
    response
      .header("Content-Type")
      .flatMap(ct => MediaType.parse(ct).toOption)
      .exists(mt => mt.mainType == MediaType.TextEventStream.mainType && mt.subType == MediaType.TextEventStream.subType)

  private[transport] def extractSingleSseData(body: String): Option[String] =
    val blocks: List[String] = body.split("\\r?\\n\\r?\\n", -1).toList
    val events: List[ServerSentEvent] = blocks.map: block =>
      val lines: List[String] = block.split("\\r?\\n", -1).toList
      ServerSentEvent.parse(lines)
    events.flatMap(_.data).find(_.nonEmpty)
