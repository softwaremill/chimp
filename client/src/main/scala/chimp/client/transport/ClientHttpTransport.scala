package chimp.client.transport

import chimp.client.transport.ClientHttpTransport.HttpOutcome
import chimp.client.{McpAuthorizationException, McpProtocolException, McpSessionNotFoundException, McpTransportException}
import chimp.protocol.{JSONRPCErrorObject, JSONRPCMessage, ProtocolMeta, ProtocolVersion}
import sttp.client4.{basicRequest, Backend, Request, Response}
import sttp.model.sse.ServerSentEvent
import sttp.model.{Header, MediaType, StatusCode, Uri}
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
  * @param headers
  *   Extra headers sent with each request to the server.
  *
  * At a stateless protocol version, a `tools/list` result leaves out the tools whose `x-mcp-header` annotations are invalid, and a
  * `tools/call` mirrors the annotated arguments of a listed tool into `Mcp-Param-*` headers.
  */
final class ClientHttpTransport[F[_]](
    backend: Backend[F],
    uri: Uri,
    headers: Seq[Header] = Nil
) extends ClientTransport[F]:

  given monad: MonadError[F] = backend.monad

  private val sessionId = AtomicReference[Option[String]](None)
  private val protocolVersion = AtomicReference(ClientHttpTransport.DefaultProtocolVersion)
  private val toolHeaders = ToolHeaders()

  override def setProtocolVersion(version: ProtocolVersion): F[Unit] = monad.eval(protocolVersion.set(version))

  override def send(msg: JSONRPCMessage): F[Option[JSONRPCMessage]] =
    ClientHttpTransport
      .basePostRequest(uri, protocolVersion.get(), sessionId.get(), msg, headers, toolHeaders)
      .send(backend)
      .flatMap(interpret(msg, _))
      .map(toolHeaders.filterToolsList(msg, _))

  private def interpret(msg: JSONRPCMessage, response: Response[Either[String, String]]): F[Option[JSONRPCMessage]] =
    response.header("Mcp-Session-Id").foreach(s => sessionId.set(Some(s)))
    ClientHttpTransport.resolveResponse(response, sessionId.get()) match
      case Left(error: McpSessionNotFoundException) =>
        sessionId.set(None)
        monad.error(error)
      case Left(error)                          => monad.error(error)
      case Right(HttpOutcome.NoBody)            => monad.unit(None)
      case Right(HttpOutcome.ErrorBody(status)) =>
        ClientHttpTransport.errorResponse(msg, status, response.body.fold(identity, identity)) match
          case Right(error) => monad.unit(Some(error))
          case Left(error)  => monad.error(error)
      case Right(kind) =>
        response.body match
          case Left(err)   => monad.error(McpTransportException(s"HTTP 200 with empty body: $err"))
          case Right(body) =>
            val payload = kind match
              case HttpOutcome.JsonBody => if body.isEmpty then None else Some(body)
              case HttpOutcome.SseBody  => ClientHttpTransport.extractSingleSseData(body)
              case _                    => None
            payload match
              case None       => monad.unit(None)
              case Some(json) =>
                ClientTransport.decode(json) match
                  case Right(message) => monad.unit(Some(message))
                  case Left(error)    =>
                    monad.error(McpProtocolException(s"Failed to decode response body: ${error.getMessage}, payload $json"))

  override def close(): F[Unit] =
    sessionId.get() match
      case None     => monad.unit(())
      case Some(id) =>
        sessionId.set(None)
        ClientHttpTransport.baseDeleteRequest(uri, protocolVersion.get(), id, headers).send(backend).map(_ => ())

object ClientHttpTransport:
  enum HttpOutcome:
    case NoBody
    case JsonBody
    case SseBody
    case ErrorBody(status: StatusCode)

  /** Whether the response to `msg` lets a streaming transport open its GET stream. The response to `server/discover` does not: with era
    * detection the client sends it before it knows the era, and before a legacy server gives a session.
    */
  private[transport] def readiesGetStream(msg: JSONRPCMessage): Boolean = msg match
    case JSONRPCMessage.Request(_, "server/discover", _, _) => false
    case _                                                  => true

  /** The version an HTTP transport sends until the client sets one. */
  private[transport] val DefaultProtocolVersion: ProtocolVersion = ProtocolVersion.LatestLegacy

  private[transport] val AcceptHeader: String =
    s"${MediaType.ApplicationJson.toString}, ${MediaType.TextEventStream.toString}"

  private[transport] def basePostRequest(
      uri: Uri,
      protocolVersion: ProtocolVersion,
      sessionId: Option[String],
      msg: JSONRPCMessage,
      headers: Seq[Header],
      toolHeaders: ToolHeaders
  ): Request[Either[String, String]] =
    val version = McpRequestHeaders.metaVersion(msg).getOrElse(protocolVersion.name)
    val stateless = ProtocolVersion.from(version).exists(_.isModern)
    basicRequest
      .post(uri)
      .headers(headers*)
      .header("Content-Type", MediaType.ApplicationJson.toString)
      .header("Accept", AcceptHeader)
      .header(ProtocolMeta.ProtocolVersionHeader, version)
      .headers((if stateless then McpRequestHeaders.standard(msg) ++ toolHeaders.paramHeaders(msg) else Nil)*)
      .body(ClientTransport.encode(msg))
      .pipe { request =>
        sessionId match
          case Some(sessionId) => request.header("Mcp-Session-Id", sessionId)
          case _               => request
      }

  private[transport] def baseGetRequest(
      uri: Uri,
      protocolVersion: ProtocolVersion,
      sessionId: Option[String],
      lastEventId: Option[String],
      headers: Seq[Header]
  ): Request[Either[String, String]] =
    basicRequest
      .get(uri)
      .headers(headers*)
      .header("Accept", MediaType.TextEventStream.toString)
      .header("MCP-Protocol-Version", protocolVersion.name)
      .pipe(request => sessionId.fold(request)(id => request.header("Mcp-Session-Id", id)))
      .pipe(request => lastEventId.fold(request)(id => request.header("Last-Event-ID", id)))

  private[transport] def baseDeleteRequest(
      uri: Uri,
      protocolVersion: ProtocolVersion,
      sessionId: String,
      headers: Seq[Header]
  ): Request[Either[String, String]] =
    basicRequest
      .delete(uri)
      .headers(headers*)
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
      case StatusCode.NotFound if currentSession.isDefined => Left(McpSessionNotFoundException(currentSession.getOrElse("")))
      case other                                           => Right(HttpOutcome.ErrorBody(other))

  /** A request that fails with an HTTP error can carry a JSON-RPC error in the body, e.g. `-32022` with HTTP 400. That error is the
    * response to the request; any other body fails with [[McpTransportException]].
    */
  private[transport] def errorResponse(
      msg: JSONRPCMessage,
      status: StatusCode,
      body: String
  ): Either[McpTransportException, JSONRPCMessage.Error] =
    val error: Option[JSONRPCMessage.Error] = msg match
      case request: JSONRPCMessage.Request =>
        io.circe.parser
          .parse(body)
          .flatMap(_.hcursor.downField("error").as[JSONRPCErrorObject])
          .toOption
          .map(error => new JSONRPCMessage.Error(id = request.id, error = error))
      case _ => None
    error.toRight(unexpectedStatus(status))

  private def unexpectedStatus(status: StatusCode): McpTransportException =
    if status == StatusCode.NotFound then McpTransportException("404 Not Found")
    else McpTransportException(s"Unexpected HTTP response: ${status.code}")

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
