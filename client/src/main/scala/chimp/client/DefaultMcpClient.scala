package chimp.client

import chimp.client.internal.Correlator
import chimp.client.transport.Transport
import chimp.protocol.*
import io.circe.syntax.*
import io.circe.{Decoder, Json}
import sttp.monad.MonadError
import sttp.monad.syntax.*

object DefaultMcpClient:
  def create[F[_], Caps](
      transport: Transport[F],
      clientInfo: Implementation,
      protocolVersion: String,
      wireCaps: ClientCapabilities
  ): McpClient[F, Caps] = new Impl[F, Caps](transport, clientInfo, protocolVersion, wireCaps)

  private final class Impl[F[_], Caps](
      transport: Transport[F],
      clientInfo: Implementation,
      protocolVersion: String,
      wireCaps: ClientCapabilities
  ) extends McpClient[F, Caps]:
    private given MonadError[F] = transport.monad
    private val correlator = Correlator()

    override def initialize(): F[InitializeResult] =
      val params = InitializeParams(
        protocolVersion = protocolVersion,
        capabilities = wireCaps,
        clientInfo = clientInfo
      )
      sendRequest[InitializeResult]("initialize", Some(params.asJson)).flatMap: r =>
        sendNotification("notifications/initialized", None).map(_ => r)

    override def ping(): F[Unit] =
      sendRequest[Json]("ping", None).map(_ => ())

    override def close(): F[Unit] = transport.close()

    override def listTools(cursor: Option[Cursor]): F[ListToolsResponse] =
      val params = cursor.map(c => ListToolsParams(cursor = Some(c)).asJson)
      sendRequest[ListToolsResponse]("tools/list", params)

    override def callTool(name: String, arguments: Json): F[CallToolResult] =
      val params = CallToolParams(name = name, arguments = arguments).asJson
      sendRequest[CallToolResult]("tools/call", Some(params))

    private def sendRequest[R: Decoder](method: String, params: Option[Json]): F[R] =
      val req = JSONRPCMessage.Request(method = method, params = params, id = correlator.nextId())
      transport.send(req).flatMap:
        case Some(JSONRPCMessage.Response(_, _, result)) =>
          result.as[R] match
            case Right(r) => summon[MonadError[F]].unit(r)
            case Left(e)  => summon[MonadError[F]].error(McpProtocolException(s"Failed to decode $method result: ${e.getMessage}"))
        case Some(JSONRPCMessage.Error(_, _, error)) =>
          summon[MonadError[F]].error(McpProtocolException(s"$method failed: ${error.code} ${error.message}"))
        case Some(other) =>
          summon[MonadError[F]].error(McpProtocolException(s"Unexpected response to $method: $other"))
        case None =>
          summon[MonadError[F]].error(McpProtocolException(s"No response received for request $method"))

    private def sendNotification(method: String, params: Option[Json]): F[Unit] =
      val n = JSONRPCMessage.Notification(method = method, params = params)
      transport.send(n).map(_ => ())
