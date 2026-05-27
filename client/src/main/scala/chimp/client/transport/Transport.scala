package chimp.client.transport

import chimp.protocol.JSONRPCMessage
import io.circe.parser
import io.circe.syntax.*
import sttp.monad.MonadError

/** A unidirectional MCP transport: the client sends a [[chimp.protocol.JSONRPCMessage]] and optionally receives a response back. For
  * transports that doesn't handle server initiated requests.
  */
trait Transport[F[_]]:
  given monad: MonadError[F]
  def send(msg: JSONRPCMessage): F[Option[JSONRPCMessage]]
  def close(): F[Unit]

/** A bidirectional MCP transport that, in addition to [[Transport.send]] calls, allows the server to push messages to the client. Incoming
  * messages are delivered to the registered handler via [[onIncoming]]. Used by [[chimp.client.BidirectionalMcpClient]].
  */
trait BidirectionalTransport[F[_]] extends Transport[F]:
  def onIncoming(handler: JSONRPCMessage => F[Unit]): F[Unit]

object Transport:
  private[client] def encode(msg: JSONRPCMessage): String =
    msg.asJson.deepDropNullValues.noSpaces

  private[client] def decode(s: String): Either[io.circe.Error, JSONRPCMessage] =
    parser.decode[JSONRPCMessage](s)
