package chimp.client.transport

import chimp.protocol.JSONRPCMessage
import io.circe.parser
import io.circe.syntax.*
import sttp.monad.MonadError

trait Transport[F[_]]:
  given monad: MonadError[F]
  def send(msg: JSONRPCMessage): F[Option[JSONRPCMessage]]
  def close(): F[Unit]

trait BidirectionalTransport[F[_]] extends Transport[F]:
  def onIncoming(handler: JSONRPCMessage => F[Unit]): F[Unit]

object Transport:
  private[client] def encode(msg: JSONRPCMessage): String =
    msg.asJson.deepDropNullValues.noSpaces

  private[client] def decode(s: String): Either[io.circe.Error, JSONRPCMessage] =
    parser.decode[JSONRPCMessage](s)
