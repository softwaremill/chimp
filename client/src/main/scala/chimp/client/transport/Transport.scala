package chimp.client.transport

import chimp.protocol.JSONRPCMessage
import sttp.monad.MonadError

trait Transport[F[_]]:
  given monad: MonadError[F]
  def send(msg: JSONRPCMessage): F[Option[JSONRPCMessage]]
  def onIncoming(handler: JSONRPCMessage => F[Unit]): F[Unit]
  def close(): F[Unit]
