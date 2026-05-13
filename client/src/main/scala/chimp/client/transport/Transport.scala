package chimp.client.transport

import chimp.protocol.JSONRPCMessage
import sttp.monad.MonadError

/** A bidirectional transport carrying JSON-RPC messages for an MCP client.
  *
  *   - `send(req)` for a `Request` returns the matching `Response` or `Error` from the peer.
  *   - `send(notif)` for a `Notification` returns `None` once the message has been delivered.
  *   - `onIncoming(handler)` registers a callback for server-initiated requests and notifications. HTTP non-streaming transports never
  *     invoke it; stdio and streaming transports do.
  */
trait Transport[F[_]]:
  given monad: MonadError[F]
  def send(msg: JSONRPCMessage): F[Option[JSONRPCMessage]]
  def onIncoming(handler: JSONRPCMessage => F[Unit]): F[Unit]
  def close(): F[Unit]
