package chimp.server

import chimp.protocol.JSONRPCMessage

/** The sink for any server to client interaction. Each streaming transport supplies its own implementation.
  */
trait OutboundSink[F[_]]:
  def send(message: JSONRPCMessage): F[Unit]
