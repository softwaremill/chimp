package chimp.server

import chimp.protocol.JSONRPCMessage

/** A write-end for server→client messages emitted while a request is being handled on a streaming endpoint (notifications, and — later —
  * server-initiated requests). The concrete realization is provided by a per-effect [[McpStreaming]] implementation.
  */
trait OutboundSink[F[_]]:
  def send(message: JSONRPCMessage): F[Unit]
