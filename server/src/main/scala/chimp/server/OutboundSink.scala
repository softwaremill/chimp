package chimp.server

import chimp.protocol.JSONRPCMessage

/** The seam between a [[StreamingServerContext]] and the wire. Server→client messages — notifications (progress, logging) and, later,
  * requests (sampling, elicitation) — are emitted by calling [[send]]. Each streaming transport supplies its own implementation.
  */
trait OutboundSink[F[_]]:
  def send(message: JSONRPCMessage): F[Unit]
