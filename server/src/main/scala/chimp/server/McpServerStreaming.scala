package chimp.server

import chimp.protocol.JSONRPCMessage
import io.circe.Json
import sttp.capabilities.Streams
import sttp.tapir.StreamBodyIO

abstract class McpServerStreaming[F[_], S]:
  val streams: Streams[S]
  type EventStream
  def sseBody: StreamBodyIO[streams.BinaryStream, EventStream, S]
  def emptyEvents: EventStream
  def eventStream(handle: OutboundSink[F] => F[Option[Json]]): F[EventStream]

trait OutboundSink[F[_]]:
  def send(message: JSONRPCMessage): F[Unit]
