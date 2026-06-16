package chimp.server

import io.circe.Json
import sttp.capabilities.Streams
import sttp.tapir.StreamBodyIO

/** The per-effect primitive needed to serve MCP over Server-Sent Events. A concrete implementation supplies the Tapir SSE response body and
  * a way to turn a request handler — which may emit server→client messages through an [[OutboundSink]] while running — into the effect's
  * native stream of [[ServerSentEvent]]s. Concrete implementations live in effect-specific modules (e.g. `chimp-server-zio`).
  *
  * @tparam F
  *   the effect type
  * @tparam S
  *   the Tapir/sttp streaming capability (e.g. `ZioStreams`)
  */
abstract class McpStreaming[F[_], S]:
  val streams: Streams[S]

  /** The effect's native stream of Server-Sent Events (e.g. `zio.stream.Stream[Throwable, ServerSentEvent]`). */
  type EventStream

  /** The Tapir output body describing an SSE (`text/event-stream`) response carrying [[EventStream]]. */
  def sseBody: StreamBodyIO[streams.BinaryStream, EventStream, S]

  /** An empty event stream, used for non-2xx responses (e.g. a rejected request). */
  def emptyEvents: EventStream

  /** Run `handle` — which receives an [[OutboundSink]] to push notifications onto while it works and returns the final JSON-RPC response
    * body (or `None` for a notification) — and produce the SSE event stream: each pushed message becomes an event, followed by the final
    * response event, after which the stream completes.
    */
  def eventStream(handle: OutboundSink[F] => F[Option[Json]]): F[EventStream]
