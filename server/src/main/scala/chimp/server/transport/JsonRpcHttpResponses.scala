package chimp.server.transport

import chimp.server.{McpResponse, OriginCheck, OutboundSink}
import io.circe.Json
import sttp.model.{Header, HeaderNames, StatusCode}
import sttp.monad.MonadError
import sttp.monad.syntax.*

/** True if the request's `Host` and `Origin` headers pass the given check. Shared by every HTTP transport, secured or not, streaming or
  * not.
  */
private[transport] def originAllowed(originCheck: OriginCheck, headers: Seq[Header]): Boolean =
  val host = headers.find(_.name.equalsIgnoreCase(HeaderNames.Host)).map(_.value)
  val origin = headers.find(_.name.equalsIgnoreCase(HeaderNames.Origin)).map(_.value)
  originCheck.validate(host, origin)

/** Runs `handle` if the origin check passes, otherwise gives a forbidden response, without running `handle`. */
private[transport] def respondToJsonRpc[F[_]](originCheck: OriginCheck, headers: Seq[Header])(handle: => F[McpResponse])(using
    m: MonadError[F]
): F[(StatusCode, Option[Json])] =
  if !originAllowed(originCheck, headers) then m.unit((StatusCode.Forbidden, None))
  else handle.map(response => (response.statusCode, response.body))

/** The same as [[respondToJsonRpc]], but the response is an event stream produced by the given [[StreamingBackend]], rather than a single
  * JSON body.
  */
private[transport] def respondWithEventStream[F[_], Caps](
    originCheck: OriginCheck,
    headers: Seq[Header],
    backend: StreamingBackend[F, Caps]
)(handle: OutboundSink[F] => F[Option[Json]])(using m: MonadError[F]): F[(StatusCode, backend.EventStream)] =
  if !originAllowed(originCheck, headers) then m.unit((StatusCode.Forbidden, backend.emptyStream))
  else backend.eventStream(handle).map(events => (StatusCode.Ok, events))
