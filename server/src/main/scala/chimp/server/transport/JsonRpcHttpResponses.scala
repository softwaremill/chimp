package chimp.server.transport

import chimp.server.{McpResponse, OriginCheck, OutboundSink}
import io.circe.Json
import sttp.model.{Header, HeaderNames, StatusCode}
import sttp.monad.MonadError
import sttp.monad.syntax.*

private[transport] def originAllowed(originCheck: OriginCheck, headers: Seq[Header]): Boolean =
  val host = headers.find(_.name.equalsIgnoreCase(HeaderNames.Host)).map(_.value)
  val origin = headers.find(_.name.equalsIgnoreCase(HeaderNames.Origin)).map(_.value)
  originCheck.validate(host, origin)

private[transport] def respondToJsonRpc[F[_]](originCheck: OriginCheck, headers: Seq[Header])(handle: => F[McpResponse])(using
    m: MonadError[F]
): F[(StatusCode, Option[Json])] =
  if !originAllowed(originCheck, headers) then m.unit((StatusCode.Forbidden, None))
  else handle.map(response => (response.statusCode, response.body))

private[transport] def respondWithEventStream[F[_], Caps](
    originCheck: OriginCheck,
    headers: Seq[Header],
    backend: ServerStreamingHttpTransport[F, Caps]
)(handle: OutboundSink[F] => F[Option[Json]])(using m: MonadError[F]): F[(StatusCode, backend.EventStream)] =
  if !originAllowed(originCheck, headers) then m.unit((StatusCode.Forbidden, backend.emptyStream))
  else backend.eventStream(handle).map(events => (StatusCode.Ok, events))
