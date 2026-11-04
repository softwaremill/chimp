package chimp.server.transport

import chimp.server.{McpResponse, OriginCheck, OutboundSink}
import io.circe.Json
import sttp.model.{Header, HeaderNames, StatusCode}
import sttp.monad.MonadError
import sttp.monad.syntax.*
import sttp.tapir.*
import sttp.tapir.json.circe.*

private[transport] def originAllowed(originCheck: OriginCheck, headers: Seq[Header]): Boolean =
  val host = headers.find(_.name.equalsIgnoreCase(HeaderNames.Host)).map(_.value)
  val origin = headers.find(_.name.equalsIgnoreCase(HeaderNames.Origin)).map(_.value)
  originCheck.validate(host, origin)

private[transport] def respondToJsonRpc[F[_]](originCheck: OriginCheck, headers: Seq[Header])(handle: => F[McpResponse])(using
    m: MonadError[F]
): F[(StatusCode, Option[Json])] =
  if !originAllowed(originCheck, headers) then m.unit((StatusCode.Forbidden, None))
  else handle.map(response => (response.statusCode, response.body))

/** A request rejected before any stream opens: an HTTP status with a plain `application/json` JSON-RPC error body. */
private[transport] type JsonRejection = (StatusCode, Json)

/** The tapir output of a [[JsonRejection]]. */
private[transport] val jsonRejectionOutput: EndpointOutput[JsonRejection] = statusCode.and(jsonBody[Json])

/** Checks the origin, then validates the request with `rejection` (runs no tool logic). A rejection - for example a modern validation error -
  * is returned on the left, to be sent as a plain JSON error with its own HTTP status; otherwise `handle` runs inside a 200 event stream.
  */
private[transport] def respondWithEventStream[F[_], Caps](
    originCheck: OriginCheck,
    headers: Seq[Header],
    backend: ServerStreamingHttpTransport[F, Caps],
    rejection: => Option[McpResponse]
)(handle: OutboundSink[F] => F[Option[Json]])(using m: MonadError[F]): F[Either[JsonRejection, (StatusCode, backend.EventStream)]] =
  if !originAllowed(originCheck, headers) then m.unit(Right((StatusCode.Forbidden, backend.emptyStream)))
  else
    rejection match
      case Some(response) => m.unit(Left((response.statusCode, response.body.getOrElse(Json.Null))))
      case None           => backend.eventStream(handle).map(events => Right((StatusCode.Ok, events)))
