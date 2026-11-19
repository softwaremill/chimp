package chimp.server.transport

import chimp.server.{ListenRequest, McpResponse, OriginCheck, OutboundSink}
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
  * is returned on the left, to be sent as a plain JSON error with its own HTTP status. An accepted `subscriptions/listen` request gets a
  * 200 subscription stream, which stays open until one side ends it; any other request runs `handle`, see
  * [[ServerStreamingHttpTransport.eventStream]].
  */
private[transport] def respondWithEventStream[F[_], Caps](
    originCheck: OriginCheck,
    headers: Seq[Header],
    backend: ServerStreamingHttpTransport[F, Caps],
    rejection: => Option[McpResponse],
    listen: => Option[ListenRequest]
)(handle: OutboundSink[F] => F[McpResponse])(using m: MonadError[F]): F[Either[JsonRejection, (StatusCode, backend.EventStream)]] =
  if !originAllowed(originCheck, headers) then m.unit(Right((StatusCode.Forbidden, backend.emptyStream)))
  else
    rejection match
      case Some(response) => m.unit(Left((response.statusCode, response.body.getOrElse(Json.Null))))
      case None           =>
        listen match
          case Some(request) => m.unit(Right((StatusCode.Ok, backend.subscriptionStream(request))))
          case None          => backend.eventStream(handle).map(_.map(events => (StatusCode.Ok, events)))

/** The plain JSON error to send instead of an event stream, when the handler ends with an error which has its own HTTP status (for example
  * a `-32021`) before it sends any message.
  */
private[server] def plainJsonError(response: McpResponse): Option[(StatusCode, Json)] = response match
  case McpResponse.JsonError(status, json) => Some((status, json))
  case _                                   => None
