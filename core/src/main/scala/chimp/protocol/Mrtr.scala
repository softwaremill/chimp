package chimp.protocol

import io.circe.syntax.*
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}

/** A server-to-client request embedded in an `input_required` tool result (MRTR, 2026-07-28): a sampling, roots or elicitation request. On
  * the wire it is the bare request object, discriminated by its `method`.
  */
enum InputRequest:
  case Sampling(request: CreateMessageRequest)
  case Roots(request: ListRootsRequest)
  case Elicit(request: ElicitRequest)

object InputRequest:
  given Encoder[InputRequest] = Encoder.instance:
    case Sampling(request) => request.asJson
    case Roots(request)    => request.asJson
    case Elicit(request)   => request.asJson

  given Decoder[InputRequest] = Decoder.instance: (c: HCursor) =>
    c.downField("method")
      .as[String]
      .flatMap:
        case "sampling/createMessage" => c.as[CreateMessageRequest].map(Sampling(_))
        case "roots/list"             => c.as[ListRootsRequest].map(Roots(_))
        case "elicitation/create"     => c.as[ElicitRequest].map(Elicit(_))
        case other                    => Left(DecodingFailure(s"Unknown InputRequest method: $other", c.history))

/** A client-to-server response to an [[InputRequest]] (MRTR): a sampling, roots or elicitation result. Discriminated structurally
  * (elicitation by `action`, roots by `roots`, otherwise sampling).
  */
enum InputResponse:
  case Sampling(result: CreateMessageResult)
  case Roots(result: ListRootsResult)
  case Elicit(result: ElicitResult)

object InputResponse:
  given Encoder[InputResponse] = Encoder.instance:
    case Sampling(result) => result.asJson
    case Roots(result)    => result.asJson
    case Elicit(result)   => result.asJson

  given Decoder[InputResponse] = Decoder.instance: (c: HCursor) =>
    if c.downField("action").succeeded then c.as[ElicitResult].map(Elicit(_))
    else if c.downField("roots").succeeded then c.as[ListRootsResult].map(Roots(_))
    else c.as[CreateMessageResult].map(Sampling(_))

/** Result of a tool call that needs client input before it can finish (MRTR): the pending [[InputRequest]]s keyed by an opaque key, the
  * opaque `requestState` the client echoes back on its response, and the modern result envelope fields.
  */
final case class InputRequiredResult(
    inputRequests: Map[String, InputRequest],
    requestState: Option[String] = None,
    resultType: ResultType = ResultType.Incomplete,
    _meta: Option[Map[String, Json]] = None
) derives io.circe.Codec
