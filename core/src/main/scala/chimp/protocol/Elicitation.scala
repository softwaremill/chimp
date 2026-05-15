package chimp.protocol

import io.circe.{Codec, Decoder, Encoder, Json}

enum ElicitAction:
  case Accept, Decline, Cancel

object ElicitAction:
  given Encoder[ElicitAction] = Encoder.instance:
    case ElicitAction.Accept  => Json.fromString("accept")
    case ElicitAction.Decline => Json.fromString("decline")
    case ElicitAction.Cancel  => Json.fromString("cancel")
  given Decoder[ElicitAction] = Decoder.decodeString.emap:
    case "accept"  => Right(ElicitAction.Accept)
    case "decline" => Right(ElicitAction.Decline)
    case "cancel"  => Right(ElicitAction.Cancel)
    case other     => Left(s"Unknown elicitation action: $other")

final case class ElicitParams(
    message: String,
    requestedSchema: Json,
    _meta: Option[Map[String, Json]] = None
) derives Codec

final case class ElicitRequest(method: String = "elicitation/create", params: ElicitParams) derives Codec

final case class ElicitResult(
    action: ElicitAction,
    content: Option[Map[String, Json]] = None,
    _meta: Option[Map[String, Json]] = None
) derives Codec
