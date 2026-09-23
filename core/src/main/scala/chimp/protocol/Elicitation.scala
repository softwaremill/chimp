package chimp.protocol

import io.circe.syntax.*
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

/** Params of an `elicitation/create` request (2026-07-28): either a `Form` (a JSON schema to fill in) or a `Url` (open a URL to complete
  * the interaction out of band). `mode` distinguishes them on the wire; it is optional on `Form` for backward compatibility with
  * 2025-11-25.
  */
enum ElicitParams:
  case Form(message: String, requestedSchema: Json, mode: Option[String] = None, _meta: Option[Map[String, Json]] = None)
  case Url(message: String, url: String, mode: String = "url", _meta: Option[Map[String, Json]] = None)

object ElicitParams:
  given Encoder[ElicitParams] = Encoder.instance:
    case Form(message, requestedSchema, mode, meta) =>
      Json
        .obj(
          "message" -> Json.fromString(message),
          "requestedSchema" -> requestedSchema,
          "mode" -> mode.asJson,
          "_meta" -> meta.asJson
        )
        .dropNullValues
    case Url(message, url, mode, meta) =>
      Json
        .obj(
          "message" -> Json.fromString(message),
          "url" -> Json.fromString(url),
          "mode" -> Json.fromString(mode),
          "_meta" -> meta.asJson
        )
        .dropNullValues

  given Decoder[ElicitParams] = Decoder.instance: c =>
    if c.downField("url").succeeded then
      for
        message <- c.downField("message").as[String]
        url <- c.downField("url").as[String]
        mode <- c.downField("mode").as[Option[String]]
        meta <- c.downField("_meta").as[Option[Map[String, Json]]]
      yield Url(message, url, mode.getOrElse("url"), meta)
    else
      for
        message <- c.downField("message").as[String]
        requestedSchema <- c.downField("requestedSchema").as[Json]
        mode <- c.downField("mode").as[Option[String]]
        meta <- c.downField("_meta").as[Option[Map[String, Json]]]
      yield Form(message, requestedSchema, mode, meta)

final case class ElicitRequest(method: String = "elicitation/create", params: ElicitParams) derives Codec

final case class ElicitResult(
    action: ElicitAction,
    content: Option[Map[String, Json]] = None,
    _meta: Option[Map[String, Json]] = None
) derives Codec
