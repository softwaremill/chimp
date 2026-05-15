package chimp.protocol

import io.circe.{Codec, Decoder, Encoder, Json}

enum Role:
  case User, Assistant

object Role:
  given Encoder[Role] = Encoder.instance:
    case Role.User      => Json.fromString("user")
    case Role.Assistant => Json.fromString("assistant")
  given Decoder[Role] = Decoder.decodeString.emap:
    case "user"      => Right(Role.User)
    case "assistant" => Right(Role.Assistant)
    case other       => Left(s"Unknown role: $other")

final case class PromptArgument(
    name: String,
    description: Option[String] = None,
    required: Option[Boolean] = None,
    title: Option[String] = None
) derives Codec

final case class Prompt(
    name: String,
    title: Option[String] = None,
    description: Option[String] = None,
    arguments: Option[List[PromptArgument]] = None,
    _meta: Option[Map[String, Json]] = None
) derives Codec

final case class PromptMessage(role: Role, content: ToolContent) derives Codec

final case class ListPromptsParams(cursor: Option[Cursor] = None, _meta: Option[Map[String, Json]] = None) derives Codec
final case class ListPromptsRequest(method: String = "prompts/list", params: Option[ListPromptsParams] = None) derives Codec
final case class ListPromptsResult(
    prompts: List[Prompt],
    nextCursor: Option[String] = None,
    _meta: Option[Map[String, Json]] = None
) derives Codec

final case class GetPromptParams(
    name: String,
    arguments: Option[Map[String, String]] = None,
    _meta: Option[Map[String, Json]] = None
) derives Codec
final case class GetPromptRequest(method: String = "prompts/get", params: GetPromptParams) derives Codec
final case class GetPromptResult(
    messages: List[PromptMessage],
    description: Option[String] = None,
    _meta: Option[Map[String, Json]] = None
) derives Codec

final case class PromptListChangedNotification(method: String = "notifications/prompts/list_changed") derives Codec

final case class PromptReference(`type`: String = "ref/prompt", name: String) derives Codec
