package chimp.protocol

import io.circe.{Codec, Decoder, Encoder, Json}

final case class ModelHint(name: Option[String] = None) derives Codec

final case class ModelPreferences(
    hints: Option[List[ModelHint]] = None,
    costPriority: Option[Double] = None,
    speedPriority: Option[Double] = None,
    intelligencePriority: Option[Double] = None
) derives Codec

enum IncludeContext:
  case None, ThisServer, AllServers

object IncludeContext:
  given Encoder[IncludeContext] = Encoder.instance:
    case IncludeContext.None       => Json.fromString("none")
    case IncludeContext.ThisServer => Json.fromString("thisServer")
    case IncludeContext.AllServers => Json.fromString("allServers")
  given Decoder[IncludeContext] = Decoder.decodeString.emap:
    case "none"       => Right(IncludeContext.None)
    case "thisServer" => Right(IncludeContext.ThisServer)
    case "allServers" => Right(IncludeContext.AllServers)
    case other        => Left(s"Unknown includeContext: $other")

final case class SamplingMessage(role: Role, content: ToolContent) derives Codec

final case class CreateMessageParams(
    messages: List[SamplingMessage],
    maxTokens: Int,
    modelPreferences: Option[ModelPreferences] = None,
    systemPrompt: Option[String] = None,
    includeContext: Option[IncludeContext] = None,
    temperature: Option[Double] = None,
    stopSequences: Option[List[String]] = None,
    metadata: Option[Map[String, Json]] = None,
    _meta: Option[Map[String, Json]] = None
) derives Codec

final case class CreateMessageRequest(method: String = "sampling/createMessage", params: CreateMessageParams) derives Codec

final case class CreateMessageResult(
    role: Role,
    content: ToolContent,
    model: String,
    stopReason: Option[String] = None,
    _meta: Option[Map[String, Json]] = None
) derives Codec
