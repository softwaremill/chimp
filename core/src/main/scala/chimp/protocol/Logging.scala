package chimp.protocol

import io.circe.{Codec, Decoder, Encoder, Json}

enum LoggingLevel:
  case Debug, Info, Notice, Warning, Error, Critical, Alert, Emergency

object LoggingLevel:
  given Encoder[LoggingLevel] = Encoder.instance: l =>
    Json.fromString(l.toString.toLowerCase)
  given Decoder[LoggingLevel] = Decoder.decodeString.emap:
    case "debug"     => Right(LoggingLevel.Debug)
    case "info"      => Right(LoggingLevel.Info)
    case "notice"    => Right(LoggingLevel.Notice)
    case "warning"   => Right(LoggingLevel.Warning)
    case "error"     => Right(LoggingLevel.Error)
    case "critical"  => Right(LoggingLevel.Critical)
    case "alert"     => Right(LoggingLevel.Alert)
    case "emergency" => Right(LoggingLevel.Emergency)
    case other       => Left(s"Unknown logging level: $other")

final case class SetLevelParams(level: LoggingLevel, _meta: Option[Map[String, Json]] = None) derives Codec

final case class SetLevelRequest(method: String = "logging/setLevel", params: SetLevelParams) derives Codec

final case class LoggingMessageParams(
    level: LoggingLevel,
    data: Json,
    logger: Option[String] = None,
    _meta: Option[Map[String, Json]] = None
) derives Codec

final case class LoggingMessageNotification(
    method: String = "notifications/message",
    params: LoggingMessageParams
) derives Codec
