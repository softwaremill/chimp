package chimp.protocol

import io.circe.{Codec, Json}

final case class ProgressParams(
    progressToken: ProgressToken,
    progress: Double,
    total: Option[Double] = None,
    message: Option[String] = None,
    _meta: Option[Map[String, Json]] = None
) derives Codec

final case class ProgressNotification(method: String = "notifications/progress", params: ProgressParams) derives Codec
