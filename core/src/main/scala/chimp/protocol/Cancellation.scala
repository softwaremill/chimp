package chimp.protocol

import io.circe.{Codec, Json}

final case class CancelledParams(
    requestId: RequestId,
    reason: Option[String] = None,
    _meta: Option[Map[String, Json]] = None
) derives Codec

final case class CancelledNotification(method: String = "notifications/cancelled", params: CancelledParams) derives Codec
