package chimp.protocol

import io.circe.{Codec, Json}

final case class Root(
    uri: String,
    name: Option[String] = None,
    _meta: Option[Map[String, Json]] = None
) derives Codec

final case class ListRootsRequest(method: String = "roots/list") derives Codec

final case class ListRootsResult(
    roots: List[Root],
    _meta: Option[Map[String, Json]] = None
) derives Codec

final case class RootsListChangedNotification(method: String = "notifications/roots/list_changed") derives Codec
