package chimp.protocol

import io.circe.syntax.*
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, HCursor, Json}

final case class Resource(
    uri: String,
    name: String,
    title: Option[String] = None,
    description: Option[String] = None,
    mimeType: Option[String] = None,
    size: Option[Long] = None,
    _meta: Option[Map[String, Json]] = None
) derives Codec

final case class ResourceTemplate(
    uriTemplate: String,
    name: String,
    title: Option[String] = None,
    description: Option[String] = None,
    mimeType: Option[String] = None,
    _meta: Option[Map[String, Json]] = None
) derives Codec

enum ResourceContents:
  case Text(uri: String, text: String, mimeType: Option[String] = None, _meta: Option[Map[String, Json]] = None)
  case Blob(uri: String, blob: String, mimeType: Option[String] = None, _meta: Option[Map[String, Json]] = None)

object ResourceContents:
  given Encoder[ResourceContents] = Encoder.instance:
    case Text(uri, text, mimeType, meta) =>
      Json
        .obj(
          "uri" -> Json.fromString(uri),
          "text" -> Json.fromString(text),
          "mimeType" -> mimeType.map(Json.fromString).getOrElse(Json.Null),
          "_meta" -> meta.map(_.asJson).getOrElse(Json.Null)
        )
        .dropNullValues
    case Blob(uri, blob, mimeType, meta) =>
      Json
        .obj(
          "uri" -> Json.fromString(uri),
          "blob" -> Json.fromString(blob),
          "mimeType" -> mimeType.map(Json.fromString).getOrElse(Json.Null),
          "_meta" -> meta.map(_.asJson).getOrElse(Json.Null)
        )
        .dropNullValues

  given Decoder[ResourceContents] = Decoder.instance: (c: HCursor) =>
    val uri = c.downField("uri").as[String]
    val mimeType = c.downField("mimeType").as[Option[String]]
    val meta = c.downField("_meta").as[Option[Map[String, Json]]]
    val textOpt = c.downField("text").as[Option[String]]
    val blobOpt = c.downField("blob").as[Option[String]]
    for
      u <- uri
      mt <- mimeType
      m <- meta
      t <- textOpt
      b <- blobOpt
      r <- (t, b) match
        case (Some(text), None) => Right(Text(u, text, mt, m))
        case (None, Some(blob)) => Right(Blob(u, blob, mt, m))
        case _                  => Left(DecodingFailure("ResourceContents must have exactly one of 'text' or 'blob'", c.history))
    yield r

final case class ListResourcesParams(cursor: Option[Cursor] = None, _meta: Option[Map[String, Json]] = None) derives Codec
final case class ListResourcesRequest(method: String = "resources/list", params: Option[ListResourcesParams] = None) derives Codec
final case class ListResourcesResult(
    resources: List[Resource],
    nextCursor: Option[String] = None,
    _meta: Option[Map[String, Json]] = None
) derives Codec

final case class ListResourceTemplatesParams(cursor: Option[Cursor] = None, _meta: Option[Map[String, Json]] = None) derives Codec
final case class ListResourceTemplatesRequest(
    method: String = "resources/templates/list",
    params: Option[ListResourceTemplatesParams] = None
) derives Codec
final case class ListResourceTemplatesResult(
    resourceTemplates: List[ResourceTemplate],
    nextCursor: Option[String] = None,
    _meta: Option[Map[String, Json]] = None
) derives Codec

final case class ReadResourceParams(uri: String, _meta: Option[Map[String, Json]] = None) derives Codec
final case class ReadResourceRequest(method: String = "resources/read", params: ReadResourceParams) derives Codec
final case class ReadResourceResult(contents: List[ResourceContents], _meta: Option[Map[String, Json]] = None) derives Codec

final case class SubscribeParams(uri: String, _meta: Option[Map[String, Json]] = None) derives Codec
final case class SubscribeRequest(method: String = "resources/subscribe", params: SubscribeParams) derives Codec

final case class UnsubscribeParams(uri: String, _meta: Option[Map[String, Json]] = None) derives Codec
final case class UnsubscribeRequest(method: String = "resources/unsubscribe", params: UnsubscribeParams) derives Codec

final case class ResourceUpdatedParams(uri: String, _meta: Option[Map[String, Json]] = None) derives Codec
final case class ResourceUpdatedNotification(
    method: String = "notifications/resources/updated",
    params: ResourceUpdatedParams
) derives Codec

final case class ResourceListChangedNotification(method: String = "notifications/resources/list_changed") derives Codec

final case class ResourceReference(`type`: String = "ref/resource", uri: String) derives Codec
