package chimp.protocol

import io.circe.syntax.*
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, HCursor, Json}

final case class ToolAnnotations(
    title: Option[String] = None,
    readOnlyHint: Option[Boolean] = None,
    destructiveHint: Option[Boolean] = None,
    idempotentHint: Option[Boolean] = None,
    openWorldHint: Option[Boolean] = None
) derives Codec

final case class ToolDefinition(
    name: String,
    description: Option[String] = None,
    inputSchema: Json,
    outputSchema: Option[Json] = None,
    title: Option[String] = None,
    annotations: Option[ToolAnnotations] = None,
    _meta: Option[Map[String, Json]] = None
) derives Codec

final case class ListToolsParams(cursor: Option[Cursor] = None, _meta: Option[Map[String, Json]] = None) derives Codec

final case class ListToolsRequest(method: String = "tools/list", params: Option[ListToolsParams] = None) derives Codec

final case class ListToolsResponse(
    tools: List[ToolDefinition],
    nextCursor: Option[String] = None,
    _meta: Option[Map[String, Json]] = None
) derives Codec

enum ToolContent:
  case Text(`type`: String = "text", text: String)
  case Image(`type`: String = "image", data: String, mimeType: String)
  case Audio(`type`: String = "audio", data: String, mimeType: String)
  case ResourceContent(`type`: String = "resource", resource: ResourceContents)
  case ResourceLink(
      `type`: String = "resource_link",
      uri: String,
      name: Option[String] = None,
      description: Option[String] = None,
      mimeType: Option[String] = None
  )

object ToolContent:
  given Encoder[ToolContent] = Encoder.instance:
    case Text(_, text) =>
      Json.obj(
        "type" -> Json.fromString("text"),
        "text" -> Json.fromString(text)
      )
    case Image(_, data, mimeType) =>
      Json.obj(
        "type"     -> Json.fromString("image"),
        "data"     -> Json.fromString(data),
        "mimeType" -> Json.fromString(mimeType)
      )
    case Audio(_, data, mimeType) =>
      Json.obj(
        "type"     -> Json.fromString("audio"),
        "data"     -> Json.fromString(data),
        "mimeType" -> Json.fromString(mimeType)
      )
    case ResourceContent(_, resource) =>
      Json.obj(
        "type"     -> Json.fromString("resource"),
        "resource" -> resource.asJson
      )
    case ResourceLink(_, uri, name, description, mimeType) =>
      Json
        .obj(
          "type"        -> Json.fromString("resource_link"),
          "uri"         -> Json.fromString(uri),
          "name"        -> name.map(Json.fromString).getOrElse(Json.Null),
          "description" -> description.map(Json.fromString).getOrElse(Json.Null),
          "mimeType"    -> mimeType.map(Json.fromString).getOrElse(Json.Null)
        )
        .dropNullValues

  given Decoder[ToolContent] = Decoder.instance: (c: HCursor) =>
    c.downField("type").as[String].flatMap:
      case "text" =>
        c.downField("text").as[String].map(Text("text", _))
      case "image" =>
        for
          data     <- c.downField("data").as[String]
          mimeType <- c.downField("mimeType").as[String]
        yield Image("image", data, mimeType)
      case "audio" =>
        for
          data     <- c.downField("data").as[String]
          mimeType <- c.downField("mimeType").as[String]
        yield Audio("audio", data, mimeType)
      case "resource" =>
        c.downField("resource").as[ResourceContents].map(ResourceContent("resource", _))
      case "resource_link" =>
        for
          uri         <- c.downField("uri").as[String]
          name        <- c.downField("name").as[Option[String]]
          description <- c.downField("description").as[Option[String]]
          mimeType    <- c.downField("mimeType").as[Option[String]]
        yield ResourceLink("resource_link", uri, name, description, mimeType)
      case other =>
        Left(DecodingFailure(s"Unknown ToolContent type: $other", c.history))

final case class CallToolParams(
    name: String,
    arguments: Json,
    _meta: Option[Map[String, Json]] = None
) derives Codec

final case class CallToolRequest(method: String = "tools/call", params: CallToolParams) derives Codec

final case class CallToolResult(
    content: List[ToolContent],
    structuredContent: Option[Json] = None,
    isError: Boolean = false,
    _meta: Option[Map[String, Json]] = None
)

object CallToolResult:
  given Encoder[CallToolResult] = Encoder.AsObject.derived[CallToolResult]
  given Decoder[CallToolResult] = Decoder.instance: c =>
    for
      content           <- c.downField("content").as[List[ToolContent]]
      structuredContent <- c.downField("structuredContent").as[Option[Json]]
      isError           <- c.downField("isError").as[Option[Boolean]]
      meta              <- c.downField("_meta").as[Option[Map[String, Json]]]
    yield CallToolResult(content, structuredContent, isError.getOrElse(false), meta)

final case class ToolListChangedNotification(method: String = "notifications/tools/list_changed") derives Codec
