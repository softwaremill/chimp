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
    icons: Option[List[Icon]] = None,
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
  case Text(`type`: String = "text", text: String, annotations: Option[Annotations] = None, _meta: Option[Map[String, Json]] = None)
  case Image(
      `type`: String = "image",
      data: String,
      mimeType: String,
      annotations: Option[Annotations] = None,
      _meta: Option[Map[String, Json]] = None
  )
  case Audio(
      `type`: String = "audio",
      data: String,
      mimeType: String,
      annotations: Option[Annotations] = None,
      _meta: Option[Map[String, Json]] = None
  )
  case ResourceContent(
      `type`: String = "resource",
      resource: ResourceContents,
      annotations: Option[Annotations] = None,
      _meta: Option[Map[String, Json]] = None
  )
  case ResourceLink(
      `type`: String = "resource_link",
      uri: String,
      name: Option[String] = None,
      title: Option[String] = None,
      description: Option[String] = None,
      mimeType: Option[String] = None,
      size: Option[Long] = None,
      icons: Option[List[Icon]] = None,
      annotations: Option[Annotations] = None,
      _meta: Option[Map[String, Json]] = None
  )

object ToolContent:
  given Encoder[ToolContent] = Encoder.instance:
    case Text(_, text, annotations, meta) =>
      Json
        .obj(
          "type" -> Json.fromString("text"),
          "text" -> Json.fromString(text),
          "annotations" -> annotations.asJson,
          "_meta" -> meta.asJson
        )
        .dropNullValues
    case Image(_, data, mimeType, annotations, meta) =>
      Json
        .obj(
          "type" -> Json.fromString("image"),
          "data" -> Json.fromString(data),
          "mimeType" -> Json.fromString(mimeType),
          "annotations" -> annotations.asJson,
          "_meta" -> meta.asJson
        )
        .dropNullValues
    case Audio(_, data, mimeType, annotations, meta) =>
      Json
        .obj(
          "type" -> Json.fromString("audio"),
          "data" -> Json.fromString(data),
          "mimeType" -> Json.fromString(mimeType),
          "annotations" -> annotations.asJson,
          "_meta" -> meta.asJson
        )
        .dropNullValues
    case ResourceContent(_, resource, annotations, meta) =>
      Json
        .obj(
          "type" -> Json.fromString("resource"),
          "resource" -> resource.asJson,
          "annotations" -> annotations.asJson,
          "_meta" -> meta.asJson
        )
        .dropNullValues
    case ResourceLink(_, uri, name, title, description, mimeType, size, icons, annotations, meta) =>
      Json
        .obj(
          "type" -> Json.fromString("resource_link"),
          "uri" -> Json.fromString(uri),
          "name" -> name.asJson,
          "title" -> title.asJson,
          "description" -> description.asJson,
          "mimeType" -> mimeType.asJson,
          "size" -> size.asJson,
          "icons" -> icons.asJson,
          "annotations" -> annotations.asJson,
          "_meta" -> meta.asJson
        )
        .dropNullValues

  given Decoder[ToolContent] = Decoder.instance: (c: HCursor) =>
    c.downField("type")
      .as[String]
      .flatMap:
        case "text" =>
          for
            text <- c.downField("text").as[String]
            annotations <- c.downField("annotations").as[Option[Annotations]]
            meta <- c.downField("_meta").as[Option[Map[String, Json]]]
          yield Text("text", text, annotations, meta)
        case "image" =>
          for
            data <- c.downField("data").as[String]
            mimeType <- c.downField("mimeType").as[String]
            annotations <- c.downField("annotations").as[Option[Annotations]]
            meta <- c.downField("_meta").as[Option[Map[String, Json]]]
          yield Image("image", data, mimeType, annotations, meta)
        case "audio" =>
          for
            data <- c.downField("data").as[String]
            mimeType <- c.downField("mimeType").as[String]
            annotations <- c.downField("annotations").as[Option[Annotations]]
            meta <- c.downField("_meta").as[Option[Map[String, Json]]]
          yield Audio("audio", data, mimeType, annotations, meta)
        case "resource" =>
          for
            resource <- c.downField("resource").as[ResourceContents]
            annotations <- c.downField("annotations").as[Option[Annotations]]
            meta <- c.downField("_meta").as[Option[Map[String, Json]]]
          yield ResourceContent("resource", resource, annotations, meta)
        case "resource_link" =>
          for
            uri <- c.downField("uri").as[String]
            name <- c.downField("name").as[Option[String]]
            title <- c.downField("title").as[Option[String]]
            description <- c.downField("description").as[Option[String]]
            mimeType <- c.downField("mimeType").as[Option[String]]
            size <- c.downField("size").as[Option[Long]]
            icons <- c.downField("icons").as[Option[List[Icon]]]
            annotations <- c.downField("annotations").as[Option[Annotations]]
            meta <- c.downField("_meta").as[Option[Map[String, Json]]]
          yield ResourceLink("resource_link", uri, name, title, description, mimeType, size, icons, annotations, meta)
        case other =>
          Left(DecodingFailure(s"Unknown ToolContent type: $other", c.history))

final case class CallToolParams(
    name: String,
    arguments: Json,
    inputResponses: Option[Map[String, InputResponse]] = None,
    requestState: Option[String] = None,
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
      content <- c.downField("content").as[List[ToolContent]]
      structuredContent <- c.downField("structuredContent").as[Option[Json]]
      isError <- c.downField("isError").as[Option[Boolean]]
      meta <- c.downField("_meta").as[Option[Map[String, Json]]]
    yield CallToolResult(content, structuredContent, isError.getOrElse(false), meta)

final case class ToolListChangedNotification(method: String = "notifications/tools/list_changed") derives Codec
