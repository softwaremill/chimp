package mcp.model

import io.circe.Codec
import io.circe.Json

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
    annotations: Option[ToolAnnotations] = None
) derives Codec

final case class ListToolsResponse(
    tools: List[ToolDefinition]
) derives Codec
