package chimp.protocol

import io.circe.{Codec, Json}

final case class Implementation(name: String, version: String, title: Option[String] = None) derives Codec

final case class ClientRootsCapability(listChanged: Option[Boolean] = None) derives Codec

final case class ClientCapabilities(
    experimental: Option[Map[String, Json]] = None,
    roots: Option[ClientRootsCapability] = None,
    sampling: Option[Json] = None,
    elicitation: Option[Json] = None
) derives Codec

final case class ServerPromptsCapability(listChanged: Option[Boolean] = None) derives Codec
final case class ServerResourcesCapability(subscribe: Option[Boolean] = None, listChanged: Option[Boolean] = None) derives Codec
final case class ServerToolsCapability(listChanged: Option[Boolean] = None) derives Codec

final case class ServerCapabilities(
    experimental: Option[Map[String, Json]] = None,
    logging: Option[Json] = None,
    completions: Option[Json] = None,
    prompts: Option[ServerPromptsCapability] = None,
    resources: Option[ServerResourcesCapability] = None,
    tools: Option[ServerToolsCapability] = None
) derives Codec

final case class InitializeParams(
    protocolVersion: String,
    capabilities: ClientCapabilities,
    clientInfo: Implementation,
    _meta: Option[Map[String, Json]] = None
) derives Codec

final case class InitializeRequest(method: String = "initialize", params: InitializeParams) derives Codec

final case class InitializeResult(
    protocolVersion: String,
    capabilities: ServerCapabilities,
    serverInfo: Implementation,
    instructions: Option[String] = None,
    _meta: Option[Map[String, Json]] = None
) derives Codec

final case class InitializedNotification(method: String = "notifications/initialized") derives Codec

final case class PingRequest(method: String = "ping") derives Codec
