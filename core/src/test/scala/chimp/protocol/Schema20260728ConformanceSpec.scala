package chimp.protocol

import io.circe.syntax.*

import scala.concurrent.duration.*

/** Schema conformance for the modern 2026-07-28 revision: shared datatypes plus the revision-specific defs (result type, cache hints,
  * discover, error codes).
  */
class Schema20260728ConformanceSpec extends SchemaConformance:

  override def schemaResourcePath: String = "/schema/2026-07-28/schema.json"

  it should "produce Implementation that matches the spec schema" in:
    validate("Implementation", Implementation(name = "chimp", version = "1.0", title = Some("Chimp")))

  it should "produce TextContent that matches the spec schema" in:
    validate("TextContent", ToolContent.Text(text = "hi"))

  it should "produce a DiscoverResult that matches the spec schema" in:
    validate(
      "DiscoverResult",
      DiscoverResult(
        supportedVersions = List("2026-07-28", "2025-11-25"),
        capabilities = ServerCapabilities(tools = Some(ServerToolsCapability(listChanged = Some(false)))),
        ttlMs = 0.millis,
        cacheScope = CacheScope.Private,
        instructions = Some("welcome"),
        _meta = Some(Map(ProtocolMeta.ServerInfo -> Implementation(name = "chimp", version = "1.0").asJson.deepDropNullValues))
      )
    )

  it should "produce an UnsupportedProtocolVersionError envelope that matches the spec schema" in:
    val msg: JSONRPCMessage =
      JSONRPCMessage.Error(id = RequestId(1), error = ProtocolMeta.unsupportedVersionError("1900-01-01", List("2026-07-28", "2025-11-25")))
    validate("UnsupportedProtocolVersionError", msg)

  it should "produce a SubscriptionFilter that matches the spec schema" in:
    validate(
      "SubscriptionFilter",
      SubscriptionFilter(resourceSubscriptions = Some(List("file:///x")), resourcesListChanged = Some(true), toolsListChanged = Some(false))
    )

  it should "produce SubscriptionsListenRequestParams that match the spec schema" in:
    validate(
      "SubscriptionsListenRequestParams",
      SubscriptionsListenParams(
        notifications = SubscriptionFilter(resourcesListChanged = Some(true)),
        _meta = Some(Map(ProtocolMeta.ProtocolVersionKey -> "2026-07-28".asJson, ProtocolMeta.ClientCapabilities -> io.circe.Json.obj()))
      )
    )

  it should "produce a SubscriptionsListenResult that matches the spec schema" in:
    validate(
      "SubscriptionsListenResult",
      SubscriptionsListenResult(_meta =
        Some(
          Map(
            ProtocolMeta.ServerInfo -> Implementation(name = "chimp", version = "1.0").asJson.deepDropNullValues,
            ProtocolMeta.SubscriptionId -> "sub-1".asJson
          )
        )
      )
    )

  it should "produce SubscriptionsAcknowledgedNotificationParams that match the spec schema" in:
    validate(
      "SubscriptionsAcknowledgedNotificationParams",
      SubscriptionsAcknowledgedParams(notifications = SubscriptionFilter(promptsListChanged = Some(true)))
    )

  // T7: types present in both revisions gain icons, annotations, _meta and other shared metadata
  private val sampleIcon =
    Icon(src = "https://example.com/icon.png", mimeType = Some("image/png"), sizes = Some(List("48x48")), theme = Some("dark"))

  it should "produce an Icon that matches the spec schema" in:
    validate("Icon", sampleIcon)

  it should "produce Annotations that match the spec schema" in:
    validate(
      "Annotations",
      Annotations(audience = Some(List(Role.User, Role.Assistant)), lastModified = Some("2026-01-12T15:00:58Z"), priority = Some(0.5))
    )

  it should "produce an Implementation with icons, websiteUrl and description that matches the spec schema" in:
    validate(
      "Implementation",
      Implementation(
        "chimp",
        "1.0",
        title = Some("Chimp"),
        icons = Some(List(sampleIcon)),
        websiteUrl = Some("https://example.com"),
        description = Some("test")
      )
    )

  it should "produce a Tool with icons that matches the spec schema" in:
    validate(
      "Tool",
      ToolDefinition(name = "add", inputSchema = io.circe.Json.obj("type" -> "object".asJson), icons = Some(List(sampleIcon)))
    )

  it should "produce a Resource with icons and annotations that matches the spec schema" in:
    validate(
      "Resource",
      Resource(uri = "file:///x", name = "x", icons = Some(List(sampleIcon)), annotations = Some(Annotations(priority = Some(0.3))))
    )

  it should "produce a Prompt with icons that matches the spec schema" in:
    validate("Prompt", Prompt(name = "greet", icons = Some(List(sampleIcon))))

  it should "produce a PromptReference with title that matches the spec schema" in:
    validate("PromptReference", PromptReference(name = "greet", title = Some("Greet")))

  it should "produce a ResourceLink with title, size, icons and annotations that matches the spec schema" in:
    val link: ToolContent = ToolContent.ResourceLink(
      uri = "file:///x",
      name = Some("x"),
      title = Some("X"),
      description = Some("a link"),
      mimeType = Some("text/plain"),
      size = Some(10L),
      icons = Some(List(sampleIcon)),
      annotations = Some(Annotations(priority = Some(0.1)))
    )
    validate("ResourceLink", link)

  it should "produce TextContent with annotations that matches the spec schema" in:
    val text: ToolContent = ToolContent.Text(text = "hi", annotations = Some(Annotations(audience = Some(List(Role.Assistant)))))
    validate("TextContent", text)
