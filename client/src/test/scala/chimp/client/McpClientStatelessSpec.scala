package chimp.client

import chimp.protocol.*
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.shared.Identity

import scala.concurrent.duration.Duration

class McpClientStatelessSpec extends AnyFlatSpec with Matchers:

  private val clientInfo = Implementation(name = "chimp-test", version = "0.0.1")
  private val serverInfo = Implementation(name = "modern-server", version = "2.0")
  private val toolsCapability = ServerCapabilities(tools = Some(ServerToolsCapability(listChanged = Some(false))))

  private def response(result: Json): JSONRPCMessage = JSONRPCMessage.Response(id = RequestId(1), result = result)

  private def discoverResult(
      supportedVersions: List[String] = List(ProtocolVersion.Latest.name),
      capabilities: ServerCapabilities = toolsCapability
  ): Json =
    DiscoverResult(
      supportedVersions = supportedVersions,
      capabilities = capabilities,
      ttlMs = Duration.Zero,
      cacheScope = CacheScope.Private,
      _meta = Some(Map(ProtocolMeta.ServerInfo -> serverInfo.asJson))
    ).asJson.deepDropNullValues

  private def unsupportedVersion(supported: List[String]): JSONRPCMessage =
    JSONRPCMessage.Error(id = RequestId(1), error = ProtocolMeta.unsupportedVersionError(ProtocolVersion.Latest.name, supported))

  private val callResult: Json = CallToolResult(content = List(ToolContent.Text(text = "hi"))).asJson

  private def modernClient(transport: InMemoryTransport, logLevel: Option[LoggingLevel] = None): McpClient[Identity] =
    transport.planResponse(response(discoverResult()))
    McpClient[Identity](transport, clientInfo, ProtocolVersion.Latest, logLevel)

  private def methods(transport: InMemoryTransport): List[String] = transport.sent.toList.collect:
    case JSONRPCMessage.Request(_, method, _, _)   => method
    case JSONRPCMessage.Notification(_, method, _) => method

  private def metaOf(message: JSONRPCMessage): Option[Json] = message match
    case JSONRPCMessage.Request(_, _, params, _) => params.flatMap(_.hcursor.downField("_meta").focus)
    case _                                       => None

  "A stateless client" should "call server/discover instead of initialize, and send no notifications/initialized" in:
    val transport = InMemoryTransport()
    val client = modernClient(transport)
    methods(transport) shouldBe List("server/discover")
    client.serverInfo shouldBe serverInfo
    client.serverCapabilities shouldBe toolsCapability
    client.protocolVersion shouldBe ProtocolVersion.Latest

  it should "put the protocol version, the client info and the client capabilities of its handlers in the _meta of every request" in:
    val transport = InMemoryTransport()
    transport.planResponse(response(discoverResult()))
    transport.planResponse(response(callResult))
    val client = McpClient.bidirectional[Identity](
      transport,
      clientInfo,
      rootsHandler = Some(() => ListRootsResult(roots = Nil)),
      protocolVersion = ProtocolVersion.Latest
    )
    val _ = client.callTool("echo", Json.obj())
    val metas = transport.sent.toList.map(metaOf)
    metas should have size 2
    all(metas.map(_.flatMap(_.hcursor.get[String](ProtocolMeta.ProtocolVersionKey).toOption))) shouldBe Some("2026-07-28")
    all(metas.map(_.flatMap(_.hcursor.downField(ProtocolMeta.ClientInfo).as[Implementation].toOption))) shouldBe Some(clientInfo)
    all(metas.map(_.flatMap(_.hcursor.downField(ProtocolMeta.ClientCapabilities).as[ClientCapabilities].toOption))) shouldBe Some(
      ClientCapabilities(roots = Some(ClientRootsCapability(listChanged = Some(true))))
    )
    all(metas.map(_.flatMap(_.hcursor.downField(ProtocolMeta.LogLevel).focus))) shouldBe None

  it should "put the log level option in the _meta of every request" in:
    val transport = InMemoryTransport()
    val client = modernClient(transport, logLevel = Some(LoggingLevel.Warning))
    transport.planResponse(response(callResult))
    val _ = client.callTool("echo", Json.obj())
    all(transport.sent.toList.map(metaOf(_).flatMap(_.hcursor.get[String](ProtocolMeta.LogLevel).toOption))) shouldBe Some("warning")

  it should "keep the _meta fields that a request already has" in:
    val transport = InMemoryTransport()
    val client = modernClient(transport)
    transport.planResponse(response(callResult))
    val _ = client.callTool("echo", Json.obj())
    metaOf(transport.sent.last).flatMap(_.hcursor.get[String](ProtocolMeta.ProtocolVersionKey).toOption) shouldBe Some("2026-07-28")
    transport.sent.last match
      case JSONRPCMessage.Request(_, _, Some(params), _) => params.hcursor.get[String]("name") shouldBe Right("echo")
      case other                                         => fail(s"Expected a request, got $other")

  it should "treat a result without resultType as complete" in:
    val transport = InMemoryTransport()
    val client = modernClient(transport)
    transport.planResponse(response(callResult.mapObject(_.remove("resultType"))))
    client.callTool("echo", Json.obj()).content shouldBe List(ToolContent.Text("text", "hi"))

  it should "fail with McpProtocolException on an unknown resultType" in:
    val transport = InMemoryTransport()
    val client = modernClient(transport)
    transport.planResponse(response(callResult.deepMerge(Json.obj("resultType" -> "partial".asJson))))
    val ex = intercept[McpProtocolException](client.callTool("echo", Json.obj()))
    ex.getMessage should include("unknown resultType")

  it should "fail with McpProtocolException on an input_required result, as it does not retry yet" in:
    val transport = InMemoryTransport()
    val client = modernClient(transport)
    transport.planResponse(response(Json.obj("resultType" -> "input_required".asJson, "inputRequests" -> Json.obj())))
    val ex = intercept[McpProtocolException](client.callTool("echo", Json.obj()))
    ex.getMessage should include("input_required")

  it should "retry once with a common version from the -32022 error, and keep that version" in:
    val transport = InMemoryTransport()
    val client = modernClient(transport)
    transport.planResponse(unsupportedVersion(List("2099-01-01", ProtocolVersion.Latest.name)))
    transport.planResponse(response(callResult))
    transport.planResponse(response(callResult))
    val _ = client.callTool("echo", Json.obj())
    val _ = client.callTool("echo", Json.obj())
    methods(transport) shouldBe List("server/discover", "tools/call", "tools/call", "tools/call")
    client.protocolVersion shouldBe ProtocolVersion.Latest

  it should "negotiate the version on -32022 during discovery" in:
    val transport = InMemoryTransport()
    transport.planResponse(unsupportedVersion(List(ProtocolVersion.Latest.name)))
    transport.planResponse(response(discoverResult()))
    val client = McpClient[Identity](transport, clientInfo, ProtocolVersion.Latest)
    methods(transport) shouldBe List("server/discover", "server/discover")
    client.serverInfo shouldBe serverInfo

  it should "fail with an error that names both version lists when -32022 offers no common version" in:
    val transport = InMemoryTransport()
    val client = modernClient(transport)
    transport.planResponse(unsupportedVersion(List("2099-01-01", "2025-11-25")))
    val ex = intercept[McpUnsupportedProtocolVersionException](client.callTool("echo", Json.obj()))
    ex.serverVersions shouldBe List("2099-01-01", "2025-11-25")
    ex.clientVersions shouldBe List("2026-07-28")
    ex.getMessage should (include("2099-01-01") and include("2026-07-28"))
    methods(transport) shouldBe List("server/discover", "tools/call")

  it should "retry only once" in:
    val transport = InMemoryTransport()
    val client = modernClient(transport)
    transport.planResponse(unsupportedVersion(List(ProtocolVersion.Latest.name)))
    transport.planResponse(unsupportedVersion(List(ProtocolVersion.Latest.name)))
    val _ = intercept[McpUnsupportedProtocolVersionException](client.callTool("echo", Json.obj()))
    methods(transport) shouldBe List("server/discover", "tools/call", "tools/call")

  it should "offer server/discover as an operation" in:
    val transport = InMemoryTransport()
    val client = modernClient(transport)
    transport.planResponse(response(discoverResult().deepMerge(Json.obj("instructions" -> "be nice".asJson))))
    client.discover().instructions shouldBe Some("be nice")

  it should "guard operations with the capabilities from the discover result" in:
    val transport = InMemoryTransport()
    transport.planResponse(response(discoverResult(capabilities = ServerCapabilities())))
    val client = McpClient[Identity](transport, clientInfo, ProtocolVersion.Latest)
    val ex = intercept[McpProtocolException](client.callTool("echo", Json.obj()))
    ex.getMessage should include("Server did not negotiate the capability required for tools/call")
    methods(transport) shouldBe List("server/discover")

  it should "read serverInfo from the top level of the discover result, or use 'unknown' when there is none" in:
    val topLevel = InMemoryTransport()
    topLevel.planResponse(
      response(discoverResult().mapObject(_.remove("_meta")).deepMerge(Json.obj("serverInfo" -> serverInfo.asJson)))
    )
    McpClient[Identity](topLevel, clientInfo, ProtocolVersion.Latest).serverInfo shouldBe serverInfo

    val missing = InMemoryTransport()
    missing.planResponse(response(discoverResult().mapObject(_.remove("_meta"))))
    McpClient[Identity](missing, clientInfo, ProtocolVersion.Latest).serverInfo shouldBe Implementation("unknown", "unknown")

  it should "reject the legacy-only operations without sending them" in:
    val transport = InMemoryTransport()
    transport.planResponse(response(discoverResult(capabilities = toolsCapability.copy(logging = Some(Json.obj())))))
    val client = McpClient.bidirectional[Identity](transport, clientInfo, protocolVersion = ProtocolVersion.Latest)
    val operations: List[(String, () => Unit)] = List(
      "ping" -> (() => client.ping()),
      "logging/setLevel" -> (() => client.setLoggingLevel(LoggingLevel.Info)),
      "resources/subscribe" -> (() => client.subscribeResource("file:///a")),
      "resources/unsubscribe" -> (() => client.unsubscribeResource("file:///a")),
      "notifications/progress" -> (() => client.sendProgress(ProgressToken("t"), 1.0)),
      "notifications/roots/list_changed" -> (() => client.sendRootsListChanged())
    )
    operations.foreach: (method, operation) =>
      val ex = intercept[McpProtocolException](operation())
      ex.getMessage should include(method)
    methods(transport) shouldBe List("server/discover")

  "A legacy client" should "reject server/discover" in:
    val transport = InMemoryTransport()
    transport.planResponse(response(legacyInitResult(ServerCapabilities())))
    val client = McpClient[Identity](transport, clientInfo)
    val _ = intercept[McpProtocolException](client.discover())
    client.protocolVersion shouldBe ProtocolVersion.LatestLegacy

  it should "use the version that the server negotiates in initialize" in:
    val transport = InMemoryTransport()
    transport.planResponse(response(legacyInitResult(ServerCapabilities(), ProtocolVersion.V2025_06_18)))
    McpClient[Identity](transport, clientInfo).protocolVersion shouldBe ProtocolVersion.V2025_06_18

  it should "send the log level option once with logging/setLevel after initialize" in:
    val transport = InMemoryTransport()
    transport.planResponse(response(legacyInitResult(ServerCapabilities(logging = Some(Json.obj())))))
    transport.planResponse(response(Json.obj()))
    val _ = McpClient[Identity](transport, clientInfo, logLevel = Some(LoggingLevel.Error))
    methods(transport) shouldBe List("initialize", "notifications/initialized", "logging/setLevel")
    transport.sent.last match
      case JSONRPCMessage.Request(_, _, Some(params), _) => params.hcursor.get[String]("level") shouldBe Right("error")
      case other                                         => fail(s"Expected a request, got $other")

  it should "not send logging/setLevel when the server has no logging capability" in:
    val transport = InMemoryTransport()
    transport.planResponse(response(legacyInitResult(ServerCapabilities())))
    val _ = McpClient[Identity](transport, clientInfo, logLevel = Some(LoggingLevel.Error))
    methods(transport) shouldBe List("initialize", "notifications/initialized")

  private def legacyInitResult(capabilities: ServerCapabilities, version: ProtocolVersion = ProtocolVersion.LatestLegacy): Json =
    InitializeResult(protocolVersion = version.name, capabilities = capabilities, serverInfo = serverInfo).asJson
