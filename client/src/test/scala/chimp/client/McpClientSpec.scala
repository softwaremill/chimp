package chimp.client

import chimp.client.transport.ClientHttpTransport
import chimp.protocol.*
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.client4.testing.SyncBackendStub
import sttp.client4.{GenericRequest, StringBody}
import sttp.model.StatusCode
import sttp.shared.Identity

class McpClientSpec extends AnyFlatSpec with Matchers:

  private val mcpUri = sttp.model.Uri.parse("http://localhost/mcp").toOption.get
  private val clientInfo = Implementation(name = "chimp-test", version = "0.0.1")

  private def envelopeFor(method: String, request: GenericRequest[?, ?]): Boolean =
    request.body match
      case StringBody(s, _, _) => s.contains(s"\"$method\"")
      case _                   => false

  it should "initialize and expose the server info" in:
    val initResult = InitializeResult(
      protocolVersion = ProtocolVersion.LatestLegacy.name,
      capabilities = ServerCapabilities(),
      serverInfo = Implementation(name = "test-server", version = "1.0")
    )
    val responseEnvelope =
      (JSONRPCMessage.Response(id = RequestId(1), result = initResult.asJson): JSONRPCMessage).asJson.noSpaces

    val backend = SyncBackendStub.whenAnyRequest.thenRespondAdjust(responseEnvelope)
    val client = McpClient[Identity](ClientHttpTransport[Identity](backend, mcpUri), clientInfo, ProtocolVersion.LatestLegacy)
    client.serverInfo.name shouldBe "test-server"

  it should "call a tool and decode the result after initialization" in:
    val initResult = InitializeResult(
      protocolVersion = ProtocolVersion.LatestLegacy.name,
      capabilities = ServerCapabilities(tools = Some(ServerToolsCapability(listChanged = Some(false)))),
      serverInfo = Implementation(name = "test-server", version = "1.0")
    )
    val initEnvelope =
      (JSONRPCMessage.Response(id = RequestId(1), result = initResult.asJson): JSONRPCMessage).asJson.noSpaces
    val callResult = CallToolResult(content = List(ToolContent.Text(text = "hi")))
    val callEnvelope =
      (JSONRPCMessage.Response(id = RequestId(1), result = callResult.asJson): JSONRPCMessage).asJson.noSpaces

    val backend = SyncBackendStub
      .whenRequestMatches(envelopeFor("initialize", _))
      .thenRespondAdjust(initEnvelope)
      .whenRequestMatches(envelopeFor("tools/call", _))
      .thenRespondAdjust(callEnvelope)
      .whenAnyRequest
      .thenRespondAdjust("", StatusCode.Accepted)

    val client = McpClient[Identity](ClientHttpTransport[Identity](backend, mcpUri), clientInfo, ProtocolVersion.LatestLegacy)
    val result = client.callTool("echo", io.circe.Json.obj("message" -> io.circe.Json.fromString("hi")))
    result.isError shouldBe false
    result.content.head shouldBe ToolContent.Text("text", "hi")

  it should "fail fast when a server capability is not negotiated" in:
    val initResult = InitializeResult(
      protocolVersion = ProtocolVersion.LatestLegacy.name,
      capabilities = ServerCapabilities(),
      serverInfo = Implementation(name = "test-server", version = "1.0")
    )
    val initEnvelope =
      (JSONRPCMessage.Response(id = RequestId(1), result = initResult.asJson): JSONRPCMessage).asJson.noSpaces
    val backend = SyncBackendStub.whenAnyRequest.thenRespondAdjust(initEnvelope)
    val client = McpClient[Identity](ClientHttpTransport[Identity](backend, mcpUri), clientInfo, ProtocolVersion.LatestLegacy)
    val ex = intercept[McpProtocolException](client.callTool("anything", io.circe.Json.obj()))
    ex.getMessage should include("Server did not negotiate the capability required for tools/call")

  it should "fail construction when no initialize response is received" in:
    val backend = SyncBackendStub.whenAnyRequest.thenRespondAdjust("")
    val t = ClientHttpTransport[Identity](backend, mcpUri)
    intercept[McpTransportException](McpClient[Identity](t, clientInfo, ProtocolVersion.LatestLegacy))

  // 2026-07-28: no initialize handshake; server/discover, then _meta and SEP-2243 headers on every request
  private def discoverEnvelope(supportedVersions: List[String]): String =
    val result = DiscoverResult(
      supportedVersions = supportedVersions,
      capabilities = ServerCapabilities(tools = Some(ServerToolsCapability(listChanged = Some(false)))),
      ttlMs = scala.concurrent.duration.Duration.Zero,
      cacheScope = CacheScope.Private,
      _meta = Some(Map(ProtocolMeta.ServerInfo -> Implementation(name = "modern-server", version = "2.0").asJson))
    )
    (JSONRPCMessage.Response(id = RequestId(1), result = result.asJson): JSONRPCMessage).asJson.noSpaces

  it should "discover a modern server and send _meta and the Mcp-Method / Mcp-Name headers on each request" in:
    val sent = java.util.concurrent.ConcurrentLinkedQueue[GenericRequest[?, ?]]()
    val callEnvelope =
      (JSONRPCMessage.Response(
        id = RequestId(2),
        result = CallToolResult(content = List(ToolContent.Text(text = "hi"))).asJson
      ): JSONRPCMessage).asJson.noSpaces
    val backend = SyncBackendStub
      .whenRequestMatches(request => sent.add(request) && envelopeFor("server/discover", request))
      .thenRespondAdjust(discoverEnvelope(List("2026-07-28", "2025-11-25")))
      .whenRequestMatches(envelopeFor("tools/call", _))
      .thenRespondAdjust(callEnvelope)

    val client =
      McpClient[Identity](ClientHttpTransport[Identity](backend, mcpUri), clientInfo, ProtocolVersion.Latest)
    client.serverInfo.name shouldBe "modern-server"
    client.callTool("echo", io.circe.Json.obj()).isError shouldBe false

    val requests = sent.toArray(Array.empty[GenericRequest[?, ?]]).toList
    requests.flatMap(_.body match
      case StringBody(s, _, _) => io.circe.parser.parse(s).toOption.flatMap(_.hcursor.get[String]("method").toOption)
      case _                   => None) shouldBe List("server/discover", "tools/call")
    val call = requests.last
    call.header("Mcp-Method") shouldBe Some("tools/call")
    call.header("Mcp-Name") shouldBe Some("echo")
    val meta = call.body match
      case StringBody(s, _, _) => io.circe.parser.parse(s).toOption.flatMap(_.hcursor.downField("params").downField("_meta").focus)
      case _                   => None
    meta.flatMap(_.hcursor.get[String](ProtocolMeta.ProtocolVersionKey).toOption) shouldBe Some("2026-07-28")
    meta.flatMap(_.hcursor.downField(ProtocolMeta.ClientCapabilities).focus) should not be empty
    meta.flatMap(_.hcursor.downField(ProtocolMeta.ClientInfo).get[String]("name").toOption) shouldBe Some("chimp-test")

  it should "fail construction when a discovered server does not support the client's modern version" in:
    val backend = SyncBackendStub.whenAnyRequest.thenRespondAdjust(discoverEnvelope(List("2025-11-25")))
    val t = ClientHttpTransport[Identity](backend, mcpUri)
    val ex = intercept[McpUnsupportedProtocolVersionException](McpClient[Identity](t, clientInfo, ProtocolVersion.Latest))
    ex.serverVersions shouldBe List("2025-11-25")
    ex.clientVersions shouldBe List("2026-07-28")

  it should "retry over HTTP after a -32022 error in the body of an HTTP 400, with the header equal to _meta" in:
    val sent = java.util.concurrent.ConcurrentLinkedQueue[GenericRequest[?, ?]]()
    val rejected = java.util.concurrent.atomic.AtomicBoolean(false)
    val rejection = io.circe.Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id" -> io.circe.Json.Null,
      "error" -> ProtocolMeta.unsupportedVersionError("2026-07-28", List("2026-07-28")).asJson
    )
    val backend = SyncBackendStub
      .whenRequestMatches(request => sent.add(request) && !rejected.getAndSet(true))
      .thenRespondAdjust(rejection.noSpaces, StatusCode.BadRequest)
      .whenAnyRequest
      .thenRespondAdjust(discoverEnvelope(List("2026-07-28")))

    val client = McpClient[Identity](ClientHttpTransport[Identity](backend, mcpUri), clientInfo, ProtocolVersion.Latest)
    client.serverInfo.name shouldBe "modern-server"
    val requests = sent.toArray(Array.empty[GenericRequest[?, ?]]).toList
    requests should have size 2
    requests.foreach: request =>
      val metaVersion = request.body match
        case StringBody(s, _, _) =>
          io.circe.parser
            .parse(s)
            .toOption
            .flatMap(_.hcursor.downField("params").downField("_meta").get[String](ProtocolMeta.ProtocolVersionKey).toOption)
        case _ => None
      request.header("MCP-Protocol-Version") shouldBe metaVersion
      metaVersion shouldBe Some("2026-07-28")

  // x-mcp-header: an HTTP client drops tools with invalid annotations and mirrors annotated arguments into Mcp-Param-* headers
  private val annotatedTools: io.circe.Json = io.circe.parser
    .parse(
      """{"tools":[
        |{"name":"weather","inputSchema":{"type":"object","properties":{
        |  "region":{"type":"string","x-mcp-header":"Region"},"days":{"type":"integer","x-mcp-header":"Days"},
        |  "verbose":{"type":"boolean","x-mcp-header":"Verbose"},"query":{"type":"string"}}}},
        |{"name":"broken","inputSchema":{"type":"object","properties":{"v":{"type":"string","x-mcp-header":"My Region"}}}}
        |]}""".stripMargin
    )
    .fold(throw _, identity)

  private def toolHeadersBackend(sent: java.util.concurrent.ConcurrentLinkedQueue[GenericRequest[?, ?]], connect: String) =
    val call = CallToolResult(content = List(ToolContent.Text(text = "ok"))).asJson
    SyncBackendStub
      .whenRequestMatches(request => sent.add(request) && (envelopeFor("server/discover", request) || envelopeFor("initialize", request)))
      .thenRespondAdjust(connect)
      .whenRequestMatches(envelopeFor("tools/list", _))
      .thenRespondAdjust((JSONRPCMessage.Response(id = RequestId(2), result = annotatedTools): JSONRPCMessage).asJson.noSpaces)
      .whenRequestMatches(envelopeFor("tools/call", _))
      .thenRespondAdjust((JSONRPCMessage.Response(id = RequestId(3), result = call): JSONRPCMessage).asJson.noSpaces)
      .whenAnyRequest
      .thenRespondAdjust("", StatusCode.Accepted)

  private val weatherArguments = io.circe.Json.obj(
    "region" -> " Zürich".asJson,
    "days" -> 3.asJson,
    "verbose" -> io.circe.Json.Null,
    "query" -> "rain".asJson
  )

  it should "drop tools with invalid x-mcp-header annotations and mirror annotated arguments into Mcp-Param headers" in:
    val sent = java.util.concurrent.ConcurrentLinkedQueue[GenericRequest[?, ?]]()
    val client = McpClient[Identity](
      ClientHttpTransport[Identity](toolHeadersBackend(sent, discoverEnvelope(List("2026-07-28"))), mcpUri),
      clientInfo,
      ProtocolVersion.Latest
    )
    client.listTools().tools.map(_.name) shouldBe List("weather")
    val _ = client.callTool("weather", weatherArguments)
    val _ = client.callTool("zürich", io.circe.Json.obj())

    val calls = sent.toArray(Array.empty[GenericRequest[?, ?]]).toList.filter(envelopeFor("tools/call", _))
    val weather = calls.head
    weather.header("Mcp-Name") shouldBe Some("weather")
    weather.header("Mcp-Param-Region") shouldBe Some("=?base64?IFrDvHJpY2g=?=")
    weather.header("Mcp-Param-Days") shouldBe Some("3")
    weather.header("Mcp-Param-Verbose") shouldBe None
    weather.headers.map(_.name.toLowerCase).filter(_.startsWith("mcp-param-")).sorted shouldBe List("mcp-param-days", "mcp-param-region")
    calls(1).header("Mcp-Name") shouldBe Some("=?base64?esO8cmljaA==?=")

  it should "leave legacy requests unchanged: no tool filtering and no Mcp headers" in:
    val sent = java.util.concurrent.ConcurrentLinkedQueue[GenericRequest[?, ?]]()
    val initResult = InitializeResult(
      protocolVersion = ProtocolVersion.LatestLegacy.name,
      capabilities = ServerCapabilities(tools = Some(ServerToolsCapability(listChanged = Some(false)))),
      serverInfo = Implementation(name = "legacy-server", version = "1.0")
    )
    val connect = (JSONRPCMessage.Response(id = RequestId(1), result = initResult.asJson): JSONRPCMessage).asJson.noSpaces
    val client = McpClient[Identity](ClientHttpTransport[Identity](toolHeadersBackend(sent, connect), mcpUri), clientInfo)
    client.listTools().tools.map(_.name) shouldBe List("weather", "broken")
    val _ = client.callTool("weather", weatherArguments)

    val call = sent.toArray(Array.empty[GenericRequest[?, ?]]).toList.filter(envelopeFor("tools/call", _)).head
    call.headers.map(_.name.toLowerCase).filter(name => name.startsWith("mcp-") && name != "mcp-protocol-version") shouldBe Nil
