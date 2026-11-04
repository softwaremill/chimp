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
      McpClient[Identity](ClientHttpTransport[Identity](backend, mcpUri, ProtocolVersion.Latest), clientInfo, ProtocolVersion.Latest)
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
    val t = ClientHttpTransport[Identity](backend, mcpUri, ProtocolVersion.Latest)
    val ex = intercept[McpProtocolException](McpClient[Identity](t, clientInfo, ProtocolVersion.Latest))
    ex.getMessage should include("does not support protocol version '2026-07-28'")
