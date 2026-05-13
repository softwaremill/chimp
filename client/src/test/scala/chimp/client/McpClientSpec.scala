package chimp.client

import chimp.client.transport.HttpTransport
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

  it should "initialize and read the server's protocol version" in:
    val initResult = InitializeResult(
      protocolVersion = ProtocolVersion.Latest.name,
      capabilities = ServerCapabilities(),
      serverInfo = Implementation(name = "test-server", version = "1.0")
    )
    val responseEnvelope =
      (JSONRPCMessage.Response(id = RequestId(1), result = initResult.asJson): JSONRPCMessage).asJson.noSpaces

    val backend = SyncBackendStub.whenAnyRequest.thenRespondAdjust(responseEnvelope)
    val t = HttpTransport[Identity](backend, mcpUri)
    val client = McpClient[Identity](t, clientInfo)
    val result = client.initialize()
    result.protocolVersion shouldBe ProtocolVersion.Latest.name
    result.serverInfo.name shouldBe "test-server"

  it should "call a tool and decode the result after initialization" in:
    val initResult = InitializeResult(
      protocolVersion = ProtocolVersion.Latest.name,
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

    val client = McpClient[Identity](HttpTransport[Identity](backend, mcpUri), clientInfo)
    val _ = client.initialize()
    val result = client.callTool("echo", io.circe.Json.obj("message" -> io.circe.Json.fromString("hi")))
    result.isError shouldBe false
    result.content.head shouldBe ToolContent.Text("text", "hi")

  it should "fail fast when a server capability is not negotiated" in:
    val initResult = InitializeResult(
      protocolVersion = ProtocolVersion.Latest.name,
      capabilities = ServerCapabilities(),
      serverInfo = Implementation(name = "test-server", version = "1.0")
    )
    val initEnvelope =
      (JSONRPCMessage.Response(id = RequestId(1), result = initResult.asJson): JSONRPCMessage).asJson.noSpaces
    val backend = SyncBackendStub.whenAnyRequest.thenRespondAdjust(initEnvelope)
    val client = McpClient[Identity](HttpTransport[Identity](backend, mcpUri), clientInfo)
    val _ = client.initialize()
    val ex = intercept[McpProtocolException](client.callTool("anything", io.circe.Json.obj()))
    ex.getMessage should include("Server did not negotiate the capability required for tools/call")

  it should "fail fast when called before initialization" in:
    val backend = SyncBackendStub.whenAnyRequest.thenRespondAdjust("")
    val t = HttpTransport[Identity](backend, mcpUri)
    val client = McpClient[Identity](t, clientInfo)
    val ex = intercept[McpProtocolException](client.listTools())
    ex.getMessage should include("Client not initialized")
