package chimp.client

import chimp.client.transport.HttpTransport
import chimp.protocol.*
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.client4.testing.SyncBackendStub
import sttp.shared.Identity

class McpClientSpec extends AnyFlatSpec with Matchers:

  private val mcpUri = sttp.model.Uri.parse("http://localhost/mcp").toOption.get
  private val clientInfo = Implementation(name = "chimp-test", version = "0.0.1")

  it should "initialize and read the server's protocol version" in:
    val initResult = InitializeResult(
      protocolVersion = ProtocolVersion.Latest,
      capabilities = ServerCapabilities(),
      serverInfo = Implementation(name = "test-server", version = "1.0")
    )
    val responseEnvelope =
      (JSONRPCMessage.Response(id = RequestId(1), result = initResult.asJson): JSONRPCMessage).asJson.noSpaces

    val backend = SyncBackendStub.whenAnyRequest.thenRespondAdjust(responseEnvelope)
    val t = HttpTransport[Identity](backend, mcpUri)
    val client = McpClient[Identity](t, clientInfo)
    val result = client.initialize()
    result.protocolVersion shouldBe ProtocolVersion.Latest
    result.serverInfo.name shouldBe "test-server"

  it should "call a tool and decode the result" in:
    val callResult = CallToolResult(content = List(ToolContent.Text(text = "hi")))
    val responseEnvelope =
      (JSONRPCMessage.Response(id = RequestId(1), result = callResult.asJson): JSONRPCMessage).asJson.noSpaces

    val backend = SyncBackendStub.whenAnyRequest.thenRespondAdjust(responseEnvelope)
    val t = HttpTransport[Identity](backend, mcpUri)
    val client = McpClient[Identity](t, clientInfo)
    val r = client.callTool("echo", io.circe.Json.obj("message" -> io.circe.Json.fromString("hi")))
    r.isError shouldBe false
    r.content.head shouldBe ToolContent.Text("text", "hi")
