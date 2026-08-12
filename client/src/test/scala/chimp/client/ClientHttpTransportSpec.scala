package chimp.client

import chimp.client.transport.ClientHttpTransport
import chimp.protocol.*
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.Assertions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.client4.testing.{RecordingBackend, ResponseStub, SyncBackendStub}
import sttp.model.{Header, Method, StatusCode}
import sttp.shared.Identity

class ClientHttpTransportSpec extends AnyFlatSpec with Matchers:

  private val mcpUri = sttp.model.Uri.parse("http://localhost/mcp").toOption.get

  it should "POST a request and decode the response body" in:
    val result = Json.obj("ok" -> Json.fromBoolean(true))
    val backend = SyncBackendStub.whenAnyRequest.thenRespondAdjust(
      (JSONRPCMessage.Response(id = RequestId(1), result = result): JSONRPCMessage).asJson.noSpaces,
      StatusCode.Ok
    )

    val transport = ClientHttpTransport[Identity](backend, mcpUri)
    val request: JSONRPCMessage = JSONRPCMessage.Request(method = "x", params = None, id = RequestId(1))
    transport.send(request) match
      case Some(JSONRPCMessage.Response(_, _, result)) => Assertions.succeed
      case other                                       => fail(s"Expected Response, got: $other")

  it should "return none for 202 Accepted (notification ack)" in:
    val backend = SyncBackendStub.whenAnyRequest.thenRespondAdjust("", StatusCode.Accepted)
    val transport = ClientHttpTransport[Identity](backend, mcpUri)
    val notification: JSONRPCMessage = JSONRPCMessage.Notification(method = "notifications/initialized")
    transport.send(notification) shouldBe None

  it should "fail with McpAuthorizationException on 401" in:
    val backend = SyncBackendStub.whenAnyRequest.thenRespondAdjust("", StatusCode.Unauthorized)
    val transport = ClientHttpTransport[Identity](backend, mcpUri)
    val request: JSONRPCMessage = JSONRPCMessage.Request(method = "x", params = None, id = RequestId(1))
    val ex = intercept[McpAuthorizationException](transport.send(request))
    ex.statusCode shouldBe 401

  it should "send the given headers with every request" in:
    val body = (JSONRPCMessage.Response(id = RequestId(1), result = Json.obj()): JSONRPCMessage).asJson.noSpaces
    val backend = RecordingBackend(
      SyncBackendStub.whenAnyRequest.thenRespond(ResponseStub.adjust(body, StatusCode.Ok, List(Header("Mcp-Session-Id", "s-1"))))
    )

    val transport = ClientHttpTransport[Identity](backend, mcpUri, headers = List(Header.authorization("Bearer", "t-1")))
    val message: JSONRPCMessage = JSONRPCMessage.Request(method = "x", params = None, id = RequestId(1))
    val _ = transport.send(message)
    transport.close()

    val requests = backend.allInteractions.map { case (request, _) => request }
    requests.map(_.method) shouldBe List(Method.POST, Method.DELETE)
    all(requests.map(_.header("Authorization"))) shouldBe Some("Bearer t-1")

  it should "not let a given header replace a header that the protocol requires" in:
    val body = (JSONRPCMessage.Response(id = RequestId(1), result = Json.obj()): JSONRPCMessage).asJson.noSpaces
    val backend = RecordingBackend(SyncBackendStub.whenAnyRequest.thenRespondAdjust(body, StatusCode.Ok))

    val overrides = List(Header("Accept", "application/json"), Header("MCP-Protocol-Version", "1999-01-01"))
    val transport = ClientHttpTransport[Identity](backend, mcpUri, headers = overrides)
    val message: JSONRPCMessage = JSONRPCMessage.Request(method = "x", params = None, id = RequestId(1))
    val _ = transport.send(message)

    val request = backend.allInteractions.map { case (request, _) => request }.head
    request.headers.filter(_.is("Accept")) shouldBe List(Header("Accept", "application/json, text/event-stream"))
    request.header("MCP-Protocol-Version") shouldBe Some(ProtocolVersion.Latest.name)
