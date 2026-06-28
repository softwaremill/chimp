package chimp.client

import chimp.client.transport.ClientHttpTransport
import chimp.protocol.*
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.Assertions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.client4.testing.SyncBackendStub
import sttp.model.StatusCode
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
