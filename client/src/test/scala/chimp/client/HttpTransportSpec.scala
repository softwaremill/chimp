package chimp.client

import chimp.client.transport.HttpTransport
import chimp.protocol.*
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.client4.testing.SyncBackendStub
import sttp.model.StatusCode
import sttp.shared.Identity

class HttpTransportSpec extends AnyFlatSpec with Matchers:

  private val mcpUri = sttp.model.Uri.parse("http://localhost/mcp").toOption.get

  private def envelope(body: JSONRPCMessage): String = (body: JSONRPCMessage).asJson.noSpaces

  it should "POST a request and decode the response body" in:
    val expectedResult = Json.obj("ok" -> Json.fromBoolean(true))
    val backend = SyncBackendStub.whenAnyRequest.thenRespondAdjust(
      envelope(JSONRPCMessage.Response(id = RequestId(1), result = expectedResult)),
      StatusCode.Ok
    )

    val t = HttpTransport[Identity](backend, mcpUri)
    val req: JSONRPCMessage = JSONRPCMessage.Request(method = "x", params = None, id = RequestId(1))
    t.send(req) match
      case Some(JSONRPCMessage.Response(_, _, r)) => r shouldBe expectedResult
      case other                                  => fail(s"Expected Response, got: $other")

  it should "return None for 202 Accepted (notification ack)" in:
    val backend = SyncBackendStub.whenAnyRequest.thenRespondAdjust("", StatusCode.Accepted)
    val t = HttpTransport[Identity](backend, mcpUri)
    val n: JSONRPCMessage = JSONRPCMessage.Notification(method = "notifications/initialized")
    t.send(n) shouldBe None

  it should "fail with McpAuthorizationException on 401" in:
    val backend = SyncBackendStub.whenAnyRequest.thenRespondAdjust("", StatusCode.Unauthorized)
    val t = HttpTransport[Identity](backend, mcpUri)
    val req: JSONRPCMessage = JSONRPCMessage.Request(method = "x", params = None, id = RequestId(1))
    val ex = intercept[McpAuthorizationException](t.send(req))
    ex.statusCode shouldBe 401
