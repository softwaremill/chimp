package chimp.client

import chimp.client.notifications.ServerNotification
import chimp.protocol.*
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ServerNotificationSpec extends AnyFlatSpec with Matchers:

  it should "parse a progress notification" in:
    val params = ProgressParams(progressToken = ProgressToken("t1"), progress = 0.5, total = Some(1.0))
    val n = JSONRPCMessage.Notification(method = "notifications/progress", params = Some(params.asJson))
    ServerNotification.parse(n) shouldBe ServerNotification.Progress(params)

  it should "parse a cancelled notification" in:
    val params = CancelledParams(requestId = RequestId(7), reason = Some("user cancelled"))
    val n = JSONRPCMessage.Notification(method = "notifications/cancelled", params = Some(params.asJson))
    ServerNotification.parse(n) shouldBe ServerNotification.Cancelled(params)

  it should "parse list_changed notifications without params" in:
    val toolsN: JSONRPCMessage.Notification = JSONRPCMessage.Notification(method = "notifications/tools/list_changed")
    val resourcesN: JSONRPCMessage.Notification = JSONRPCMessage.Notification(method = "notifications/resources/list_changed")
    val promptsN: JSONRPCMessage.Notification = JSONRPCMessage.Notification(method = "notifications/prompts/list_changed")
    ServerNotification.parse(toolsN) shouldBe ServerNotification.ToolsListChanged
    ServerNotification.parse(resourcesN) shouldBe ServerNotification.ResourcesListChanged
    ServerNotification.parse(promptsN) shouldBe ServerNotification.PromptsListChanged

  it should "fall back to Unknown for unrecognised methods" in:
    val n: JSONRPCMessage.Notification = JSONRPCMessage.Notification(method = "notifications/unknown", params = Some(Json.obj()))
    ServerNotification.parse(n) shouldBe ServerNotification.Unknown("notifications/unknown", Some(Json.obj()))
