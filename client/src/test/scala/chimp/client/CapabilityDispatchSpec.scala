package chimp.client

import chimp.client.capabilities.{Elicitation, Roots}
import chimp.client.notifications.{ServerNotification, ServerNotificationListener}
import chimp.protocol.*
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.shared.Identity

class CapabilityDispatchSpec extends AnyFlatSpec with Matchers:

  private val clientInfo = Implementation(name = "chimp-test", version = "0.0.1")

  it should "respond to a server-initiated roots/list with the registered handler's result" in:
    given Roots[Identity] = () => ListRootsResult(roots = List(Root(uri = "file:///x", name = Some("x"))))
    val t = InMemoryTransport()
    val _ = McpClient[Identity, Roots[Identity]](t, clientInfo)

    val req: JSONRPCMessage = JSONRPCMessage.Request(method = "roots/list", id = RequestId("server-1"))
    t.simulateIncoming(req)

    t.sent.last match
      case JSONRPCMessage.Response(_, _, result) =>
        val r = result.as[ListRootsResult].toOption.get
        r.roots.head.name shouldBe Some("x")
      case other => fail(s"Expected Response, got $other")

  it should "respond with MethodNotFound for capabilities the client didn't opt into" in:
    val t = InMemoryTransport()
    val _ = McpClient[Identity, Any](t, clientInfo)

    val req: JSONRPCMessage = JSONRPCMessage.Request(method = "sampling/createMessage", id = RequestId("server-2"))
    t.simulateIncoming(req)

    t.sent.last match
      case JSONRPCMessage.Error(_, _, err) => err.code shouldBe JSONRPCErrorCodes.MethodNotFound.code
      case other                            => fail(s"Expected Error, got $other")

  it should "deliver incoming notifications to registered listeners" in:
    val t = InMemoryTransport()
    val client = McpClient[Identity, Any](t, clientInfo)
    var received: Option[ServerNotification] = None
    val listener: ServerNotificationListener[Identity] = n => { received = Some(ServerNotification.parse(n)); () }
    client.onServerNotification(listener)

    val params = ProgressParams(progressToken = ProgressToken("p1"), progress = 0.42)
    val notif: JSONRPCMessage = JSONRPCMessage.Notification(method = "notifications/progress", params = Some(params.asJson))
    t.simulateIncoming(notif)

    received shouldBe Some(ServerNotification.Progress(params))

  it should "include opted-in capabilities on initialize" in:
    given Roots[Identity] = () => ListRootsResult(roots = Nil)
    given Elicitation[Identity] = (_: ElicitRequest) => ElicitResult(action = ElicitAction.Cancel)
    val t = InMemoryTransport()
    val initResult = InitializeResult(
      protocolVersion = ProtocolVersion.Latest,
      capabilities = ServerCapabilities(),
      serverInfo = Implementation(name = "s", version = "1")
    )
    t.planResponse(JSONRPCMessage.Response(id = RequestId(1), result = initResult.asJson))
    val client = McpClient[Identity, Roots[Identity] & Elicitation[Identity]](t, clientInfo)
    client.initialize()

    t.sent.head match
      case r: JSONRPCMessage.Request =>
        val params = r.params.get.as[InitializeParams].toOption.get
        params.capabilities.roots.isDefined shouldBe true
        params.capabilities.elicitation.isDefined shouldBe true
        params.capabilities.sampling shouldBe None
      case other => fail(s"Expected Request, got $other")
