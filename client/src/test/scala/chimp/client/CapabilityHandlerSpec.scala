package chimp.client

import chimp.client.notifications.{ServerNotification, ServerNotificationListener}
import chimp.protocol.*
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.shared.Identity

class CapabilityHandlerSpec extends AnyFlatSpec with Matchers:

  private val clientInfo = Implementation(name = "chimp-test", version = "0.0.1")

  it should "respond to a server-initiated roots/list with the registered handler's result" in:
    val transport = InMemoryTransport()
    val _ = McpClient[Identity](
      transport,
      clientInfo,
      rootsHandler = Some(() => ListRootsResult(roots = List(Root(uri = "file:///x", name = Some("x")))))
    )

    val request: JSONRPCMessage = JSONRPCMessage.Request(method = "roots/list", id = RequestId("server-1"))
    transport.simulateIncoming(request)

    transport.sent.last match
      case JSONRPCMessage.Response(_, _, result) =>
        result.as[ListRootsResult].toOption match
          case Some(result) =>
            result.roots.head.name shouldBe Some("x")
          case _ => fail("Expected result")
      case other => fail(s"Expected Response, got $other")

  it should "respond with MethodNotFound for capabilities the client didn't opt into" in:
    val transport = InMemoryTransport()
    val _ = McpClient[Identity](transport, clientInfo)

    val request: JSONRPCMessage = JSONRPCMessage.Request(method = "sampling/createMessage", id = RequestId("server-2"))
    transport.simulateIncoming(request)

    transport.sent.last match
      case JSONRPCMessage.Error(_, _, err) => err.code shouldBe JSONRPCErrorCodes.MethodNotFound.code
      case other                           => fail(s"Expected Error, got $other")

  it should "deliver incoming notifications to registered listeners" in:
    val transport = InMemoryTransport()
    val client = McpClient[Identity](transport, clientInfo)
    var received: Option[ServerNotification] = None
    val listener: ServerNotificationListener[Identity] = n => { received = Some(n); () }
    val _ = client.onServerNotification(listener)

    val params = ProgressParams(progressToken = ProgressToken("p1"), progress = 0.42)
    val notification: JSONRPCMessage = JSONRPCMessage.Notification(method = "notifications/progress", params = Some(params.asJson))
    transport.simulateIncoming(notification)

    received shouldBe Some(ServerNotification.Progress(params))

  it should "include opted-in capabilities on initialize" in:
    val transport = InMemoryTransport()
    val initResult = InitializeResult(
      protocolVersion = ProtocolVersion.Latest.name,
      capabilities = ServerCapabilities(),
      serverInfo = Implementation(name = "s", version = "1")
    )
    transport.planResponse(JSONRPCMessage.Response(id = RequestId(1), result = initResult.asJson))
    val client = McpClient[Identity](
      transport,
      clientInfo,
      rootsHandler = Some(() => ListRootsResult(roots = Nil)),
      elicitationHandler = Some((_: ElicitRequest) => ElicitResult(action = ElicitAction.Cancel))
    )
    val _ = client.initialize()

    transport.sent.head match
      case request: JSONRPCMessage.Request =>
        request.params.get.as[InitializeParams].toOption match
          case Some(params) =>
            params.capabilities.roots.isDefined shouldBe true
            params.capabilities.elicitation.isDefined shouldBe true
            params.capabilities.sampling shouldBe None
          case _ => fail("Expected params")
      case other => fail(s"Expected Request, got $other")
