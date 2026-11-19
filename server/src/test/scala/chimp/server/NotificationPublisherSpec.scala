package chimp.server

import chimp.protocol.*
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.model.StatusCode
import sttp.monad.{IdentityMonad, MonadError}
import sttp.shared.Identity
import sttp.tapir.Schema

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicReference
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*

class NotificationPublisherSpec extends AnyFlatSpec with Matchers:
  private given MonadError[Identity] = IdentityMonad

  private case class NoInput() derives io.circe.Codec, Schema

  private val noop = tool("noop").input[NoInput].handle(_ => ToolResult.text("ok"))
  private val textResource = resource("test://a").name("a").handle(() => Right(Nil))
  private val greet = prompt("greet").handle(_ => GetPromptResult(messages = Nil))

  private def fullServer(publisher: NotificationPublisher): StreamingMcpServer[Identity] =
    McpServer[Identity]()
      .addTool(noop)
      .addResource(textResource)
      .addPrompt(greet)
      .withSubscriptions(ResourceSubscriptions[Identity](_ => (), _ => ()))
      .withPublisher(publisher)
      .streaming

  private val modernMeta: Json =
    Json.obj(ProtocolMeta.ProtocolVersionKey -> "2026-07-28".asJson, ProtocolMeta.ClientCapabilities -> Json.obj())

  private def request(method: String, id: RequestId, params: Json): Json =
    (JSONRPCMessage.Request(method = method, params = Some(params), id = id): JSONRPCMessage).asJson

  private def listen(id: RequestId, filter: Json): Json =
    request("subscriptions/listen", id, Json.obj("notifications" -> filter, "_meta" -> modernMeta))

  private def capabilities(handler: McpHandler[Identity, ?], method: String): ServerCapabilities =
    val params = if method == "initialize" then Json.obj("protocolVersion" -> "2025-11-25".asJson) else Json.obj()
    handler
      .handleJsonRpc(request(method, RequestId(1), params), Nil, _ => ???)
      .body
      .get
      .hcursor
      .downField("result")
      .get[ServerCapabilities]("capabilities")
      .toOption
      .get

  private class RecordingOutput extends SubscriptionOutput:
    val messages: ListBuffer[Json] = ListBuffer.empty
    var closed = false
    def send(message: Json): Unit = messages += message
    def close(): Unit = closed = true
    def methods: List[Option[String]] = messages.toList.map(_.hcursor.get[String]("method").toOption)

  private def subscriptionId(message: Json): Option[Json] =
    message.hcursor.downField("params").downField("_meta").downField(ProtocolMeta.SubscriptionId).focus

  "a server with a publisher on a streaming transport" should "advertise list changes and subscribe to modern clients only" in:
    val handler = McpHandler(fullServer(NotificationPublisher()), canListen = true)
    val modern = capabilities(handler, "server/discover")
    modern.tools.flatMap(_.listChanged) shouldBe Some(true)
    modern.prompts.flatMap(_.listChanged) shouldBe Some(true)
    modern.resources.flatMap(_.listChanged) shouldBe Some(true)
    modern.resources.flatMap(_.subscribe) shouldBe Some(true)
    val legacy = capabilities(handler, "initialize")
    legacy.tools.flatMap(_.listChanged) shouldBe Some(false)
    legacy.resources.flatMap(_.subscribe) shouldBe Some(true)

  it should "acknowledge only the notification types it can send" in:
    val handler = McpHandler(McpServer[Identity]().addTool(noop).withPublisher(NotificationPublisher()).streaming, canListen = true)
    val filter =
      Json.obj("toolsListChanged" -> true.asJson, "promptsListChanged" -> true.asJson, "resourceSubscriptions" -> List("x").asJson)
    val accepted = handler.listenRequest(listen(RequestId(7), filter), Nil)
    accepted.map(_.id) shouldBe Some(RequestId(7))
    accepted.map(_.filter) shouldBe Some(SubscriptionFilter(toolsListChanged = Some(true)))

  it should "not take a legacy subscriptions/listen request as a subscription" in:
    val handler = McpHandler(fullServer(NotificationPublisher()), canListen = true)
    val legacyListen = request("subscriptions/listen", RequestId(1), Json.obj("notifications" -> Json.obj()))
    handler.listenRequest(legacyListen, Nil) shouldBe None

  "a server with a publisher on the sync HTTP transport" should "advertise no list changes and answer subscriptions/listen with -32601" in:
    val handler = McpHandler(fullServer(NotificationPublisher()))
    val modern = capabilities(handler, "server/discover")
    modern.tools.flatMap(_.listChanged) shouldBe Some(false)
    modern.resources.flatMap(_.subscribe) shouldBe Some(false)
    val listenRequest = listen(RequestId(1), Json.obj("toolsListChanged" -> true.asJson))
    handler.listenRequest(listenRequest, Nil) shouldBe None
    val response = handler.handleJsonRpc(listenRequest, Nil, _ => ???)
    response.statusCode shouldBe StatusCode.NotFound
    response.body.flatMap(_.hcursor.downField("error").get[Int]("code").toOption) shouldBe Some(JSONRPCErrorCodes.MethodNotFound.code)

  "a subscription" should "start with the acknowledgement and carry the subscription id on every message" in:
    val publisher = NotificationPublisher()
    val output = RecordingOutput()
    val _ = publisher.open(RequestId(3), SubscriptionFilter(toolsListChanged = Some(true)), output)
    publisher.toolsListChanged()
    output.methods shouldBe List(Some("notifications/subscriptions/acknowledged"), Some("notifications/tools/list_changed"))
    output.messages.toList.map(subscriptionId) shouldBe List.fill(2)(Some(3.asJson))
    output.messages.head.hcursor.downField("params").get[SubscriptionFilter]("notifications") shouldBe
      Right(SubscriptionFilter(toolsListChanged = Some(true)))

  it should "deliver an event which the client publishes as soon as it sees the acknowledgement" in:
    val publisher = NotificationPublisher()
    val received = ConcurrentLinkedQueue[Json]()
    val client = AtomicReference(Option.empty[Thread])
    val output = new SubscriptionOutput:
      def send(message: Json): Unit =
        val _ = received.add(message)
        if client.get.isEmpty then
          val reacting = Thread(() => publisher.toolsListChanged())
          client.set(Some(reacting))
          reacting.start()
          while reacting.isAlive && reacting.getState != Thread.State.BLOCKED do Thread.onSpinWait()
      def close(): Unit = ()
    val _ = publisher.open(RequestId(1), SubscriptionFilter(toolsListChanged = Some(true)), output)
    client.get.foreach(_.join(5000))
    received.asScala.toList.map(_.hcursor.get[String]("method").toOption) shouldBe
      List(Some("notifications/subscriptions/acknowledged"), Some("notifications/tools/list_changed"))

  it should "send only the notification types and resource URIs the filter names" in:
    val publisher = NotificationPublisher()
    val output = RecordingOutput()
    val _ = publisher.open(RequestId("s"), SubscriptionFilter(resourceSubscriptions = Some(List("test://a"))), output)
    publisher.toolsListChanged()
    publisher.promptsListChanged()
    publisher.resourcesListChanged()
    publisher.resourceUpdated("test://b")
    publisher.resourceUpdated("test://a")
    output.methods.drop(1) shouldBe List(Some("notifications/resources/updated"))
    output.messages.last.hcursor.downField("params").get[String]("uri") shouldBe Right("test://a")

  it should "keep several subscriptions apart" in:
    val publisher = NotificationPublisher()
    val tools = RecordingOutput()
    val prompts = RecordingOutput()
    val _ = publisher.open(RequestId(1), SubscriptionFilter(toolsListChanged = Some(true)), tools)
    val _ = publisher.open(RequestId(2), SubscriptionFilter(promptsListChanged = Some(true), toolsListChanged = Some(true)), prompts)
    publisher.promptsListChanged()
    publisher.toolsListChanged()
    tools.methods.drop(1) shouldBe List(Some("notifications/tools/list_changed"))
    tools.messages.toList.map(subscriptionId).distinct shouldBe List(Some(1.asJson))
    prompts.methods.drop(1) shouldBe List(Some("notifications/prompts/list_changed"), Some("notifications/tools/list_changed"))
    prompts.messages.toList.map(subscriptionId).distinct shouldBe List(Some(2.asJson))

  it should "get nothing more after the client cancels it" in:
    val publisher = NotificationPublisher()
    val output = RecordingOutput()
    val subscription = publisher.open(RequestId(1), SubscriptionFilter(toolsListChanged = Some(true)), output)
    subscription.cancel()
    publisher.toolsListChanged()
    publisher.close()
    output.methods shouldBe List(Some("notifications/subscriptions/acknowledged"))
    output.closed shouldBe false
    publisher.openSubscriptions shouldBe 0

  it should "end with a successful listen result when the publisher closes" in:
    val publisher = NotificationPublisher()
    val output = RecordingOutput()
    val _ = publisher.open(RequestId(5), SubscriptionFilter(toolsListChanged = Some(true)), output)
    publisher.close()
    publisher.toolsListChanged()
    val result = output.messages.last
    result.hcursor.get[Int]("id") shouldBe Right(5)
    result.hcursor.downField("result").get[String]("resultType") shouldBe Right("complete")
    result.hcursor.downField("result").downField("_meta").downField(ProtocolMeta.SubscriptionId).focus shouldBe Some(5.asJson)
    output.messages.size shouldBe 2
    output.closed shouldBe true
    publisher.openSubscriptions shouldBe 0

  it should "end at once when it opens after the publisher closed" in:
    val publisher = NotificationPublisher()
    publisher.close()
    val output = RecordingOutput()
    val _ = publisher.open(RequestId(1), SubscriptionFilter(), output)
    output.messages.size shouldBe 2
    output.closed shouldBe true

  "stdio subscriptions" should "end on the client's notifications/cancelled, and send the result and notifications/cancelled at shutdown" in:
    val publisher = NotificationPublisher()
    val written = ListBuffer.empty[Json]
    val stdio = StdioSubscriptions(McpHandler(McpServer[Identity]()), written += _)
    val filter = SubscriptionFilter(toolsListChanged = Some(true))
    stdio.listen(ListenRequest(RequestId(1), filter, publisher))
    stdio.listen(ListenRequest(RequestId(2), filter, publisher))
    stdio.cancel(
      (JSONRPCMessage
        .Notification(method = "notifications/cancelled", params = Some(Json.obj("requestId" -> 1.asJson))): JSONRPCMessage).asJson
    )
    publisher.toolsListChanged()
    stdio.endAll()
    val tail = written.toList.drop(2)
    tail.map(subscriptionId) shouldBe List(Some(2.asJson), None, None)
    tail(1).hcursor.get[Int]("id") shouldBe Right(2)
    tail(2).hcursor.get[String]("method") shouldBe Right("notifications/cancelled")
    tail(2).hcursor.downField("params").get[Int]("requestId") shouldBe Right(2)
    publisher.openSubscriptions shouldBe 0
