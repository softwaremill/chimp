package chimp.server

import chimp.protocol.ProtocolMeta
import io.circe.syntax.*
import io.circe.{parser, Codec, Json}
import org.scalatest.Assertion
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.tapir.Schema

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** `subscriptions/listen` over Streamable HTTP. Uses a plain HTTP client, because chimp's client does not listen yet. */
trait McpServerSubscriptionHttpTests[F[_]] extends AsyncFlatSpec with Matchers:
  this: ToFuture[F] =>

  /** Serves `server` on a streaming HTTP endpoint and runs the blocking `test` with the endpoint's URI. */
  protected def withStreamingHttpEndpoint(server: StreamingMcpServer[F])(test: URI => Assertion): Future[Assertion]

  private case class NoInput() derives Codec, Schema

  private def listeningServer(publisher: NotificationPublisher): StreamingMcpServer[F] =
    StreamingMcpServer[F]()
      .addTool(tool("noop").input[NoInput].serverLogic[F]((_, _) => monad.unit(ToolResult.text("ok"))))
      .addResource(resource("test://a").name("a").serverLogic[F](_ => monad.unit(Right(Nil))))
      .withPublisher(publisher)

  private val httpClient = HttpClient.newBuilder().version(HttpClient.Version.HTTP_1_1).build()

  private final class SseStream(response: HttpResponse[java.util.stream.Stream[String]]):
    private val messages = LinkedBlockingQueue[Option[Json]]()
    private val reader = Thread: () =>
      try
        response
          .body()
          .iterator()
          .asScala
          .collect { case s"data:$data" => parser.parse(data.trim).toOption }
          .flatten
          .foreach(json => messages.put(Some(json)))
      catch case _: Exception => ()
      finally messages.put(None)
    reader.setDaemon(true)
    reader.start()

    def status: Int = response.statusCode()

    /** The next message, or `None` when the stream ended. */
    def next(): Option[Json] = Option(messages.poll(10, TimeUnit.SECONDS)).getOrElse(fail("No message on the stream within 10 seconds"))

    def close(): Unit = Try(response.body().close()).getOrElse(())

  private def listen(uri: URI, id: Json, filter: Json): SseStream =
    val meta = Json.obj(ProtocolMeta.ProtocolVersionKey -> "2026-07-28".asJson, ProtocolMeta.ClientCapabilities -> Json.obj())
    val body = Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id" -> id,
      "method" -> "subscriptions/listen".asJson,
      "params" -> Json.obj("notifications" -> filter, "_meta" -> meta)
    )
    val request = HttpRequest
      .newBuilder(uri)
      .header("Content-Type", "application/json")
      .header("Accept", "application/json, text/event-stream")
      .header(ProtocolMeta.ProtocolVersionHeader, "2026-07-28")
      .header("Mcp-Method", "subscriptions/listen")
      .POST(HttpRequest.BodyPublishers.ofString(body.noSpaces))
      .build()
    SseStream(httpClient.send(request, HttpResponse.BodyHandlers.ofLines()))

  private def method(message: Option[Json]): Option[String] = message.flatMap(_.hcursor.get[String]("method").toOption)

  private def subscriptionId(message: Option[Json]): Option[Json] =
    message.flatMap(_.hcursor.downField("params").downField("_meta").downField(ProtocolMeta.SubscriptionId).focus)

  private def eventually(condition: => Boolean): Boolean =
    Iterator.continually { Thread.sleep(20); condition }.take(250).exists(identity)

  "a streaming HTTP server" should "acknowledge a subscription first and send it only the requested notifications" in:
    val publisher = NotificationPublisher()
    withStreamingHttpEndpoint(listeningServer(publisher)): uri =>
      val filter = Json.obj(
        "toolsListChanged" -> true.asJson,
        "promptsListChanged" -> true.asJson,
        "resourceSubscriptions" -> List("test://a").asJson
      )
      val stream = listen(uri, 11.asJson, filter)
      try
        stream.status shouldBe 200
        val ack = stream.next()
        method(ack) shouldBe Some("notifications/subscriptions/acknowledged")
        ack.flatMap(_.hcursor.downField("params").downField("notifications").focus) shouldBe
          Some(Json.obj("resourceSubscriptions" -> List("test://a").asJson, "toolsListChanged" -> true.asJson))
        publisher.promptsListChanged()
        publisher.resourcesListChanged()
        publisher.resourceUpdated("test://b")
        publisher.resourceUpdated("test://a")
        publisher.toolsListChanged()
        val updated = stream.next()
        val toolsChanged = stream.next()
        method(updated) shouldBe Some("notifications/resources/updated")
        updated.flatMap(_.hcursor.downField("params").get[String]("uri").toOption) shouldBe Some("test://a")
        method(toolsChanged) shouldBe Some("notifications/tools/list_changed")
        List(ack, updated, toolsChanged).map(subscriptionId) shouldBe List.fill(3)(Some(11.asJson))
      finally stream.close()

  it should "stop publishing to a subscription when the client closes its stream" in:
    val publisher = NotificationPublisher()
    withStreamingHttpEndpoint(listeningServer(publisher)): uri =>
      val stream = listen(uri, 1.asJson, Json.obj("toolsListChanged" -> true.asJson))
      method(stream.next()) shouldBe Some("notifications/subscriptions/acknowledged")
      publisher.openSubscriptions shouldBe 1
      stream.close()
      eventually { publisher.toolsListChanged(); publisher.openSubscriptions == 0 } shouldBe true

  it should "end every subscription with a successful listen result when the publisher closes" in:
    val publisher = NotificationPublisher()
    withStreamingHttpEndpoint(listeningServer(publisher)): uri =>
      val first = listen(uri, 1.asJson, Json.obj("toolsListChanged" -> true.asJson))
      val second = listen(uri, "second".asJson, Json.obj("resourcesListChanged" -> true.asJson))
      try
        List(first.next(), second.next()).map(subscriptionId) shouldBe List(Some(1.asJson), Some("second".asJson))
        publisher.close()
        val results = List(first.next(), second.next())
        results.map(_.flatMap(_.hcursor.downField("id").focus)) shouldBe List(Some(1.asJson), Some("second".asJson))
        results.map(_.flatMap(_.hcursor.downField("result").downField("_meta").downField(ProtocolMeta.SubscriptionId).focus)) shouldBe
          List(Some(1.asJson), Some("second".asJson))
        List(first.next(), second.next()) shouldBe List(None, None)
      finally
        first.close()
        second.close()
