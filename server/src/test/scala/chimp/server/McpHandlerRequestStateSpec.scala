package chimp.server

import chimp.protocol.*
import io.circe.*
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.model.StatusCode
import sttp.monad.{IdentityMonad, MonadError}
import sttp.shared.Identity
import sttp.tapir.Schema

import java.time.{Clock, Instant, ZoneId, ZoneOffset}
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import scala.concurrent.duration.*

class McpHandlerRequestStateSpec extends AnyFlatSpec with Matchers:
  import JSONRPCMessage.*

  given MonadError[Identity] = IdentityMonad

  case class CountInput(count: Int) derives Codec, Schema

  private final class SettableClock(millis: AtomicLong) extends Clock:
    def getZone: ZoneId = ZoneOffset.UTC
    override def withZone(zone: ZoneId): Clock = this
    def instant(): Instant = Instant.ofEpochMilli(millis.get())

  private val now = AtomicLong(1_000_000L)
  private val signer = RequestStateSigner(Array.fill[Byte](32)(7), ttl = 1.minute, clock = SettableClock(now))
  private val runs = AtomicInteger(0)

  private def stateful(name: String) = tool(name)
    .input[CountInput]
    .serverLogicWithContext[Identity]: (in, ctx, _) =>
      runs.incrementAndGet()
      ctx.request.requestState match
        case Some(state) => ToolResult.text(s"state $state")
        case None        => InputRequired.withRequestState(s"count=${in.count}")

  private val handler = McpHandler(McpServer[Identity]().addTools(stateful("a"), stateful("b")).withRequestStateSigner(signer))

  private val securedHandler =
    val secured = McpServer[Identity]()
      .serverSecurityLogicPure[Unit, Unit, String](sttp.tapir.emptyInput, sttp.tapir.emptyOutput)(_ => Right("ignored"))
      .addTool(stateful("a"))
      .withRequestStateSigner(signer, principal => principal)
    McpHandler[Identity, SecuredServerContext[Identity, String]](secured)

  private def request(tool: String, count: Int, state: Option[String]): Json =
    val meta = Json.obj(ProtocolMeta.ProtocolVersionKey -> "2026-07-28".asJson, ProtocolMeta.ClientCapabilities -> Json.obj())
    val params = Json.obj("name" -> tool.asJson, "arguments" -> Json.obj("count" -> count.asJson), "_meta" -> meta)
    val message: JSONRPCMessage =
      Request(
        method = "tools/call",
        params = Some(params.deepMerge(Json.obj("requestState" -> state.asJson)).dropNullValues),
        id = RequestId("r")
      )
    message.asJson

  private def call(tool: String, count: Int = 1, state: Option[String] = None): McpResponse =
    handler.handleJsonRpc(request(tool, count, state), Seq.empty)

  private def callAs(principal: String, state: Option[String] = None): McpResponse =
    securedHandler.handleJsonRpc(request("a", 1, state), Seq.empty, info => SecuredServerContext[Identity, String](principal, info))

  private def result(response: McpResponse): Json =
    response.body.flatMap(_.hcursor.downField("result").focus).getOrElse(fail(s"Expected a result, got ${response.body}"))

  private def stateOf(response: McpResponse): String = result(response).hcursor.get[String]("requestState").getOrElse(fail("No state"))

  private def textOf(response: McpResponse): Option[String] =
    result(response).hcursor.downField("content").downArray.get[String]("text").toOption

  private def shouldBeRejected(call: => McpResponse): org.scalatest.Assertion =
    val runsBefore = runs.get()
    val response = call
    response.statusCode shouldBe StatusCode.BadRequest
    response.body.flatMap(_.hcursor.downField("error").get[Int]("code").toOption) shouldBe Some(JSONRPCErrorCodes.InvalidParams.code)
    runs.get() shouldBe runsBefore

  "A request state signer" should "sign the state and give the handler the original state on the retry" in:
    val signed = stateOf(call("a", count = 3))
    signed should not include "count=3"
    textOf(call("a", count = 3, state = Some(signed))) shouldBe Some("state count=3")

  it should "reject a changed state before the handler runs" in:
    val signed = stateOf(call("a"))
    shouldBeRejected(call("a", state = Some(signed + "-TAMPERED")))
    shouldBeRejected(call("a", state = Some("x" + signed.drop(1))))
    shouldBeRejected(call("a", state = Some("count=1")))

  it should "reject an expired state" in:
    val signed = stateOf(call("a"))
    now.addAndGet(2.minutes.toMillis)
    shouldBeRejected(call("a", state = Some(signed)))

  it should "reject a state of another tool, or of other arguments" in:
    val signed = stateOf(call("a", count = 1))
    shouldBeRejected(call("b", count = 1, state = Some(signed)))
    shouldBeRejected(call("a", count = 2, state = Some(signed)))

  it should "reject a state of another principal on a secured server" in:
    val signed = stateOf(callAs("alice"))
    textOf(callAs("alice", Some(signed))) shouldBe Some("state count=1")
    shouldBeRejected(callAs("bob", Some(signed)))

  it should "need a secret of at least 32 bytes" in:
    an[IllegalArgumentException] should be thrownBy RequestStateSigner(Array.fill[Byte](31)(1))
