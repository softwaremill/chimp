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

class McpHandlerInputRequiredSpec extends AnyFlatSpec with Matchers:
  import JSONRPCMessage.*

  given MonadError[Identity] = IdentityMonad

  case class NoInput() derives Schema, Codec

  private val nameSchema = Json.obj("type" -> "object".asJson, "properties" -> Json.obj("name" -> Json.obj("type" -> "string".asJson)))

  private def name(ctx: ServerContext[Identity], key: String): Option[String] =
    ctx.request.elicitResult(key).flatMap(_.content).flatMap(_.get("name")).flatMap(_.asString)

  private val greetTool = tool("greet")
    .input[NoInput]
    .serverLogicWithContext[Identity]: (_, ctx, _) =>
      name(ctx, "user_name") match
        case Some(name) => ToolResult.text(s"Hello, $name!")
        case None       => InputRequired("user_name" -> InputRequired.elicitForm("What is your name?", nameSchema))

  private val twoRoundsTool = tool("two_rounds")
    .input[NoInput]
    .serverLogicWithContext[Identity]: (_, ctx, _) =>
      (ctx.request.requestState, name(ctx, "first"), name(ctx, "second")) match
        case (Some("round-2"), _, Some(second)) => ToolResult.text(s"done: $second")
        case (Some("round-1"), Some(_), _)      =>
          InputRequired("second" -> InputRequired.elicitForm("Second?", nameSchema)).withRequestState("round-2")
        case _ => InputRequired("first" -> InputRequired.elicitForm("First?", nameSchema)).withRequestState("round-1")

  private val samplingParams = CreateMessageParams(List(SamplingMessage(Role.User, ToolContent.Text(text = "Hi?"))), maxTokens = 10)

  private val askAllTool = tool("ask_all")
    .input[NoInput]
    .handle: _ =>
      InputRequired(
        "form" -> InputRequired.elicitForm("Name?", nameSchema),
        "url" -> InputRequired.elicitUrl("Sign in", "https://example.com/login"),
        "sample" -> InputRequired.sample(samplingParams),
        "roots" -> InputRequired.listRoots
      )

  private val adaptiveTool = tool("adaptive")
    .input[NoInput]
    .serverLogicWithContext[Identity]: (_, ctx, _) =>
      if ctx.request.clientCapabilities.exists(_.sampling.isDefined) then InputRequired("q" -> InputRequired.sample(samplingParams))
      else InputRequired("q" -> InputRequired.elicitForm("Name?", nameSchema))

  private val stateOnlyTool = tool("state_only")
    .input[NoInput]
    .serverLogicWithContext[Identity]: (_, ctx, _) =>
      ctx.request.requestState.fold(InputRequired.withRequestState("again"))(state => ToolResult.text(s"state $state"))

  private val greetPrompt = prompt("greet")
    .serverLogicWithContext[Identity]: (_, ctx, _) =>
      name(ctx, "user_name") match
        case Some(name) => GetPromptResult(messages = List(PromptMessage(Role.User, ToolContent.Text(text = s"Hello, $name"))))
        case None       => InputRequired("user_name" -> InputRequired.elicitForm("Name?", nameSchema))

  private val rootsResource = resource("test://roots")
    .serverLogicWithContext[Identity]: (ctx, _) =>
      ctx.request.rootsResult("roots") match
        case Some(roots) => Right(List(ResourceContents.Text("test://roots", roots.roots.map(_.uri).mkString(","))))
        case None        => InputRequired("roots" -> InputRequired.listRoots)

  private val rootsTemplate = resourceTemplate("test://roots/{id}")
    .serverLogicWithContext[Identity]: (_, uri, ctx, _) =>
      ctx.request.rootsResult("roots") match
        case Some(roots) => Right(List(ResourceContents.Text(uri, roots.roots.map(_.uri).mkString(","))))
        case None        => InputRequired("roots" -> InputRequired.listRoots)

  private val handler = McpHandler(
    McpServer[Identity]()
      .addTools(greetTool, twoRoundsTool, askAllTool, adaptiveTool, stateOnlyTool)
      .addPrompt(greetPrompt)
      .addResource(rootsResource)
      .addResourceTemplate(rootsTemplate)
  )

  private val allCapabilities =
    Json.obj("sampling" -> Json.obj(), "roots" -> Json.obj(), "elicitation" -> Json.obj("form" -> Json.obj(), "url" -> Json.obj()))

  private def call(method: String, params: Json, capabilities: Json = allCapabilities, modern: Boolean = true): McpResponse =
    val meta =
      if modern then Json.obj(ProtocolMeta.ProtocolVersionKey -> "2026-07-28".asJson, ProtocolMeta.ClientCapabilities -> capabilities)
      else Json.obj()
    val request: JSONRPCMessage = Request(method = method, params = Some(params.deepMerge(Json.obj("_meta" -> meta))), id = RequestId("r"))
    handler.handleJsonRpc(request.asJson, Seq.empty)

  private def callTool(tool: String, extra: (String, Json)*): McpResponse =
    call("tools/call", Json.obj(("name" -> tool.asJson) +: ("arguments" -> Json.obj()) +: extra*))

  private def bodyOf(response: McpResponse): Json = response.body.getOrElse(fail("Expected a body"))

  private def resultOf(response: McpResponse): Json =
    bodyOf(response).as[JSONRPCMessage].getOrElse(fail("decode")) match
      case Response(_, _, result) => result
      case other                  => fail(s"Expected Response, got $other")

  private def errorOf(response: McpResponse): JSONRPCErrorObject =
    bodyOf(response).as[JSONRPCMessage].getOrElse(fail("decode")) match
      case Error(_, _, error) => error
      case other              => fail(s"Expected Error, got $other")

  private def nameAnswer(value: String): Json = ElicitResult(ElicitAction.Accept, Some(Map("name" -> value.asJson))).asJson

  private def textOf(result: Json): Option[String] = result.hcursor.downField("content").downArray.get[String]("text").toOption

  "An input-required outcome" should "reach a modern client as resultType input_required with its keyed input requests" in:
    val response = callTool("greet")
    response.statusCode shouldBe StatusCode.Ok
    val result = resultOf(response)
    result.hcursor.get[String]("resultType") shouldBe Right("input_required")
    result.hcursor.downField("inputRequests").downField("user_name").get[String]("method") shouldBe Right("elicitation/create")
    result.hcursor.downField("_meta").downField(ProtocolMeta.ServerInfo).succeeded shouldBe true
    result.hcursor.downField("requestState").succeeded shouldBe false
    result.hcursor.downField("ttlMs").succeeded shouldBe false

  it should "let the handler read the answer on the retry, and ignore keys it did not ask for" in:
    val responses = Json.obj("user_name" -> nameAnswer("Alice"), "unexpected" -> nameAnswer("Mallory"))
    val result = resultOf(callTool("greet", "inputResponses" -> responses))
    result.hcursor.get[String]("resultType") shouldBe Right("complete")
    textOf(result) shouldBe Some("Hello, Alice!")

  it should "let the handler ask again when the retry lacks an answer it needs" in:
    val result = resultOf(callTool("greet", "inputResponses" -> Json.obj("wrong_key" -> nameAnswer("Alice"))))
    result.hcursor.get[String]("resultType") shouldBe Right("input_required")

  it should "carry the request state across several rounds" in:
    val first = resultOf(callTool("two_rounds"))
    first.hcursor.get[String]("requestState") shouldBe Right("round-1")
    val second =
      resultOf(callTool("two_rounds", "inputResponses" -> Json.obj("first" -> nameAnswer("a")), "requestState" -> "round-1".asJson))
    second.hcursor.get[String]("requestState") shouldBe Right("round-2")
    second.hcursor.downField("inputRequests").keys.map(_.toList) shouldBe Some(List("second"))
    val third =
      resultOf(callTool("two_rounds", "inputResponses" -> Json.obj("second" -> nameAnswer("b")), "requestState" -> "round-2".asJson))
    textOf(third) shouldBe Some("done: b")

  it should "carry only a request state when the handler asks for no input" in:
    val result = resultOf(callTool("state_only"))
    result.hcursor.get[String]("requestState") shouldBe Right("again")
    result.hcursor.downField("inputRequests").succeeded shouldBe false
    textOf(resultOf(callTool("state_only", "requestState" -> "again".asJson))) shouldBe Some("state again")

  it should "encode form, URL, sampling and roots requests as the schema defines them" in:
    val requests = resultOf(callTool("ask_all")).hcursor.downField("inputRequests")
    requests.downField("form").downField("params").get[String]("message") shouldBe Right("Name?")
    requests.downField("url").downField("params").get[String]("mode") shouldBe Right("url")
    requests.downField("sample").get[String]("method") shouldBe Right("sampling/createMessage")
    requests.downField("roots").get[String]("method") shouldBe Right("roots/list")

  it should "let the handler choose what to ask for from the client capabilities" in:
    def asked(capabilities: Json) =
      call("tools/call", Json.obj("name" -> "adaptive".asJson), capabilities).body.flatMap(
        _.hcursor.downField("result").downField("inputRequests").downField("q").get[String]("method").toOption
      )
    asked(Json.obj("sampling" -> Json.obj())) shouldBe Some("sampling/createMessage")
    asked(Json.obj("elicitation" -> Json.obj())) shouldBe Some("elicitation/create")

  it should "be possible for prompts, resources and resource templates" in:
    resultOf(call("prompts/get", Json.obj("name" -> "greet".asJson))).hcursor.get[String]("resultType") shouldBe Right("input_required")
    for uri <- List("test://roots", "test://roots/1") do
      withClue(uri):
        val first = resultOf(call("resources/read", Json.obj("uri" -> uri.asJson)))
        first.hcursor.get[String]("resultType") shouldBe Right("input_required")
        first.hcursor.downField("ttlMs").succeeded shouldBe false
        val roots = ListRootsResult(List(Root("file:///a"))).asJson
        val retry = call("resources/read", Json.obj("uri" -> uri.asJson, "inputResponses" -> Json.obj("roots" -> roots)))
        resultOf(retry).hcursor.downField("contents").downArray.get[String]("text") shouldBe Right("file:///a")
    val promptRetry = call("prompts/get", Json.obj("name" -> "greet".asJson, "inputResponses" -> Json.obj("user_name" -> nameAnswer("Bo"))))
    resultOf(promptRetry).hcursor.downField("messages").downArray.downField("content").get[String]("text") shouldBe Right("Hello, Bo")

  it should "fail with -32021, the missing capabilities and HTTP 400 when the client did not declare what an input request needs" in:
    val cases = List(
      Json.obj() -> Json.obj(
        "sampling" -> Json.obj(),
        "roots" -> Json.obj(),
        "elicitation" -> Json.obj("form" -> Json.obj(), "url" -> Json.obj())
      ),
      Json.obj("sampling" -> Json.obj(), "roots" -> Json.obj(), "elicitation" -> Json.obj()) -> Json.obj(
        "elicitation" -> Json.obj("url" -> Json.obj())
      ),
      Json.obj("sampling" -> Json.obj(), "roots" -> Json.obj(), "elicitation" -> Json.obj("url" -> Json.obj())) -> Json.obj(
        "elicitation" -> Json.obj("form" -> Json.obj())
      )
    )
    for (declared, missing) <- cases do
      withClue(declared.noSpaces):
        val response = call("tools/call", Json.obj("name" -> "ask_all".asJson), declared)
        response.statusCode shouldBe StatusCode.BadRequest
        val error = errorOf(response)
        error.code shouldBe JSONRPCErrorCodes.MissingRequiredClientCapability.code
        error.data.flatMap(_.hcursor.downField("requiredCapabilities").focus) shouldBe Some(missing)

  it should "fail with -32602 and HTTP 400 when inputResponses or requestState is malformed, before the handler runs" in:
    val malformed = List(
      "inputResponses" -> 12345.asJson,
      "inputResponses" -> Json.Null,
      "inputResponses" -> Json.arr(),
      "inputResponses" -> Json.obj("user_name" -> 12345.asJson),
      "inputResponses" -> Json.obj("user_name" -> Json.obj("action" -> "maybe".asJson)),
      "requestState" -> 1.asJson
    )
    for field <- malformed do
      withClue(field):
        val response = callTool("greet", field)
        response.statusCode shouldBe StatusCode.BadRequest
        errorOf(response).code shouldBe JSONRPCErrorCodes.InvalidParams.code

  it should "become an internal error naming the required protocol version on a legacy request" in:
    val response = call("tools/call", Json.obj("name" -> "greet".asJson, "arguments" -> Json.obj()), modern = false)
    response.statusCode shouldBe StatusCode.Ok
    val error = errorOf(response)
    error.code shouldBe JSONRPCErrorCodes.InternalError.code
    error.message should include("2026-07-28")
