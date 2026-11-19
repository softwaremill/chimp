package chimp.server

import chimp.protocol.*
import io.circe.syntax.*
import io.circe.{parser, Codec, Json}
import org.scalatest.Assertion
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.monad.syntax.*
import sttp.tapir.Schema

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import scala.concurrent.Future

/** Input-required outcomes over Streamable HTTP, where an error with its own HTTP status must not hide in a 200 event stream. */
trait McpServerInputRequiredHttpTests[F[_]] extends AsyncFlatSpec with Matchers:
  this: ToFuture[F] =>

  /** Serves `server` on a streaming HTTP endpoint and runs the blocking `test` with the endpoint's URI. */
  protected def withStreamingHttpEndpoint(server: StreamingMcpServer[F])(test: URI => Assertion): Future[Assertion]

  private case class NoInput() derives Codec, Schema

  private val sampling = InputRequired.sample(
    CreateMessageParams(List(SamplingMessage(Role.User, ToolContent.Text(text = "Capital of France?"))), maxTokens = 10)
  )

  private val inputRequiredServer: StreamingMcpServer[F] =
    StreamingMcpServer[F]()
      .addTool(
        tool("ask")
          .input[NoInput]
          .serverLogicWithContext[F]((_, ctx, _) =>
            monad.unit(ctx.request.samplingResult("q").fold(InputRequired("q" -> sampling))(_ => ToolResult.text("answered")))
          )
      )
      .addStreamingTool(
        tool("progress_then_ask")
          .input[NoInput]
          .streamingServerLogic[F]((_, ctx, _) => ctx.reportProgress(50, Some(100)).map(_ => InputRequired("q" -> sampling)))
      )

  private val httpClient = HttpClient.newBuilder().version(HttpClient.Version.HTTP_1_1).build()

  private def callTool(uri: URI, name: String, capabilities: Json): HttpResponse[String] =
    val meta = Json.obj(
      ProtocolMeta.ProtocolVersionKey -> "2026-07-28".asJson,
      ProtocolMeta.ClientCapabilities -> capabilities,
      "progressToken" -> "p".asJson
    )
    val body = Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id" -> 1.asJson,
      "method" -> "tools/call".asJson,
      "params" -> Json.obj("name" -> name.asJson, "arguments" -> Json.obj(), "_meta" -> meta)
    )
    val request = HttpRequest
      .newBuilder(uri)
      .header("Content-Type", "application/json")
      .header("Accept", "application/json, text/event-stream")
      .header(ProtocolMeta.ProtocolVersionHeader, "2026-07-28")
      .header("Mcp-Method", "tools/call")
      .header("Mcp-Name", name)
      .POST(HttpRequest.BodyPublishers.ofString(body.noSpaces))
      .build()
    httpClient.send(request, HttpResponse.BodyHandlers.ofString())

  private def contentType(response: HttpResponse[String]): String = response.headers().firstValue("Content-Type").orElse("")

  private def events(response: HttpResponse[String]): List[Json] =
    response.body().linesIterator.collect { case s"data:$data" => parser.parse(data.trim).toOption }.flatten.toList

  "a streaming HTTP server" should "send an input-required result on the event stream" in:
    withStreamingHttpEndpoint(inputRequiredServer): uri =>
      val response = callTool(uri, "ask", Json.obj("sampling" -> Json.obj()))
      response.statusCode() shouldBe 200
      contentType(response) should include("text/event-stream")
      val result = events(response).flatMap(_.hcursor.downField("result").focus)
      result.map(_.hcursor.get[String]("resultType")) shouldBe List(Right("input_required"))
      result.map(_.hcursor.downField("inputRequests").downField("q").get[String]("method")) shouldBe List(Right("sampling/createMessage"))

  it should "answer an input request the client has no capability for with a plain JSON -32021 and HTTP 400" in:
    withStreamingHttpEndpoint(inputRequiredServer): uri =>
      val response = callTool(uri, "ask", Json.obj())
      response.statusCode() shouldBe 400
      contentType(response) should include("application/json")
      val error = parser.parse(response.body()).toOption.flatMap(_.hcursor.downField("error").focus)
      error.flatMap(_.hcursor.get[Int]("code").toOption) shouldBe Some(JSONRPCErrorCodes.MissingRequiredClientCapability.code)
      error.flatMap(_.hcursor.downField("data").downField("requiredCapabilities").focus) shouldBe Some(Json.obj("sampling" -> Json.obj()))

  it should "end an event stream which already carries a message with the -32021" in:
    withStreamingHttpEndpoint(inputRequiredServer): uri =>
      val response = callTool(uri, "progress_then_ask", Json.obj())
      response.statusCode() shouldBe 200
      val messages = events(response)
      messages.map(_.hcursor.get[String]("method").toOption) shouldBe List(Some("notifications/progress"), None)
      messages.lastOption.flatMap(_.hcursor.downField("error").get[Int]("code").toOption) shouldBe
        Some(JSONRPCErrorCodes.MissingRequiredClientCapability.code)
