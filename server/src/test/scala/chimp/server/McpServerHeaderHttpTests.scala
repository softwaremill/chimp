package chimp.server

import chimp.protocol.*
import io.circe.syntax.*
import io.circe.{parser, Codec, Json}
import org.scalatest.Assertion
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.tapir.Schema

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import scala.concurrent.Future

/** Standard and `x-mcp-header` custom request headers of 2026-07-28 requests, on the plain and the streaming HTTP transports. */
trait McpServerHeaderHttpTests[F[_]] extends AsyncFlatSpec with Matchers:
  this: ToFuture[F] =>

  /** Serves `server` on a plain HTTP endpoint and runs the blocking `test` with the endpoint's URI. */
  protected def withHttpEndpoint(server: McpServer[F])(test: URI => Assertion): Future[Assertion]

  /** Serves `server` on a streaming HTTP endpoint and runs the blocking `test` with the endpoint's URI. */
  protected def withStreamingHttpEndpoint(server: StreamingMcpServer[F])(test: URI => Assertion): Future[Assertion]

  private case class RegionInput(region: String, limit: Option[Int]) derives Codec, Schema

  private val headerServer: McpServer[F] =
    McpServer[F]().addTool(
      tool("region")
        .input[RegionInput]
        .withMcpHeader("region", "Region")
        .withMcpHeader("limit", "Limit")
        .serverLogic[F]((in, _) => monad.unit(ToolResult.text(s"region ${in.region}")))
    )

  private val httpClient = HttpClient.newBuilder().version(HttpClient.Version.HTTP_1_1).build()

  private def post(uri: URI, method: String, params: Json, headers: (String, String)*): HttpResponse[String] =
    val meta = Json.obj(ProtocolMeta.ProtocolVersionKey -> "2026-07-28".asJson, ProtocolMeta.ClientCapabilities -> Json.obj())
    val body = Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id" -> 1.asJson,
      "method" -> method.asJson,
      "params" -> params.deepMerge(Json.obj("_meta" -> meta))
    )
    val request = headers
      .foldLeft(HttpRequest.newBuilder(uri))((builder, header) => builder.header(header._1, header._2))
      .header("Content-Type", "application/json")
      .header("Accept", "application/json, text/event-stream")
      .header(ProtocolMeta.ProtocolVersionHeader, "2026-07-28")
      .header("Mcp-Method", method)
      .POST(HttpRequest.BodyPublishers.ofString(body.noSpaces))
      .build()
    httpClient.send(request, HttpResponse.BodyHandlers.ofString())

  private def callRegion(uri: URI, arguments: Json, headers: (String, String)*): HttpResponse[String] =
    post(uri, "tools/call", Json.obj("name" -> "region".asJson, "arguments" -> arguments), ("Mcp-Name" -> "region") +: headers*)

  private def shouldBeServed(response: HttpResponse[String], text: String): Assertion =
    response.statusCode() shouldBe 200
    response.body() should include(text)
    response.body() should not include "\"error\""

  private def shouldBeHeaderMismatch(response: HttpResponse[String]): Assertion =
    response.statusCode() shouldBe 400
    response.headers().firstValue("Content-Type").orElse("") should startWith("application/json")
    parser.parse(response.body()).flatMap(_.hcursor.downField("error").get[Int]("code")) shouldBe Right(
      JSONRPCErrorCodes.HeaderMismatch.code
    )

  private def headerTests(transport: String, serve: McpServer[F] => (URI => Assertion) => Future[Assertion]): Unit =
    it should s"list the x-mcp-header annotations in the input schema ($transport)" in:
      serve(headerServer): uri =>
        val response = post(uri, "tools/list", Json.obj())
        response.statusCode() shouldBe 200
        response.body() should include("\"x-mcp-header\":\"Region\"")

    it should s"serve a tools/call whose Mcp-Param headers match the arguments, decoding Base64 ($transport)" in:
      serve(headerServer): uri =>
        val arguments = Json.obj("region" -> "Hello".asJson, "limit" -> 42.asJson)
        shouldBeServed(callRegion(uri, arguments, "Mcp-Param-Region" -> "=?base64?SGVsbG8=?=", "mcp-param-limit" -> "42.0"), "region Hello")

    it should s"reject a tools/call with a missing, malformed or mismatched Mcp-Param header with -32020 and HTTP 400 ($transport)" in:
      serve(headerServer): uri =>
        val arguments = Json.obj("region" -> "Hello".asJson)
        shouldBeHeaderMismatch(callRegion(uri, arguments))
        shouldBeHeaderMismatch(callRegion(uri, arguments, "Mcp-Param-Region" -> "=?base64?SGVsbG8?="))
        shouldBeHeaderMismatch(callRegion(uri, arguments, "Mcp-Param-Region" -> "Bye"))

    it should s"reject a tools/call whose Base64 Mcp-Name does not match the body with -32020 and HTTP 400 ($transport)" in:
      serve(headerServer): uri =>
        val params = Json.obj("name" -> "region".asJson, "arguments" -> Json.obj("region" -> "eu".asJson))
        shouldBeServed(post(uri, "tools/call", params, "Mcp-Name" -> "=?base64?cmVnaW9u?=", "Mcp-Param-Region" -> "eu"), "region eu")
        shouldBeHeaderMismatch(post(uri, "tools/call", params, "Mcp-Name" -> "=?base64?b3RoZXI=?=", "Mcp-Param-Region" -> "eu"))

  behavior of "an HTTP server checking request headers"
  headerTests("plain", server => withHttpEndpoint(server))
  headerTests("streaming", server => withStreamingHttpEndpoint(server.streaming))
