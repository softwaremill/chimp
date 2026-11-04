package chimp.server.ox

import chimp.client.transport.{ClientHttpTransport, ClientTransport}
import chimp.client.transport.ox.OxClientHttpTransport
import chimp.client.{BidirectionalMcpClient, McpClient}
import chimp.protocol.{Implementation, ProtocolMeta, ProtocolVersion}
import chimp.server.transport.SecuredServerStreamingHttpTransport
import chimp.server.{
  tool,
  McpServer,
  McpServerStreamingTests,
  McpServerTests,
  SecuredMcpServer,
  SecuredMcpServerStreamingTests,
  SecuredMcpServerTests,
  SecuredStreamingMcpServer,
  StreamingMcpServer,
  SyncToFuture,
  ToolResult
}
import io.circe.syntax.*
import io.circe.{Codec, Json}
import org.scalatest.Assertion
import ox.{supervised, Ox}
import sttp.capabilities.WebSockets
import sttp.client4.{basicRequest, DefaultSyncBackend, SyncBackend}
import sttp.model.{Header, StatusCode}
import sttp.model.Uri.UriContext
import sttp.shared.Identity
import sttp.tapir.Schema
import sttp.tapir.server.netty.sync.{NettySyncServer, OxStreams}
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

class OxMcpServerHttpSpec
    extends McpServerTests[Identity]
    with McpServerStreamingTests[Identity]
    with SecuredMcpServerTests[Identity]
    with SecuredMcpServerStreamingTests[Identity]
    with SyncToFuture:
  private val clientInfo = Implementation("chimp-server-test", "0.0.1")

  override protected def withServer(server: McpServer[Identity])(test: McpClient[Identity] => Identity[Assertion]): Future[Assertion] =
    withStreamingServer(server.streaming)(test)

  override protected def withStreamingServer(
      server: StreamingMcpServer[Identity]
  )(test: BidirectionalMcpClient[Identity] => Identity[Assertion]): Future[Assertion] =
    withHttpServer(OxServerHttpTransport(List("mcp")).serve(server)): (port, backend, ox) =>
      given Ox = ox
      val transport =
        OxClientHttpTransport(backend, uri"http://localhost:$port/mcp", ProtocolVersion.LatestLegacy, ClientTransport.defaultTimeout)
      try test(McpClient.bidirectional(transport, clientInfo))
      finally transport.close()

  override protected def withSecuredServer(
      server: SecuredMcpServer[Identity, String, String, User],
      token: String
  )(test: McpClient[Identity] => Identity[Assertion]): Future[Assertion] =
    withHttpServer(server.endpoint(List("mcp"))): (port, backend, _) =>
      val transport = ClientHttpTransport[Identity](
        backend,
        uri"http://localhost:$port/mcp",
        headers = List(Header.authorization("Bearer", token))
      )
      try test(McpClient(transport, clientInfo))
      finally transport.close()

  override protected def withSecuredStreamingServer(
      server: SecuredStreamingMcpServer[Identity, String, String, User],
      token: String
  )(test: BidirectionalMcpClient[Identity] => Identity[Assertion]): Future[Assertion] =
    val endpoint = SecuredServerStreamingHttpTransport(List("mcp"), OxServerHttpTransport(List("mcp"))).serve(server)
    withHttpServer(endpoint): (port, backend, ox) =>
      given Ox = ox
      val transport = OxClientHttpTransport(
        backend,
        uri"http://localhost:$port/mcp",
        ProtocolVersion.LatestLegacy,
        ClientTransport.defaultTimeout,
        headers = List(Header.authorization("Bearer", token))
      )
      try test(McpClient.bidirectional(transport, clientInfo))
      finally transport.close()

  // 2026-07-28 requests over raw HTTP: chimp's client does not write the modern `_meta` or the SEP-2243 headers yet
  // (T10 https://github.com/softwaremill/chimp/issues/248, T12 https://github.com/softwaremill/chimp/issues/250)
  private case class EchoInput(message: String) derives Codec, Schema

  private val modernServer: StreamingMcpServer[Identity] =
    McpServer[Identity]().addTool(tool("echo").input[EchoInput].handle(in => ToolResult.text(in.message))).streaming

  private def modernPost(port: Int, backend: SyncBackend, method: String, params: Json, headers: Map[String, String]) =
    val meta = Json.obj(ProtocolMeta.ProtocolVersionKey -> "2026-07-28".asJson, ProtocolMeta.ClientCapabilities -> Json.obj())
    val body = Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id" -> 1.asJson,
      "method" -> method.asJson,
      "params" -> params.deepMerge(Json.obj("_meta" -> meta))
    )
    basicRequest
      .post(uri"http://localhost:$port/mcp")
      .headers(
        Map("Content-Type" -> "application/json", "Accept" -> "application/json, text/event-stream", "MCP-Protocol-Version" -> "2026-07-28")
          ++ headers
      )
      .body(body.noSpaces)
      .send(backend)

  "Streaming HTTP transport" should "serve a 2026-07-28 tools/call end to end" in:
    withHttpServer(OxServerHttpTransport(List("mcp")).serve(modernServer)): (port, backend, _) =>
      val params = Json.obj("name" -> "echo".asJson, "arguments" -> Json.obj("message" -> "modern hello".asJson))
      val response = modernPost(port, backend, "tools/call", params, Map("Mcp-Method" -> "tools/call", "Mcp-Name" -> "echo"))
      response.code shouldBe StatusCode.Ok
      val body = response.body.fold(identity, identity)
      body should include("modern hello")
      body should include("\"resultType\":\"complete\"")

  it should "reject a 2026-07-28 request without Mcp-Method with a plain HTTP 400 and -32020" in:
    withHttpServer(OxServerHttpTransport(List("mcp")).serve(modernServer)): (port, backend, _) =>
      val response = modernPost(port, backend, "tools/list", Json.obj(), Map.empty)
      response.code shouldBe StatusCode.BadRequest
      response.body.fold(identity, identity) should include("-32020")

  "Secured streaming HTTP transport" should "reject a 2026-07-28 request without Mcp-Method with a plain HTTP 400 and -32020" in:
    val endpoint = SecuredServerStreamingHttpTransport(List("mcp"), OxServerHttpTransport(List("mcp"))).serve(securedStreamingServer)
    withHttpServer(endpoint): (port, backend, _) =>
      val response = modernPost(port, backend, "tools/list", Json.obj(), Map("Authorization" -> s"Bearer $validToken"))
      response.code shouldBe StatusCode.BadRequest
      response.body.fold(identity, identity) should include("-32020")

  private def withHttpServer(
      endpoint: ServerEndpoint[OxStreams & WebSockets, Identity]
  )(test: (Int, SyncBackend, Ox) => Assertion): Future[Assertion] =
    toFuture:
      supervised:
        val binding = NettySyncServer().port(0).addEndpoint(endpoint).start()
        try
          val backend = DefaultSyncBackend()
          try test(binding.port, backend, summon[Ox])
          finally backend.close()
        finally binding.stop()
