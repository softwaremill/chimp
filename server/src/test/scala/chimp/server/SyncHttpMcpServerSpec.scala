package chimp.server

import chimp.client.McpClient
import chimp.client.transport.ClientHttpTransport
import chimp.protocol.{Implementation, ProtocolMeta}
import io.circe.syntax.*
import io.circe.{parser, Codec, Json}
import org.scalatest.Assertion
import ox.supervised
import sttp.client4.*
import sttp.model.StatusCode
import sttp.shared.Identity
import sttp.tapir.Schema
import sttp.tapir.server.netty.sync.NettySyncServer

import scala.concurrent.Future

class SyncHttpMcpServerSpec extends McpServerTests[Identity] with SyncToFuture:
  private val clientInfo = Implementation("chimp-server-test", "0.0.1")

  override protected def withServer(server: McpServer[Identity])(test: McpClient[Identity] => Identity[Assertion]): Future[Assertion] =
    toFuture:
      supervised:
        val binding = NettySyncServer().port(0).addEndpoint(server.endpoint(List("mcp"))).start()
        try
          val backend = DefaultSyncBackend()
          try
            val transport = ClientHttpTransport[Identity](backend, uri"http://localhost:${binding.port}/mcp")
            try test(McpClient(transport, clientInfo))
            finally transport.close()
          finally backend.close()
        finally binding.stop()

  private case class NoInput() derives Codec, Schema

  "Sync HTTP transport" should "answer an input request the client has no capability for with HTTP 400 and -32021" in:
    val server = McpServer[Identity]().addTool(tool("ask").input[NoInput].handle(_ => InputRequired("roots" -> InputRequired.listRoots)))
    toFuture:
      supervised:
        val binding = NettySyncServer().port(0).addEndpoint(server.endpoint(List("mcp"))).start()
        try
          val backend = DefaultSyncBackend()
          try
            val meta = Json.obj(ProtocolMeta.ProtocolVersionKey -> "2026-07-28".asJson, ProtocolMeta.ClientCapabilities -> Json.obj())
            val params = Json.obj("name" -> "ask".asJson, "arguments" -> Json.obj(), "_meta" -> meta)
            val body = Json.obj("jsonrpc" -> "2.0".asJson, "id" -> 1.asJson, "method" -> "tools/call".asJson, "params" -> params)
            val response = basicRequest
              .post(uri"http://localhost:${binding.port}/mcp")
              .headers(Map("Content-Type" -> "application/json", "MCP-Protocol-Version" -> "2026-07-28", "Mcp-Method" -> "tools/call"))
              .header("Mcp-Name", "ask")
              .body(body.noSpaces)
              .send(backend)
            response.code shouldBe StatusCode.BadRequest
            parser.parse(response.body.fold(identity, identity)).flatMap(_.hcursor.downField("error").get[Int]("code")) shouldBe Right(
              -32021
            )
          finally backend.close()
        finally binding.stop()
