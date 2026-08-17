package chimp.conformance.client

import chimp.client.McpClient
import chimp.client.transport.ClientHttpTransport
import chimp.client.transport.ox.OxClientHttpTransport
import chimp.protocol.*
import io.circe.Json
import ox.supervised
import sttp.client4.DefaultSyncBackend
import sttp.model.Uri
import sttp.shared.Identity

object Main:

  private val clientInfo = Implementation(name = "chimp-conformance-client", version = "0.1.0")

  private def defaultsFrom(requestedSchema: Json): Map[String, Json] =
    requestedSchema.hcursor
      .downField("properties")
      .as[Map[String, Json]]
      .getOrElse(Map.empty)
      .flatMap((key, prop) => prop.hcursor.downField("default").focus.map(key -> _))

  def main(args: Array[String]): Unit =
    if args.isEmpty then
      System.err.println("Usage: chimp-conformance-client <serverUrl>")
      sys.exit(2)

    val serverUrl = Uri.parse(args.last) match
      case Right(url) => url
      case Left(e)    => System.err.println(s"Invalid server URL: $e"); sys.exit(2)

    val scenario = sys.env.getOrElse("MCP_CONFORMANCE_SCENARIO", "")
    val protocolVersion: ProtocolVersion = sys.env
      .get("MCP_CONFORMANCE_PROTOCOL_VERSION")
      .flatMap(ProtocolVersion.from)
      .getOrElse(ProtocolVersion.Latest)

    val backend = DefaultSyncBackend()
    val transport = ClientHttpTransport[Identity](backend, serverUrl, protocolVersion)

    val rc: Int =
      try
        scenario match
          case "initialize" =>
            val client = McpClient[Identity](transport, clientInfo, protocolVersion)
            client.close()
            0

          case "tools_call" =>
            val client = McpClient[Identity](transport, clientInfo, protocolVersion)
            val _ = client.callTool(
              "add_numbers",
              Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(3))
            )
            client.close()
            0

          case "json-schema-2020-12-preservation" =>
            val client = McpClient[Identity](transport, clientInfo, protocolVersion)
            val tools = client.listTools().tools
            val schema = tools.find(_.name == "json_schema_2020_12_tool").map(_.inputSchema).getOrElse(Json.Null)
            val _ = client.callTool("json_schema_echo", Json.obj("schema" -> schema))
            client.close()
            0

          case "elicitation-sep1034-client-defaults" =>
            supervised:
              val oxTransport = OxClientHttpTransport(backend, serverUrl, protocolVersion)
              val handler = (req: ElicitRequest) => ElicitResult(ElicitAction.Accept, Some(defaultsFrom(req.params.requestedSchema)))
              val client = McpClient.bidirectional[Identity](
                oxTransport,
                clientInfo,
                elicitationHandler = Some(handler),
                protocolVersion = protocolVersion
              )
              val _ = client.callTool("test_client_elicitation_defaults", Json.obj())
              client.close()
              0

          case s if s == "sse-retry" || s.startsWith("auth/") =>
            2

          case other =>
            System.err.println(s"Scenario not implemented: $other")
            3
      catch
        case t: Throwable =>
          t.printStackTrace()
          1
      finally
        try backend.close()
        catch case _: Throwable => ()

    sys.exit(rc)
