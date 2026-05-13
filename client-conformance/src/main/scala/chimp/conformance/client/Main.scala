package chimp.conformance.client

import chimp.client.McpClient
import chimp.client.transport.HttpTransport
import chimp.protocol.*
import io.circe.Json
import sttp.client4.DefaultSyncBackend
import sttp.model.Uri
import sttp.shared.Identity

object Main:

  private val clientInfo = Implementation(name = "chimp-conformance-client", version = "0.1.0")

  def main(args: Array[String]): Unit =
    if args.isEmpty then
      System.err.println("Usage: chimp-conformance-client <serverUrl>")
      sys.exit(2)

    val serverUrl = Uri.parse(args.last) match
      case Right(url) => url
      case Left(e)    => System.err.println(s"Invalid server URL: $e"); sys.exit(2)

    val scenario = sys.env.getOrElse("MCP_CONFORMANCE_SCENARIO", "")
    val protocolVersion = sys.env.getOrElse("MCP_CONFORMANCE_PROTOCOL_VERSION", ProtocolVersion.Latest)

    val backend = DefaultSyncBackend()
    val transport = HttpTransport[Identity](backend, serverUrl, protocolVersion)

    val rc: Int =
      try
        scenario match
          case "initialize" =>
            val client = McpClient[Identity](transport, clientInfo, protocolVersion = protocolVersion)
            val _ = client.initialize()
            client.close()
            0

          case "tools_call" =>
            val client = McpClient[Identity](transport, clientInfo, protocolVersion = protocolVersion)
            val _ = client.initialize()
            val _ = client.callTool(
              "add_numbers",
              Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(3))
            )
            client.close()
            0

          case s if s == "elicitation-sep1034-client-defaults" || s == "sse-retry" || s.startsWith("auth/") =>
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
