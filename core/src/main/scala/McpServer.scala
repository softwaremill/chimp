package mcp.server

import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.netty.sync.NettySyncServer
import sttp.tapir.stringBody
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax.*
import mcp.model.*
import scala.concurrent.duration.*
import io.circe._

object McpServer:
  // Sample tool: calculate_sum
  val calculateSumTool = ToolDefinition(
    name = "calculate_sum",
    description = Some("Add two numbers together"),
    inputSchema = Json.obj(
      "type" -> Json.fromString("object"),
      "properties" -> Json.obj(
        "a" -> Json.obj("type" -> Json.fromString("number")),
        "b" -> Json.obj("type" -> Json.fromString("number"))
      ),
      "required" -> Json.arr(Json.fromString("a"), Json.fromString("b"))
    ),
    annotations = Some(ToolAnnotations(title = Some("Calculate Sum"), readOnlyHint = Some(true)))
  )

  val tools = List(calculateSumTool)

  def handleJsonRpc(request: Json): Json = {
    val response: JSONRPCMessage = request.as[JSONRPCMessage] match {
      case Left(err) =>
        JSONRPCMessage.Error(
          id = RequestId("null"),
          error = JSONRPCErrorObject(
            code = JSONRPCErrorCodes.ParseError,
            message = s"Parse error: ${err.getMessage}"
          )
        )
      case Right(JSONRPCMessage.Request(_, method, params: Option[io.circe.Json], id)) =>
        method match {
          case "tools/list" =>
            val result = ListToolsResponse(tools)
            JSONRPCMessage.Response(id = id, result = result.asJson)
          case "tools/call" =>
            val (toolNameOpt, argumentsOpt) = {
              var name: Option[String] = None
              var args: Option[io.circe.Json] = None
              params.foreach { p =>
                val cursor = p.asInstanceOf[io.circe.Json].hcursor
                name = cursor.downField("name").as[String].toOption
                args = cursor.downField("arguments").focus
              }
              (name, args)
            }
            toolNameOpt match {
              case Some("calculate_sum") =>
                val (a, b): (Option[Double], Option[Double]) = argumentsOpt match {
                  case Some(arg) =>
                    (
                      arg.hcursor.downField("a").as[Double].toOption,
                      arg.hcursor.downField("b").as[Double].toOption
                    )
                  case None => (None, None)
                }
                (a, b) match {
                  case (Some(x), Some(y)) =>
                    val sum = x + y
                    val result = ToolCallResult(
                      content = List(ToolContent.Text(text = sum.toString)),
                      isError = false
                    )
                    JSONRPCMessage.Response(id = id, result = result.asJson)
                  case _ =>
                    JSONRPCMessage.Error(
                      id = id,
                      error = JSONRPCErrorObject(
                        code = JSONRPCErrorCodes.InvalidParams,
                        message = "Invalid params for calculate_sum"
                      )
                    )
                }
              case Some(other) =>
                JSONRPCMessage.Error(
                  id = id,
                  error = JSONRPCErrorObject(
                    code = JSONRPCErrorCodes.MethodNotFound,
                    message = s"Unknown tool: $other"
                  )
                )
              case None =>
                JSONRPCMessage.Error(
                  id = id,
                  error = JSONRPCErrorObject(
                    code = JSONRPCErrorCodes.InvalidParams,
                    message = "Missing tool name"
                  )
                )
            }
          case "initialize" =>
            val capabilities = ServerCapabilities(
              tools = Some(ServerToolsCapability(listChanged = Some(true)))
            )
            val result = InitializeResult(
              protocolVersion = "2.0",
              capabilities = capabilities,
              serverInfo = Implementation(name = "MCP", version = "1.0")
            )
            JSONRPCMessage.Response(id = id, result = result.asJson)
          case other =>
            JSONRPCMessage.Error(
              id = id,
              error = JSONRPCErrorObject(
                code = JSONRPCErrorCodes.MethodNotFound,
                message = s"Unknown method: $other"
              )
            )
        }
      case Right(_) =>
        JSONRPCMessage.Error(
          id = RequestId("null"),
          error = JSONRPCErrorObject(
            code = JSONRPCErrorCodes.InvalidRequest,
            message = "Invalid request type"
          )
        )
    }
    response.asJson
  }

  def main(args: Array[String]): Unit =
    // Regular JSON-RPC endpoint
    val jsonRpcEndpoint = endpoint.post
      .in("jsonrpc")
      .in(jsonBody[Json])
      .out(jsonBody[Json])

    val serverEndpoint = jsonRpcEndpoint.handleSuccess { json =>
      println("BBB")
      val r = handleJsonRpc(json).deepDropNullValues
      println(r)
      println(r.noSpaces)
      r
    }

    // Streaming endpoint for tool updates
    val streamEndpoint = endpoint.get
      .in("jsonrpc")
      .out(stringBody)

    val streamServerEndpoint = streamEndpoint.handleSuccess { _ =>
      println("AAA")
      // Create a stream of tool updates
      val toolsJson = tools.asJson.noSpaces
      s"data: $toolsJson\n\n"
    }

    println("Starting MCP server on http://localhost:8080 ...")
    NettySyncServer()
      .port(8080)
      .addEndpoint(serverEndpoint)
      .addEndpoint(streamServerEndpoint)
      .startAndWait()
