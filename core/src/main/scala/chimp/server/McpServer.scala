package chimp.server

import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.netty.sync.NettySyncServer
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax.*
import io.circe.*
import chimp.mcp.*
import sttp.tapir.docs.apispec.schema.TapirSchemaToJsonSchema
import sttp.apispec.circe.*
import chimp.Tool

class McpServer(tools: List[Tool[?]]):
  private def toolToDefinition(tool: Tool[?]): ToolDefinition =
    val jsonSchema = TapirSchemaToJsonSchema(tool.inputSchema, markOptionsAsNullable = true)
    val json = jsonSchema.asJson
    ToolDefinition(
      name = tool.name,
      description = tool.description,
      inputSchema = json,
      annotations = tool.annotations
        .map(a => ToolAnnotations(a.title, a.readOnlyHint, a.destructiveHint, a.idempotentHint, a.openWorldHint))
    )

  private val toolDefs: List[ToolDefinition] = tools.map(toolToDefinition)

  def handleJsonRpc(request: Json): Json = {
    val response: JSONRPCMessage = request.as[JSONRPCMessage] match {
      case Left(err) =>
        JSONRPCMessage.Error(
          id = RequestId("null"),
          error = JSONRPCErrorObject(
            code = JSONRPCErrorCodes.ParseError.code,
            message = s"Parse error: ${err.getMessage}"
          )
        )
      case Right(JSONRPCMessage.Request(_, method, params: Option[io.circe.Json], id)) =>
        method match {
          case "tools/list" =>
            val result = ListToolsResponse(toolDefs)
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
                        code = JSONRPCErrorCodes.InvalidParams.code,
                        message = "Invalid params for calculate_sum"
                      )
                    )
                }
              case Some(other) =>
                JSONRPCMessage.Error(
                  id = id,
                  error = JSONRPCErrorObject(
                    code = JSONRPCErrorCodes.MethodNotFound.code,
                    message = s"Unknown tool: $other"
                  )
                )
              case None =>
                JSONRPCMessage.Error(
                  id = id,
                  error = JSONRPCErrorObject(
                    code = JSONRPCErrorCodes.InvalidParams.code,
                    message = "Missing tool name"
                  )
                )
            }
          case "initialize" =>
            val capabilities = ServerCapabilities(
              tools = Some(ServerToolsCapability(listChanged = Some(true)))
            )
            val result = InitializeResult(
              protocolVersion = "2025-03-26",
              capabilities = capabilities,
              serverInfo = Implementation(name = "MCP", version = "1.0")
            )
            JSONRPCMessage.Response(id = id, result = result.asJson)
          case other =>
            JSONRPCMessage.Error(
              id = id,
              error = JSONRPCErrorObject(
                code = JSONRPCErrorCodes.MethodNotFound.code,
                message = s"Unknown method: $other"
              )
            )
        }
      case Right(_) =>
        JSONRPCMessage.Error(
          id = RequestId("null"),
          error = JSONRPCErrorObject(
            code = JSONRPCErrorCodes.InvalidRequest.code,
            message = "Invalid request type"
          )
        )
    }
    response.asJson
  }

object McpServer:
  def apply(tools: List[Tool[?]]): McpServer = new McpServer(tools)

  def main(args: Array[String]): Unit =
    // Example: pass an empty list or construct your tools elsewhere
    val server = McpServer(List())
    val jsonRpcEndpoint = endpoint.post
      .in("jsonrpc")
      .in(jsonBody[Json])
      .out(jsonBody[Json])

    val serverEndpoint = jsonRpcEndpoint.handleSuccess { json =>
      println("Request: " + json)
      val r = server.handleJsonRpc(json).deepDropNullValues
      println("Response: " + r)
      r
    }

    println("Starting MCP server on http://localhost:8080 ...")
    NettySyncServer()
      .port(8080)
      .addEndpoint(serverEndpoint)
      .startAndWait()
