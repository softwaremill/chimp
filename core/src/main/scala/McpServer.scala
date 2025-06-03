package mcp.server

import sttp.tapir.endpoint
import sttp.tapir.json.circe.{jsonBody, schemaForCirceJson}
import sttp.tapir.server.netty.sync.NettySyncServer
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax.*
import mcp.model.*

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

  // JSON-RPC 2.0 request/response models
  final case class JsonRpcRequest(jsonrpc: String, method: String, params: Option[Json], id: Option[Json]) derives Decoder
  final case class JsonRpcResponse(jsonrpc: String = "2.0", result: Option[Json] = None, error: Option[Json] = None, id: Option[Json])
      derives Encoder

  def handleJsonRpc(request: Json): Json = {
    val response: JsonRpcResponse = request.as[JsonRpcRequest] match {
      case Left(err) =>
        JsonRpcResponse(
          error = Some(
            Json.obj(
              "code" -> Json.fromInt(-32700),
              "message" -> Json.fromString("Parse error: " + err.getMessage)
            )
          ),
          id = None
        )
      case Right(rpcReq) =>
        rpcReq.method match {
          case "tools/list" =>
            val result = ListToolsResponse(tools).asJson
            JsonRpcResponse(result = Some(result), id = rpcReq.id)
          case "tools/call" =>
            val toolName = for {
              params <- rpcReq.params
              name <- params.hcursor.downField("name").as[String].toOption
            } yield name
            val arguments = for {
              params <- rpcReq.params
              args <- params.hcursor.downField("arguments").focus
            } yield args
            toolName match {
              case Some("calculate_sum") =>
                val (a, b): (Option[Double], Option[Double]) = arguments match {
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
                    val result = Json.arr(
                      Json.obj(
                        "type" -> Json.fromString("text"),
                        "text" -> Json.fromString(sum.toString)
                      )
                    )
                    JsonRpcResponse(result = Some(Json.obj("content" -> result)), id = rpcReq.id)
                  case _ =>
                    JsonRpcResponse(
                      error = Some(
                        Json.obj(
                          "code" -> Json.fromInt(-32602),
                          "message" -> Json.fromString("Invalid params for calculate_sum")
                        )
                      ),
                      id = rpcReq.id
                    )
                }
              case Some(other) =>
                JsonRpcResponse(
                  error = Some(
                    Json.obj(
                      "code" -> Json.fromInt(-32601),
                      "message" -> Json.fromString(s"Unknown tool: $other")
                    )
                  ),
                  id = rpcReq.id
                )
              case None =>
                JsonRpcResponse(
                  error = Some(
                    Json.obj(
                      "code" -> Json.fromInt(-32602),
                      "message" -> Json.fromString("Missing tool name")
                    )
                  ),
                  id = rpcReq.id
                )
            }
          case other =>
            JsonRpcResponse(
              error = Some(
                Json.obj(
                  "code" -> Json.fromInt(-32601),
                  "message" -> Json.fromString(s"Unknown method: $other")
                )
              ),
              id = rpcReq.id
            )
        }
    }
    response.asJson
  }

  def main(args: Array[String]): Unit =
    val jsonRpcEndpoint = endpoint.post
      .in(jsonBody[Json])
      .out(jsonBody[Json])

    val serverEndpoint = jsonRpcEndpoint.handleSuccess(handleJsonRpc)

    println("Starting MCP server on http://localhost:8080 ...")
    NettySyncServer()
      .port(8080)
      .addEndpoint(serverEndpoint)
      .startAndWait()
