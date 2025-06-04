// Combined MCP 2025-03-26 protocol and tool data model.
// NOTE: RequestId and ProgressToken use newtype wrappers for spec accuracy and to avoid ambiguous implicits.
package mcp.model

import io.circe.{Codec, Decoder, Encoder, Json}

// --- JSON-RPC base types ---
// Use newtype wrappers for union types to avoid ambiguous implicits
final case class RequestId(value: String | Int)
object RequestId {
  given encoder: Encoder[RequestId] = Encoder.instance {
    case RequestId(s: String) => Json.fromString(s)
    case RequestId(i: Int)    => Json.fromInt(i)
  }
  given decoder: Decoder[RequestId] = Decoder.instance { c =>
    c.as[String].map(RequestId(_)).orElse(c.as[Int].map(RequestId(_)))
  }
  given codec: Codec[RequestId] = Codec.from(decoder, encoder)
}
final case class ProgressToken(value: String | Int)
object ProgressToken {
  given encoder: Encoder[ProgressToken] = Encoder.instance {
    case ProgressToken(s: String) => Json.fromString(s)
    case ProgressToken(i: Int)    => Json.fromInt(i)
  }
  given decoder: Decoder[ProgressToken] = Decoder.instance { c =>
    c.as[String].map(ProgressToken(_)).orElse(c.as[Int].map(ProgressToken(_)))
  }
  given codec: Codec[ProgressToken] = Codec.from(decoder, encoder)
}
// For pagination
type Cursor = String

// Note: JSONRPCMessage is a protocol sum type; custom codecs are needed for serialization.
enum JSONRPCMessage:
  case Request(jsonrpc: String = "2.0", method: String, params: Option[Json] = None, id: RequestId)
  case Notification(jsonrpc: String = "2.0", method: String, params: Option[Json] = None)
  case Response(jsonrpc: String = "2.0", id: RequestId, result: Json)
  case Error(jsonrpc: String = "2.0", id: RequestId, error: JSONRPCErrorObject)
  case BatchRequest(requests: List[JSONRPCMessage])
  case BatchResponse(responses: List[JSONRPCMessage])

object JSONRPCMessage {
  import io.circe._
  import io.circe.syntax._

  given Decoder[JSONRPCMessage] = Decoder.instance { c =>
    val jsonrpc = c.downField("jsonrpc").as[String].getOrElse("2.0")
    val methodOpt = c.downField("method").as[String].toOption
    val idOpt = c.downField("id").as[RequestId].toOption
    val paramsOpt = c.downField("params").focus
    val resultOpt = c.downField("result").focus
    val errorOpt = c.downField("error").as[JSONRPCErrorObject].toOption
    val isBatchRequest = c.keys.exists(_.exists(_ == "requests"))
    val isBatchResponse = c.keys.exists(_.exists(_ == "responses"))

    (methodOpt, idOpt, paramsOpt, resultOpt, errorOpt, isBatchRequest, isBatchResponse) match {
      case (Some(method), Some(id), _, None, None, false, false) =>
        // Request (with or without params)
        Right(JSONRPCMessage.Request(jsonrpc, method, paramsOpt, id))
      case (Some(method), None, _, None, None, false, false) =>
        // Notification (with or without params)
        Right(JSONRPCMessage.Notification(jsonrpc, method, paramsOpt))
      case (None, Some(id), None, Some(result), None, false, false) =>
        // Response
        Right(JSONRPCMessage.Response(jsonrpc, id, result))
      case (None, Some(id), None, None, Some(error), false, false) =>
        // Error
        Right(JSONRPCMessage.Error(jsonrpc, id, error))
      case (None, None, None, None, None, true, false) =>
        // BatchRequest
        c.downField("requests").as[List[JSONRPCMessage]].map(JSONRPCMessage.BatchRequest(_))
      case (None, None, None, None, None, false, true) =>
        // BatchResponse
        c.downField("responses").as[List[JSONRPCMessage]].map(JSONRPCMessage.BatchResponse(_))
      case _ =>
        Left(DecodingFailure("type JSONRPCMessage could not be decoded from JSON", c.history))
    }
  }

  given Encoder[JSONRPCMessage] = Encoder.instance {
    case JSONRPCMessage.Request(jsonrpc, method, params, id) =>
      Json
        .obj(
          "jsonrpc" -> Json.fromString(jsonrpc),
          "method" -> Json.fromString(method),
          "params" -> params.getOrElse(Json.Null),
          "id" -> id.asJson
        )
        .dropNullValues
    case JSONRPCMessage.Notification(jsonrpc, method, params) =>
      Json
        .obj(
          "jsonrpc" -> Json.fromString(jsonrpc),
          "method" -> Json.fromString(method),
          "params" -> params.getOrElse(Json.Null)
        )
        .dropNullValues
    case JSONRPCMessage.Response(jsonrpc, id, result) =>
      Json.obj(
        "jsonrpc" -> Json.fromString(jsonrpc),
        "id" -> id.asJson,
        "result" -> result
      )
    case JSONRPCMessage.Error(jsonrpc, id, error) =>
      Json.obj(
        "jsonrpc" -> Json.fromString(jsonrpc),
        "id" -> id.asJson,
        "error" -> error.asJson
      )
    case JSONRPCMessage.BatchRequest(requests) =>
      Json.obj(
        "requests" -> requests.asJson
      )
    case JSONRPCMessage.BatchResponse(responses) =>
      Json.obj(
        "responses" -> responses.asJson
      )
  }
}

final case class JSONRPCErrorObject(
    code: Int,
    message: String,
    data: Option[Json] = None
) derives Codec

object JSONRPCErrorCodes {
  val ParseError = -32700
  val InvalidRequest = -32600
  val MethodNotFound = -32601
  val InvalidParams = -32602
  val InternalError = -32603
}

// --- Capabilities ---
final case class ClientCapabilities(
    experimental: Option[Map[String, Json]] = None,
    roots: Option[ClientRootsCapability] = None,
    sampling: Option[Json] = None
) derives Codec
final case class ClientRootsCapability(listChanged: Option[Boolean] = None) derives Codec

final case class ServerCapabilities(
    experimental: Option[Map[String, Json]] = None,
    logging: Option[Json] = None,
    completions: Option[Json] = None,
    prompts: Option[ServerPromptsCapability] = None,
    resources: Option[ServerResourcesCapability] = None,
    tools: Option[ServerToolsCapability] = None
) derives Codec
final case class ServerPromptsCapability(listChanged: Option[Boolean] = None) derives Codec
final case class ServerResourcesCapability(subscribe: Option[Boolean] = None, listChanged: Option[Boolean] = None) derives Codec
final case class ServerToolsCapability(listChanged: Option[Boolean] = None) derives Codec

final case class Implementation(name: String, version: String) derives Codec

// --- Progress, Cancellation, Initialization, Ping ---
final case class CancelledNotification(
    method: String = "notifications/cancelled",
    params: CancelledParams
) derives Codec
final case class CancelledParams(requestId: RequestId, reason: Option[String] = None) derives Codec

final case class InitializeRequest(
    method: String = "initialize",
    params: InitializeParams
) derives Codec
final case class InitializeParams(
    protocolVersion: String,
    capabilities: ClientCapabilities,
    clientInfo: Implementation
) derives Codec
final case class InitializeResult(
    protocolVersion: String,
    capabilities: ServerCapabilities,
    serverInfo: Implementation,
    instructions: Option[String] = None
) derives Codec
final case class InitializedNotification(method: String = "notifications/initialized") derives Codec

final case class PingRequest(method: String = "ping") derives Codec

// --- Model selection ---
final case class ModelPreferences(
    hints: Option[List[ModelHint]] = None,
    costPriority: Option[Double] = None,
    speedPriority: Option[Double] = None,
    intelligencePriority: Option[Double] = None
) derives Codec
final case class ModelHint(name: Option[String] = None) derives Codec

// --- Resource and prompt references ---
final case class ResourceReference(`type`: String = "ref/resource", uri: String) derives Codec
final case class PromptReference(`type`: String = "ref/prompt", name: String) derives Codec

// --- Roots ---
final case class ListRootsRequest(method: String = "roots/list") derives Codec
final case class ListRootsResult(roots: List[Root]) derives Codec
final case class Root(uri: String, name: Option[String] = None) derives Codec
final case class RootsListChangedNotification(method: String = "notifications/roots/list_changed") derives Codec

// --- Autocomplete ---
// Use an enum for CompleteRef instead of Either
enum CompleteRef derives Codec:
  case Prompt(prompt: PromptReference)
  case Resource(resource: ResourceReference)

final case class CompleteRequest(
    method: String = "completion/complete",
    params: CompleteParams
) derives Codec
final case class CompleteParams(
    ref: CompleteRef,
    argument: CompleteArgument
) derives Codec
final case class CompleteArgument(name: String, value: String) derives Codec
final case class CompleteResult(completion: Completion) derives Codec
final case class Completion(values: List[String], total: Option[Int] = None, hasMore: Option[Boolean] = None) derives Codec

// --- Tool model ---
final case class ToolAnnotations(
    title: Option[String] = None,
    readOnlyHint: Option[Boolean] = None,
    destructiveHint: Option[Boolean] = None,
    idempotentHint: Option[Boolean] = None,
    openWorldHint: Option[Boolean] = None
) derives Codec

final case class ToolDefinition(
    name: String,
    description: Option[String] = None,
    inputSchema: Json,
    annotations: Option[ToolAnnotations] = None
) derives Codec

final case class ListToolsResponse(
    tools: List[ToolDefinition],
    nextCursor: Option[String] = None
) derives Codec

// Tool result content types
enum ToolContent derives Codec:
  case Text(
      `type`: String = "text",
      text: String
  )
  case Image(
      `type`: String = "image",
      data: String, // base64
      mimeType: String
  )
  case Audio(
      `type`: String = "audio",
      data: String, // base64
      mimeType: String
  )
  case ResourceContent(
      `type`: String = "resource",
      resource: Resource
  )

final case class Resource(
    uri: String,
    mimeType: String,
    text: Option[String] = None
) derives Codec

final case class ToolCallResult(
    content: List[ToolContent],
    isError: Boolean = false
) derives Codec

// --- Tool call (request/response) ---
final case class CallToolRequest(
    method: String = "tools/call",
    params: CallToolParams
) derives Codec
final case class CallToolParams(
    name: String,
    arguments: Json
) derives Codec
final case class CallToolResult(
    content: List[ToolContent],
    isError: Boolean = false
) derives Codec

// --- List tools (request/response) ---
final case class ListToolsRequest(
    method: String = "tools/list",
    params: Option[ListToolsParams] = None
) derives Codec
final case class ListToolsParams(cursor: Option[Cursor] = None) derives Codec
// ListToolsResponse is above

// --- Notifications for list changes ---
final case class ToolListChangedNotification(method: String = "notifications/tools/list_changed") derives Codec
final case class PromptListChangedNotification(method: String = "notifications/prompts/list_changed") derives Codec
final case class ResourceListChangedNotification(method: String = "notifications/resources/list_changed") derives Codec

// --- Logging ---
final case class LoggingMessageNotification(
    method: String = "notifications/logging/message",
    params: LoggingMessageParams
) derives Codec
final case class LoggingMessageParams(
    level: String,
    message: String
) derives Codec

// --- Progress notification ---
final case class ProgressNotification(
    method: String = "notifications/progress",
    params: ProgressParams
) derives Codec
final case class ProgressParams(
    requestId: RequestId,
    progressToken: Option[ProgressToken] = None,
    message: Option[String] = None,
    percent: Option[Double] = None
) derives Codec
