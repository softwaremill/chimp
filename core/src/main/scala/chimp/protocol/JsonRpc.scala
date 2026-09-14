package chimp.protocol

import io.circe.syntax.*
import io.circe.*

enum JSONRPCMessage:
  case Request(jsonrpc: String = "2.0", method: String, params: Option[Json] = None, id: RequestId)
  case Notification(jsonrpc: String = "2.0", method: String, params: Option[Json] = None)
  case Response(jsonrpc: String = "2.0", id: RequestId, result: Json)
  case Error(jsonrpc: String = "2.0", id: RequestId, error: JSONRPCErrorObject)

object JSONRPCMessage:

  given Decoder[JSONRPCMessage] = Decoder.instance: c =>
    val jsonrpc = c.downField("jsonrpc").as[String].getOrElse("2.0")
    val methodOpt = c.downField("method").as[String].toOption
    val idOpt = c.downField("id").as[RequestId].toOption
    val paramsOpt = c.downField("params").focus
    val resultOpt = c.downField("result").focus
    val errorOpt = c.downField("error").as[JSONRPCErrorObject].toOption

    (methodOpt, idOpt, resultOpt, errorOpt) match
      case (Some(method), Some(id), None, None) => Right(Request(jsonrpc, method, paramsOpt, id))
      case (Some(method), None, None, None)     => Right(Notification(jsonrpc, method, paramsOpt))
      case (None, Some(id), Some(result), None) => Right(Response(jsonrpc, id, result))
      case (None, Some(id), None, Some(error))  => Right(Error(jsonrpc, id, error))
      case _                                    => Left(DecodingFailure("type JSONRPCMessage could not be decoded from JSON", c.history))

  given Encoder[JSONRPCMessage] = Encoder.instance:
    case Request(jsonrpc, method, params, id) =>
      Json
        .obj(
          "jsonrpc" -> Json.fromString(jsonrpc),
          "method" -> Json.fromString(method),
          "params" -> params.getOrElse(Json.Null),
          "id" -> id.asJson
        )
        .dropNullValues
    case Notification(jsonrpc, method, params) =>
      Json
        .obj(
          "jsonrpc" -> Json.fromString(jsonrpc),
          "method" -> Json.fromString(method),
          "params" -> params.getOrElse(Json.Null)
        )
        .dropNullValues
    case Response(jsonrpc, id, result) =>
      Json.obj(
        "jsonrpc" -> Json.fromString(jsonrpc),
        "id" -> id.asJson,
        "result" -> result
      )
    case Error(jsonrpc, id, error) =>
      Json.obj(
        "jsonrpc" -> Json.fromString(jsonrpc),
        "id" -> id.asJson,
        "error" -> error.asJson
      )

final case class JSONRPCErrorObject(code: Int, message: String, data: Option[Json] = None) derives Codec

enum JSONRPCErrorCodes(val code: Int):
  case ParseError extends JSONRPCErrorCodes(-32700)
  case InvalidRequest extends JSONRPCErrorCodes(-32600)
  case MethodNotFound extends JSONRPCErrorCodes(-32601)
  case InvalidParams extends JSONRPCErrorCodes(-32602)
  case InternalError extends JSONRPCErrorCodes(-32603)
  case InvocationError extends JSONRPCErrorCodes(-32000)
  case ResourceNotFound extends JSONRPCErrorCodes(-32002)
