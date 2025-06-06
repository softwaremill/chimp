package chimp.protocol

import io.circe.Json
import io.circe.parser.*
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModelSpec extends AnyFlatSpec with Matchers {
  import JSONRPCMessage.*
  import chimp.protocol.JSONRPCErrorCodes.*

  // Helper function to parse JSON strings
  private def parseJson(str: String): Json = parse(str).getOrElse(throw new RuntimeException("Invalid JSON"))

  "JSONRPCMessage" should "handle Request messages according to JSON-RPC 2.0 spec" in {
    // Given
    val request: JSONRPCMessage = Request(
      method = "test/method",
      params = Some(Json.obj("param1" -> Json.fromString("value1"))),
      id = RequestId("123")
    )
    val expectedJson = parseJson("""
      {
        "jsonrpc": "2.0",
        "method": "test/method",
        "params": {"param1": "value1"},
        "id": "123"
      }
    """)

    // When
    val json = request.asJson
    val decoded = json.as[JSONRPCMessage].getOrElse(fail("Failed to decode JSON"))

    // Then
    json shouldBe expectedJson
    decoded shouldBe request
  }

  it should "handle Notification messages according to JSON-RPC 2.0 spec" in {
    // Given
    val notification: JSONRPCMessage = Notification(
      method = "test/notification",
      params = Some(Json.obj("param1" -> Json.fromString("value1")))
    )
    val expectedJson = parseJson("""
      {
        "jsonrpc": "2.0",
        "method": "test/notification",
        "params": {"param1": "value1"}
      }
    """)

    // When
    val json = notification.asJson
    val decoded = json.as[JSONRPCMessage].getOrElse(fail("Failed to decode JSON"))

    // Then
    json shouldBe expectedJson
    decoded shouldBe notification
  }

  it should "handle Response messages according to JSON-RPC 2.0 spec" in {
    // Given
    val response: JSONRPCMessage = Response(
      id = RequestId(123),
      result = Json.obj("result" -> Json.fromString("success"))
    )
    val expectedJson = parseJson("""
      {
        "jsonrpc": "2.0",
        "id": 123,
        "result": {"result": "success"}
      }
    """)

    // When
    val json = response.asJson
    val decoded = json.as[JSONRPCMessage].getOrElse(fail("Failed to decode JSON"))

    // Then
    json shouldBe expectedJson
    decoded shouldBe response
  }

  it should "handle Error messages according to JSON-RPC 2.0 spec" in {
    // Given
    val error: JSONRPCMessage = Error(
      id = RequestId("error-123"),
      error = JSONRPCErrorObject(
        code = InvalidRequest.code,
        message = "Invalid request",
        data = Some(Json.obj("details" -> Json.fromString("More info")))
      )
    )
    val expectedJson = parseJson("""
      {
        "jsonrpc": "2.0",
        "id": "error-123",
        "error": {
          "code": -32600,
          "message": "Invalid request",
          "data": {"details": "More info"}
        }
      }
    """)

    // When
    val json = error.asJson
    val decoded = json.as[JSONRPCMessage].getOrElse(fail("Failed to decode JSON"))

    // Then
    json shouldBe expectedJson
    decoded shouldBe error
  }

  it should "handle BatchRequest messages according to JSON-RPC 2.0 spec" in {
    // Given
    val batchRequest: JSONRPCMessage = BatchRequest(
      List(
        Request(method = "method1", id = RequestId(1)),
        Request(method = "method2", id = RequestId(2))
      )
    )
    val expectedJson = parseJson("""
      {
        "requests": [
          {
            "jsonrpc": "2.0",
            "method": "method1",
            "id": 1
          },
          {
            "jsonrpc": "2.0",
            "method": "method2",
            "id": 2
          }
        ]
      }
    """)

    // When
    val json = batchRequest.asJson
    val decoded = json.as[JSONRPCMessage].getOrElse(fail("Failed to decode JSON"))

    // Then
    json shouldBe expectedJson
    decoded shouldBe batchRequest
  }

  it should "handle BatchResponse messages according to JSON-RPC 2.0 spec" in {
    // Given
    val batchResponse: JSONRPCMessage = BatchResponse(
      List(
        Response(id = RequestId(1), result = Json.obj("result1" -> Json.fromString("value1"))),
        Response(id = RequestId(2), result = Json.obj("result2" -> Json.fromString("value2")))
      )
    )
    val expectedJson = parseJson("""
      {
        "responses": [
          {
            "jsonrpc": "2.0",
            "id": 1,
            "result": {"result1": "value1"}
          },
          {
            "jsonrpc": "2.0",
            "id": 2,
            "result": {"result2": "value2"}
          }
        ]
      }
    """)

    // When
    val json = batchResponse.asJson
    val decoded = json.as[JSONRPCMessage].getOrElse(fail("Failed to decode JSON"))

    // Then
    json shouldBe expectedJson
    decoded shouldBe batchResponse
  }

  it should "handle both numeric and string request IDs according to JSON-RPC 2.0 spec" in {
    // Given
    val numericRequest: JSONRPCMessage = Request(method = "test", id = RequestId(123))
    val stringRequest: JSONRPCMessage = Request(method = "test", id = RequestId("abc"))

    // When
    val numericJson = numericRequest.asJson
    val stringJson = stringRequest.asJson
    val decodedNumeric = numericJson.as[JSONRPCMessage].getOrElse(fail())
    val decodedString = stringJson.as[JSONRPCMessage].getOrElse(fail())

    // Then
    decodedNumeric shouldBe numericRequest
    decodedString shouldBe stringRequest
  }

  it should "handle optional parameters according to JSON-RPC 2.0 spec" in {
    // Given
    val requestWithParams: JSONRPCMessage = Request(
      method = "test",
      params = Some(Json.obj("param" -> Json.fromString("value"))),
      id = RequestId(1)
    )
    val requestWithoutParams: JSONRPCMessage = Request(method = "test", id = RequestId(2))

    // When
    val jsonWithParams = requestWithParams.asJson
    val jsonWithoutParams = requestWithoutParams.asJson
    val decodedWithParams = jsonWithParams.as[JSONRPCMessage].getOrElse(fail())
    val decodedWithoutParams = jsonWithoutParams.as[JSONRPCMessage].getOrElse(fail())

    // Then
    decodedWithParams shouldBe requestWithParams
    decodedWithoutParams shouldBe requestWithoutParams
  }

  it should "handle all standard JSON-RPC error codes according to spec" in {
    // Given
    val errorCodes = List(
      ParseError,
      InvalidRequest,
      MethodNotFound,
      InvalidParams,
      InternalError
    )

    // When/Then
    errorCodes.foreach { code =>
      // Given
      val error: JSONRPCMessage = Error(
        id = RequestId("test"),
        error = JSONRPCErrorObject(code = code.code, message = "Test error")
      )

      // When
      val json = error.asJson
      val decoded = json.as[JSONRPCMessage].getOrElse(fail())

      // Then
      decoded shouldBe error
    }
  }
}
