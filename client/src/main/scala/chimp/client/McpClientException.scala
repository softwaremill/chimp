package chimp.client

import chimp.protocol.RequestId

class McpTransportException(message: String, cause: Throwable = null) extends RuntimeException(message, cause)

final class McpAuthorizationException(message: String, val statusCode: Int) extends McpTransportException(message)

final class McpSessionNotFoundException(sessionId: String)
    extends McpTransportException(s"Server reported session-id $sessionId as not found")

final class McpProtocolException(message: String) extends McpTransportException(message)

final class McpTimeoutException(requestId: RequestId) extends RuntimeException(s"Request $requestId timed out")
