package chimp.client

import chimp.protocol.RequestId

class McpTransportException(message: String, cause: Throwable = null) extends RuntimeException(message, cause)

final class McpAuthorizationException(message: String, val statusCode: Int) extends McpTransportException(message)

final class McpSessionNotFoundException(sessionId: String)
    extends McpTransportException(s"Server reported session-id $sessionId as not found")

class McpProtocolException(message: String) extends McpTransportException(message)

/** The server and chimp share no protocol version. Both version lists are part of the message. */
final class McpUnsupportedProtocolVersionException(val serverVersions: List[String], val clientVersions: List[String])
    extends McpProtocolException(
      s"The server and the client share no protocol version: the server supports ${serverVersions.mkString(", ")}, " +
        s"the client supports ${clientVersions.mkString(", ")}"
    )

final class McpTimeoutException(requestId: RequestId) extends RuntimeException(s"Request $requestId timed out")
