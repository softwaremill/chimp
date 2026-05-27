package chimp.client.transport

import java.io.File

/** Abstract base for streaming stdio transports that should consume the subprocess's stdout as an asynchronous stream. Concrete
  * implementations live in effect-specific modules.
  */
abstract class StreamingStdioTransport[F[_]](
    protected val command: List[String],
    protected val env: Map[String, String] = Map.empty,
    protected val workDir: Option[File] = None
) extends BidirectionalTransport[F]
