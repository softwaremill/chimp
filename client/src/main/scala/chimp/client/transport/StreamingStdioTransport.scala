package chimp.client.transport

import java.io.File

/** Abstract base for streaming stdio transports. The extra type parameter `S` carries the streaming capability evidence required by the
  * concrete implementation's effect (e.g. ZIO, fs2), which is needed to consume the subprocess's stdout as an asynchronous stream. Concrete
  * implementations live in effect-specific modules.
  */
abstract class StreamingStdioTransport[F[_], S](
    protected val command: List[String],
    protected val env: Map[String, String] = Map.empty,
    protected val workDir: Option[File] = None
) extends BidirectionalTransport[F]
