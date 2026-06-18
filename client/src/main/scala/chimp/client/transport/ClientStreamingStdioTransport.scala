package chimp.client.transport

import java.io.File
import scala.concurrent.duration.FiniteDuration

/** Abstract base for streaming stdio transports that should consume the subprocess's stdout as an asynchronous stream.
  */
abstract class ClientStreamingStdioTransport[F[_]](
    protected val command: List[String],
    protected val env: Map[String, String] = Map.empty,
    protected val workDir: Option[File] = None,
    protected val timeout: FiniteDuration = ClientTransport.defaultTimeout
) extends ClientBidirectionalTransport[F]
