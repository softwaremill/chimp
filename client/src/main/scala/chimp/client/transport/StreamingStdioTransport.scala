package chimp.client.transport

import sttp.capabilities.Streams

import java.io.File

/** Abstract async stdio transport using a Streams-capable effect.
  *
  * Concrete implementations live in per-effect chimp client subprojects (`client-zio`, `client-fs2`) and replace blocking `readLine` reads
  * with non-blocking stream consumption tied to the effect's runtime.
  */
abstract class StreamingStdioTransport[F[_], S](
    protected val command: List[String],
    protected val env: Map[String, String] = Map.empty,
    protected val workDir: Option[File] = None,
    protected val streams: Streams[S]
) extends Transport[F]
