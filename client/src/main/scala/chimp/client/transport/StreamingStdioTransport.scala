package chimp.client.transport

import sttp.capabilities.Streams

import java.io.File

/** Abstract async stdio transport using a Streams-capable effect.
  *
  * Concrete implementations live in per-effect chimp client subprojects (`client-zio`, `client-fs2`) and replace blocking
  * `readLine` reads with non-blocking stream consumption tied to the effect's runtime.
  */
abstract class StreamingStdioTransport[F[_], S](
    command: List[String],
    env: Map[String, String] = Map.empty,
    workDir: Option[File] = None,
    streams: Streams[S]
) extends Transport[F]
