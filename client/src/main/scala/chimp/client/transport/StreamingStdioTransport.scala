package chimp.client.transport

import sttp.capabilities.Streams

import java.io.File

abstract class StreamingStdioTransport[F[_], S](
    protected val command: List[String],
    protected val env: Map[String, String] = Map.empty,
    protected val workDir: Option[File] = None,
    protected val streams: Streams[S]
) extends Transport[F]
