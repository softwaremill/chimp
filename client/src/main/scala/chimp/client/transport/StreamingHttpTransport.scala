package chimp.client.transport

import sttp.capabilities.Streams
import sttp.client4.StreamBackend
import sttp.model.Uri

/** Abstract Streamable HTTP transport using a Streams-capable sttp backend.
  *
  * Concrete implementations live in per-effect chimp client subprojects (`client-zio`, `client-fs2`) and provide SSE parsing, resumability
  * via `Last-Event-ID`, and bidirectional server-initiated dispatch over a long-lived GET.
  */
abstract class StreamingHttpTransport[F[_], S](
    protected val backend: StreamBackend[F, S],
    protected val uri: Uri,
    protected val streams: Streams[S]
) extends Transport[F]
