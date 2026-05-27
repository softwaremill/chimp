package chimp.client.transport

import sttp.capabilities.Streams
import sttp.client4.StreamBackend
import sttp.model.Uri

abstract class StreamingHttpTransport[F[_], S](
    protected val backend: StreamBackend[F, S],
    protected val uri: Uri,
    protected val streams: Streams[S]
) extends BidirectionalTransport[F]
