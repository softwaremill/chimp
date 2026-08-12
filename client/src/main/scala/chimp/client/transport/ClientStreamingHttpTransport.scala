package chimp.client.transport

import sttp.capabilities.Streams
import sttp.client4.StreamBackend
import sttp.model.{Header, Uri}

/** Abstract base for streaming HTTP transports. The extra type parameter `S` carries the streaming capability evidence required by the sttp
  * [[sttp.client4.StreamBackend]], which is needed to consume Server-Sent Event responses as an asynchronous stream.
  */
abstract class ClientStreamingHttpTransport[F[_], S](
    protected val backend: StreamBackend[F, S],
    protected val uri: Uri,
    protected val streams: Streams[S],
    protected val headers: Seq[Header]
) extends ClientBidirectionalTransport[F]
