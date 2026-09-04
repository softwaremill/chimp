package chimp.client.transport.pekko

import chimp.client.BidirectionalMcpClient
import chimp.client.notifications.{ServerNotification, ServerNotificationListener}
import org.apache.pekko.NotUsed
import org.apache.pekko.stream.Materializer
import org.apache.pekko.stream.scaladsl.Source

import scala.concurrent.Future

extension (client: BidirectionalMcpClient[Future])
  /** A [[Source]] of notifications pushed by the server. When the source is materialized it registers a listener with the client, emits
    * every notification the server sends in order, and removes the listener when the stream terminates. The buffer holds up to `bufferSize`
    * notifications; when it is full the newest notifications are dropped, so delivery never blocks the transport.
    */
  def serverNotifications(bufferSize: Int = 1024)(using mat: Materializer): Source[ServerNotification, NotUsed] =
    given scala.concurrent.ExecutionContext = mat.executionContext
    Source
      .queue[ServerNotification](bufferSize)
      .watchTermination(): (queue, done) =>
        val listener: ServerNotificationListener[Future] = n =>
          val _ = queue.offer(n)
          Future.unit
        val _ = client.onServerNotification(listener)
        val _ = done.onComplete(_ => client.removeServerNotification(listener))
        NotUsed
