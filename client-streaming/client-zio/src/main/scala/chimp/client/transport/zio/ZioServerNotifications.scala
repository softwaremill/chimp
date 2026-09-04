package chimp.client.transport.zio

import chimp.client.BidirectionalMcpClient
import chimp.client.notifications.{ServerNotification, ServerNotificationListener}
import zio.stream.ZStream
import zio.{Queue, Task, ZIO}

extension (client: BidirectionalMcpClient[Task])
  /** A [[ZStream]] of notifications pushed by the server. When the stream is run it registers a listener with the client, emits every
    * notification the server sends, and removes the listener and shuts down the backing queue when the stream finishes. The queue is
    * unbounded, so delivery never blocks the transport.
    */
  def serverNotifications: ZStream[Any, Throwable, ServerNotification] =
    ZStream.unwrapScoped:
      for
        queue <- ZIO.acquireRelease(Queue.unbounded[ServerNotification])(_.shutdown)
        listener = new ServerNotificationListener[Task]:
          def onNotification(n: ServerNotification): Task[Unit] = queue.offer(n).unit
        _ <- ZIO.acquireRelease(client.onServerNotification(listener))(_ => client.removeServerNotification(listener).orDie)
      yield ZStream.fromQueue(queue)
