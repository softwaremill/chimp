package chimp.client.transport.ox

import chimp.client.BidirectionalMcpClient
import chimp.client.notifications.{ServerNotification, ServerNotificationListener}
import ox.channels.Channel
import ox.discard
import ox.flow.Flow
import sttp.shared.Identity

extension (client: BidirectionalMcpClient[Identity])
  /** A [[Flow]] of notifications pushed by the server. Each time the flow is run it registers a listener with the client, emits every
    * notification the server sends, and removes the listener when the flow finishes. The backing channel is unbounded, so delivery never
    * blocks the transport; run the flow with a bound (for example `.take`) or in its own scope if you do not want it to run for the whole
    * lifetime of the client.
    */
  def serverNotifications: Flow[ServerNotification] =
    Flow.usingEmit: emit =>
      val channel = Channel.unlimited[ServerNotification]
      val listener: ServerNotificationListener[Identity] = n => channel.sendOrClosed(n).discard
      client.onServerNotification(listener)
      try channel.foreach(n => emit(n))
      finally client.removeServerNotification(listener)
