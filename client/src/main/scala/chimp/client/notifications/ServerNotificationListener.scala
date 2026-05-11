package chimp.client.notifications

import chimp.protocol.JSONRPCMessage

trait ServerNotificationListener[F[_]]:
  def onNotification(n: JSONRPCMessage.Notification): F[Unit]
