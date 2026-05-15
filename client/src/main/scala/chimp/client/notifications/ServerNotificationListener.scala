package chimp.client.notifications

trait ServerNotificationListener[F[_]]:
  def onNotification(n: ServerNotification): F[Unit]
