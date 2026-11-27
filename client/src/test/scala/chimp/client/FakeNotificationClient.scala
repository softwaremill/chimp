package chimp.client

import chimp.client.notifications.{ServerNotification, ServerNotificationListener}
import chimp.protocol.*
import io.circe.Json
import sttp.monad.MonadError
import sttp.monad.syntax.*

import java.util.concurrent.atomic.AtomicReference

/** A minimal [[BidirectionalMcpClient]] for testing the effect-specific `serverNotifications` streams. Only the notification-listener
  * methods are functional; every other method fails. Use [[emit]] to push a notification to all registered listeners, and [[listenerCount]]
  * to observe registration and removal.
  */
final class FakeNotificationClient[F[_]](using val monad: MonadError[F]) extends BidirectionalMcpClient[F]:
  private val listeners = AtomicReference[List[ServerNotificationListener[F]]](Nil)

  def listenerCount: Int = listeners.get().size

  def emit(n: ServerNotification): F[Unit] =
    listeners.get().foldLeft(monad.unit(()))((acc, l) => acc.flatMap(_ => l.onNotification(n)))

  override def onServerNotification(listener: ServerNotificationListener[F]): F[Unit] =
    val _ = listeners.updateAndGet(_ :+ listener)
    monad.unit(())

  override def removeServerNotification(listener: ServerNotificationListener[F]): F[Unit] =
    val _ = listeners.updateAndGet(_.filterNot(_ eq listener))
    monad.unit(())

  private def unsupported[A]: F[A] = monad.error(UnsupportedOperationException("not supported by FakeNotificationClient"))

  override val serverCapabilities: ServerCapabilities = ServerCapabilities()
  override val serverInfo: Implementation = Implementation(name = "fake", version = "0.0.0")
  override def ping(): F[Unit] = unsupported
  override def close(): F[Unit] = monad.unit(())
  override def listTools(cursor: Option[Cursor]): F[ListToolsResponse] = unsupported
  override def callTool(name: String, arguments: Json): F[CallToolResult] = unsupported
  override def listPrompts(cursor: Option[Cursor]): F[ListPromptsResult] = unsupported
  override def getPrompt(name: String, arguments: Map[String, String]): F[GetPromptResult] = unsupported
  override def listResources(cursor: Option[Cursor]): F[ListResourcesResult] = unsupported
  override def listResourceTemplates(cursor: Option[Cursor]): F[ListResourceTemplatesResult] = unsupported
  override def readResource(uri: String): F[ReadResourceResult] = unsupported
  override def complete(ref: CompleteRef, argument: CompleteArgument): F[CompleteResult] = unsupported
  override def setLoggingLevel(level: LoggingLevel): F[Unit] = unsupported
  override def sendProgress(token: ProgressToken, progress: Double, total: Option[Double], message: Option[String]): F[Unit] = unsupported
  override def subscribeResource(uri: String): F[Unit] = unsupported
  override def unsubscribeResource(uri: String): F[Unit] = unsupported
  override def sendRootsListChanged(): F[Unit] = unsupported
