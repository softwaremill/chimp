package chimp.client.internal

import chimp.protocol.{JSONRPCErrorObject, JSONRPCMessage, RequestId}
import sttp.shared.Identity

import java.util.concurrent.{ConcurrentHashMap, TimeoutException}
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, Promise}

trait PendingRequests[F[_]]:
  def register(id: RequestId, timeout: FiniteDuration): F[() => F[JSONRPCMessage]]
  def complete(id: RequestId, msg: JSONRPCMessage): F[Boolean]
  def closeAll(reason: String): F[Unit]

final class SyncPendingRequests extends PendingRequests[Identity]:
  private val pending = ConcurrentHashMap[RequestId, Promise[JSONRPCMessage]]()

  override def register(id: RequestId, timeout: FiniteDuration): () => JSONRPCMessage =
    val promise = Promise[JSONRPCMessage]()
    pending.put(id, promise)
    () =>
      try Await.result(promise.future, timeout)
      catch
        case t: TimeoutException =>
          pending.computeIfPresent(id, (_, _) => null)
          throw t

  override def complete(id: RequestId, msg: JSONRPCMessage): Boolean =
    var completed = false
    pending.computeIfPresent(
      id,
      (_, promise) =>
        completed = promise.trySuccess(msg)
        null
    )
    completed

  override def closeAll(reason: String): Unit =
    val it = pending.entrySet().iterator()
    while it.hasNext do
      val entry = it.next()
      val poison = JSONRPCMessage.Error(
        id = entry.getKey,
        error = JSONRPCErrorObject(code = -32000, message = reason)
      )
      val _ = entry.getValue.trySuccess(poison)
      it.remove()
