package chimp.client.internal

import chimp.protocol.{JSONRPCErrorObject, JSONRPCMessage, RequestId}
import sttp.shared.Identity

import java.util.concurrent.{ConcurrentHashMap, SynchronousQueue, TimeUnit}

trait PendingRequests[F[_]]:
  def register(id: RequestId): F[() => F[JSONRPCMessage]]
  def complete(id: RequestId, msg: JSONRPCMessage): F[Boolean]
  def closeAll(reason: String): F[Unit]

final class SyncPendingRequests extends PendingRequests[Identity]:
  private val pending = ConcurrentHashMap[RequestId, SynchronousQueue[JSONRPCMessage]]()

  override def register(id: RequestId): () => JSONRPCMessage =
    val q = SynchronousQueue[JSONRPCMessage]()
    pending.put(id, q)
    () => q.take()

  override def complete(id: RequestId, msg: JSONRPCMessage): Boolean =
    val q = pending.remove(id)
    if q == null then false
    else q.offer(msg, 1, TimeUnit.SECONDS)

  override def closeAll(reason: String): Unit =
    val it = pending.entrySet().iterator()
    while it.hasNext do
      val entry = it.next()
      val poison = JSONRPCMessage.Error(
        id = entry.getKey,
        error = JSONRPCErrorObject(code = -32000, message = reason)
      )
      val _ = entry.getValue.offer(poison, 100, TimeUnit.MILLISECONDS)
      it.remove()
