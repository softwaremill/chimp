package chimp.client.internal

import chimp.client.McpTimeoutException
import chimp.protocol.{JSONRPCErrorCodes, JSONRPCErrorObject, JSONRPCMessage, RequestId}
import sttp.shared.Identity

import java.util.concurrent.{ConcurrentHashMap, TimeoutException}
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, Promise}

trait PendingRequests[F[_]]:
  def register(requestId: RequestId, timeout: FiniteDuration): F[() => F[JSONRPCMessage]]
  def complete(requestId: RequestId, msg: JSONRPCMessage): F[Boolean]
  def isPending(requestId: RequestId): F[Boolean]
  def closeAll(reason: String): F[Unit]

private[client] final class SyncPendingRequests extends PendingRequests[Identity]:
  private val pending = ConcurrentHashMap[RequestId, Promise[JSONRPCMessage]]()

  override def register(requestId: RequestId, timeout: FiniteDuration): () => JSONRPCMessage =
    val promise = Promise[JSONRPCMessage]()
    pending.put(requestId, promise)
    () =>
      try Await.result(promise.future, timeout)
      catch
        case _: TimeoutException =>
          pending.computeIfPresent(requestId, (_, _) => null)
          throw new McpTimeoutException(requestId)

  override def complete(requestId: RequestId, msg: JSONRPCMessage): Boolean =
    var completed = false
    pending.computeIfPresent(
      requestId,
      (_, promise) =>
        completed = promise.trySuccess(msg)
        null
    )
    completed

  override def isPending(requestId: RequestId): Boolean = pending.containsKey(requestId)

  override def closeAll(reason: String): Unit =
    val it = pending.entrySet().iterator()
    while it.hasNext do
      val entry = it.next()
      val poison = JSONRPCMessage.Error(
        id = entry.getKey,
        error = JSONRPCErrorObject(code = JSONRPCErrorCodes.InvocationError.code, message = reason)
      )
      val _ = entry.getValue.trySuccess(poison)
      it.remove()
