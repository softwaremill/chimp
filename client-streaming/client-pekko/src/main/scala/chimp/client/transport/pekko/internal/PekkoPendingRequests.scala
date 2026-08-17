package chimp.client.transport.pekko.internal

import chimp.client.internal.PendingRequests
import chimp.client.{McpTimeoutException, McpTransportException}
import chimp.protocol.{JSONRPCMessage, RequestId}
import org.apache.pekko.stream.Materializer

import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future, Promise}

private[pekko] final class PekkoPendingRequests(using mat: Materializer) extends PendingRequests[Future]:
  private given ExecutionContext = mat.executionContext

  private val pending = ConcurrentHashMap[RequestId, Promise[JSONRPCMessage]]()

  override def register(requestId: RequestId, timeout: FiniteDuration): Future[() => Future[JSONRPCMessage]] =
    val promise = Promise[JSONRPCMessage]()
    val _ = pending.put(requestId, promise)
    Future.successful: () =>
      val timeoutTask = mat.scheduleOnce(timeout, () => { val _ = promise.tryFailure(McpTimeoutException(requestId)) })
      promise.future.andThen { case _ =>
        val _ = timeoutTask.cancel()
        pending.remove(requestId)
      }

  override def complete(requestId: RequestId, msg: JSONRPCMessage): Future[Boolean] =
    Future.successful(Option(pending.remove(requestId)).exists(_.trySuccess(msg)))

  override def isPending(requestId: RequestId): Future[Boolean] = Future.successful(pending.containsKey(requestId))

  override def closeAll(reason: String): Future[Unit] =
    val it = pending.entrySet().iterator()
    while it.hasNext do
      val entry = it.next()
      val _ = entry.getValue.tryFailure(McpTransportException(reason))
      it.remove()
    Future.unit
