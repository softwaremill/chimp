package chimp.client.transport.pekko.internal

import chimp.client.internal.PendingRequests
import chimp.client.{McpTimeoutException, McpTransportException}
import chimp.protocol.{JSONRPCMessage, RequestId}
import org.apache.pekko.stream.Materializer

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future, Promise}

private[pekko] final class PekkoPendingRequests(using mat: Materializer) extends PendingRequests[Future]:
  private given ExecutionContext = mat.executionContext

  private val state = StateActor(new PekkoPendingRequests.State, "chimp-mcp-client-pending-requests")

  override def register(requestId: RequestId, timeout: FiniteDuration): Future[() => Future[JSONRPCMessage]] =
    state.ask(_.register(requestId)).map { promise => () =>
      val timeoutTask = mat.scheduleOnce(timeout, () => { val _ = promise.tryFailure(McpTimeoutException(requestId)) })
      promise.future.andThen { case _ =>
        val _ = timeoutTask.cancel()
        state.tell(_.remove(requestId))
      }
    }

  override def complete(requestId: RequestId, msg: JSONRPCMessage): Future[Boolean] =
    state.ask(_.take(requestId)).map(_.exists(_.trySuccess(msg)))

  override def isPending(requestId: RequestId): Future[Boolean] = state.ask(_.isPending(requestId))

  override def closeAll(reason: String): Future[Unit] =
    state.ask(_.takeAll()).map(_.foreach(promise => promise.tryFailure(McpTransportException(reason))))

  private[pekko] def stop(): Unit = state.stopWhenIdle()

private[pekko] object PekkoPendingRequests:
  private final class State:
    private var pending: Map[RequestId, Promise[JSONRPCMessage]] = Map.empty

    def register(requestId: RequestId): Promise[JSONRPCMessage] =
      val promise = Promise[JSONRPCMessage]()
      pending += requestId -> promise
      promise

    def take(requestId: RequestId): Option[Promise[JSONRPCMessage]] =
      val promise = pending.get(requestId)
      pending -= requestId
      promise

    def isPending(requestId: RequestId): Boolean = pending.contains(requestId)

    def remove(requestId: RequestId): Unit = pending -= requestId

    def takeAll(): List[Promise[JSONRPCMessage]] =
      val all = pending.values.toList
      pending = Map.empty
      all
