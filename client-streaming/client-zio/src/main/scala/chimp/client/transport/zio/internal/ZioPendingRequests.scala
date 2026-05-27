package chimp.client.transport.zio

import chimp.client.{McpTimeoutException, McpTransportException}
import chimp.client.internal.PendingRequests
import chimp.protocol.{JSONRPCMessage, RequestId}
import zio.{Duration, Promise, Ref, Task, UIO, ZIO}

import scala.concurrent.duration.FiniteDuration

private[zio] final class ZioPendingRequests private (pending: Ref[Map[RequestId, Promise[Throwable, JSONRPCMessage]]])
    extends PendingRequests[Task]:
  override def register(requestId: RequestId, timeout: FiniteDuration): Task[() => Task[JSONRPCMessage]] =
    Promise
      .make[Throwable, JSONRPCMessage]
      .flatMap: promise =>
        pending
          .update(_ + (requestId -> promise))
          .as: () =>
            promise.await
              .timeoutFail(new McpTimeoutException(requestId))(Duration.fromScala(timeout))
              .onError(_ => pending.update(_ - requestId))

  override def complete(requestId: RequestId, msg: JSONRPCMessage): Task[Boolean] =
    pending
      .modify { pending =>
        pending.get(requestId) match
          case Some(promise) => (Some(promise), pending - requestId)
          case None          => (None, pending)
      }
      .flatMap:
        case Some(promise) => promise.succeed(msg).as(true)
        case None          => ZIO.succeed(false)

  override def isPending(requestId: RequestId): Task[Boolean] = pending.get.map(_.contains(requestId))

  override def closeAll(reason: String): Task[Unit] =
    pending
      .getAndSet(Map.empty)
      .flatMap: pending =>
        ZIO.foreachDiscard(pending.values)(_.fail(McpTransportException(reason)))

object ZioPendingRequests:
  def make: UIO[ZioPendingRequests] =
    Ref.make(Map.empty[RequestId, Promise[Throwable, JSONRPCMessage]]).map(new ZioPendingRequests(_))
