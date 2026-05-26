package chimp.client.transport.zio

import chimp.client.McpTransportException
import chimp.client.internal.PendingRequests
import chimp.protocol.{JSONRPCMessage, RequestId}
import zio.{Duration, Promise, Ref, Task, UIO, ZIO}

import java.util.concurrent.TimeoutException
import scala.concurrent.duration.FiniteDuration

final class ZioPendingRequests private (pending: Ref[Map[RequestId, Promise[Throwable, JSONRPCMessage]]]) extends PendingRequests[Task]:
  override def register(id: RequestId, timeout: FiniteDuration): Task[() => Task[JSONRPCMessage]] =
    Promise
      .make[Throwable, JSONRPCMessage]
      .flatMap: promise =>
        pending
          .update(_ + (id -> promise))
          .as: () =>
            promise.await
              .timeoutFail(new TimeoutException(s"Request $id timed out after $timeout"))(Duration.fromScala(timeout))
              .onError(_ => pending.update(_ - id))

  override def complete(id: RequestId, msg: JSONRPCMessage): Task[Boolean] =
    pending
      .modify { pending =>
        pending.get(id) match
          case Some(promise) => (Some(promise), pending - id)
          case None          => (None, pending)
      }
      .flatMap:
        case Some(promise) => promise.succeed(msg).as(true)
        case None          => ZIO.succeed(false)

  override def isPending(id: RequestId): Task[Boolean] = pending.get.map(_.contains(id))

  override def closeAll(reason: String): Task[Unit] =
    pending
      .getAndSet(Map.empty)
      .flatMap: pending =>
        ZIO.foreachDiscard(pending.values)(_.fail(McpTransportException(reason)))

object ZioPendingRequests:
  def make: UIO[ZioPendingRequests] =
    Ref.make(Map.empty[RequestId, Promise[Throwable, JSONRPCMessage]]).map(new ZioPendingRequests(_))
