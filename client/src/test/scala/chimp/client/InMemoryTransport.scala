package chimp.client

import chimp.client.transport.BidirectionalTransport
import chimp.protocol.JSONRPCMessage
import sttp.monad.{IdentityMonad, MonadError}
import sttp.shared.Identity

import java.util.concurrent.atomic.AtomicReference
import scala.collection.mutable

final class InMemoryTransport extends BidirectionalTransport[Identity]:
  given monad: MonadError[Identity] = IdentityMonad

  private val incomingHandler = AtomicReference[JSONRPCMessage => Identity[Unit]](_ => ())
  val sent: mutable.ListBuffer[JSONRPCMessage] = mutable.ListBuffer.empty
  private val plannedResponses = mutable.Queue[JSONRPCMessage]()

  def planResponse(msg: JSONRPCMessage): Unit = plannedResponses.enqueue(msg)
  def simulateIncoming(msg: JSONRPCMessage): Unit = incomingHandler.get()(msg)
  def closed: Boolean = closedFlag

  private var closedFlag = false

  override def send(msg: JSONRPCMessage): Identity[Option[JSONRPCMessage]] =
    sent.append(msg)
    msg match
      case _: JSONRPCMessage.Request =>
        if plannedResponses.nonEmpty then Some(plannedResponses.dequeue()) else None
      case _ => None

  override def onIncoming(handler: JSONRPCMessage => Identity[Unit]): Identity[Unit] =
    incomingHandler.set(handler)

  override def close(): Identity[Unit] =
    closedFlag = true
