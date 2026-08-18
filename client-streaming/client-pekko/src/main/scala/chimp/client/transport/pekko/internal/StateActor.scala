package chimp.client.transport.pekko.internal

import chimp.client.McpTransportException
import org.apache.pekko.actor.typed.scaladsl.AskPattern.*
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.scaladsl.adapter.*
import org.apache.pekko.actor.typed.{ActorRef, ActorSystem, Behavior, Scheduler, SupervisorStrategy}
import org.apache.pekko.actor.{DeadLetterSuppression, NoSerializationVerificationNeeded}
import org.apache.pekko.stream.Materializer
import org.apache.pekko.util.Timeout

import java.util.UUID
import java.util.concurrent.TimeoutException
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

private[pekko] final class StateActor[S](state: S, name: String)(using mat: Materializer):
  private given ExecutionContext = mat.executionContext
  private given Timeout = StateActor.askTimeout

  private sealed trait Command extends NoSerializationVerificationNeeded with DeadLetterSuppression
  private final case class Modify(modify: S => Unit) extends Command
  private final case class Read[A](read: S => A, replyTo: ActorRef[A]) extends Command
  private case object StopWhenIdle extends Command
  private case object Idle extends Command

  private val system: ActorSystem[Nothing] = mat.system.toTyped
  private given Scheduler = system.scheduler

  private val ref: ActorRef[Command] =
    system.systemActorOf(Behaviors.supervise(holder).onFailure[Throwable](SupervisorStrategy.resume), s"$name-${UUID.randomUUID()}")

  private def holder: Behavior[Command] =
    Behaviors.setup: context =>
      Behaviors.receiveMessage:
        case Modify(modify) =>
          modify(state)
          Behaviors.same
        case Read(read, replyTo) =>
          replyTo ! read(state)
          Behaviors.same
        case StopWhenIdle =>
          context.setReceiveTimeout(StateActor.idleTimeout, Idle)
          Behaviors.same
        case Idle => Behaviors.stopped

  def tell(modify: S => Unit): Unit = ref ! Modify(modify)

  def ask[A](read: S => A): Future[A] =
    ref
      .ask[A](replyTo => Read(read, replyTo))
      .recoverWith { case t: TimeoutException => Future.failed(McpTransportException("The transport state is not available", t)) }

  /** Keeps the state available for the work which still runs in the background, and stops the actor once nothing reads the state any more.
    */
  def stopWhenIdle(): Unit = ref ! StopWhenIdle

private[pekko] object StateActor:
  private val askTimeout: Timeout = Timeout(10.seconds)
  private val idleTimeout = 30.seconds
