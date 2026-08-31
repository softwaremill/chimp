package chimp.server

import chimp.protocol.{GetTaskResult, TaskId}
import sttp.monad.MonadError
import sttp.shared.Identity

import java.util.concurrent.{ConcurrentHashMap, ExecutorService, Executors, Future as JavaFuture}
import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** Durable-ish store of task state for the Tasks extension, addressable by task id. The default in-memory implementation keeps tasks for
  * the lifetime of the process.
  */
trait TaskStore[F[_]]:
  def create(task: GetTaskResult): F[Unit]
  def get(taskId: TaskId): F[Option[GetTaskResult]]

  /** Applies `f` to the stored task if present, atomically, and returns the updated task. */
  def update(taskId: TaskId)(f: GetTaskResult => GetTaskResult): F[Option[GetTaskResult]]

object TaskStore:
  def inMemory[F[_]](using m: MonadError[F]): TaskStore[F] = new TaskStore[F]:
    private val tasks = ConcurrentHashMap[TaskId, GetTaskResult]()

    def create(task: GetTaskResult): F[Unit] = m.eval:
      val _ = tasks.put(task.taskId, task)
      ()

    def get(taskId: TaskId): F[Option[GetTaskResult]] = m.eval(Option(tasks.get(taskId)))

    def update(taskId: TaskId)(f: GetTaskResult => GetTaskResult): F[Option[GetTaskResult]] = m.eval:
      Option(tasks.computeIfPresent(taskId, (_, current) => f(current)))

/** Runs task bodies in the background and supports best-effort cancellation. The body is passed as a thunk so that, on eager effect types
  * such as `Identity`, it is only run on the background worker rather than at the call site.
  */
trait TaskExecutor[F[_]]:
  def start(taskId: TaskId)(body: => F[Unit]): F[Unit]
  def cancel(taskId: TaskId): F[Unit]

object TaskExecutor:

  /** A [[TaskExecutor]] for synchronous (`Identity`) servers, such as the Netty sync server, backed by an `ExecutorService`. Defaults to a
    * virtual-thread-per-task executor (JDK 21+), which suits the blocking tool logic that tasks run and scales to many concurrent tasks.
    * Cancellation interrupts the worker thread.
    */
  def threadPool(pool: ExecutorService = Executors.newVirtualThreadPerTaskExecutor()): TaskExecutor[Identity] = new TaskExecutor[Identity]:
    private val running = ConcurrentHashMap[TaskId, JavaFuture[?]]()

    def start(taskId: TaskId)(body: => Identity[Unit]): Identity[Unit] =
      val future = pool.submit(new Runnable:
        def run(): Unit =
          try body
          finally
            val _ = running.remove(taskId))
      val _ = running.put(taskId, future)
      ()

    def cancel(taskId: TaskId): Identity[Unit] =
      val _ = Option(running.remove(taskId)).foreach(_.cancel(true))
      ()

/** Bundles everything a server needs to answer requests with tasks (Tasks extension, experimental).
  *
  * @param useTask
  *   Given a tool name, whether to answer its `tools/call` with a task when the client declares task support. Defaults to always.
  * @param requireTask
  *   Given a tool name, whether a task is required; if the client does not declare task support, the call fails with `-32003`. Defaults to
  *   never.
  */
final case class TaskSupport[F[_]](
    store: TaskStore[F],
    executor: TaskExecutor[F],
    ttl: Option[FiniteDuration] = Some(1.hour),
    pollInterval: Option[FiniteDuration] = Some(1.second),
    useTask: String => Boolean = (_: String) => true,
    requireTask: String => Boolean = (_: String) => false
)
