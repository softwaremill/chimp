package chimp.protocol

import io.circe.{Codec, Decoder, Encoder, Json}

/** The MCP Tasks extension (SEP-2663, identifier `io.modelcontextprotocol/tasks`): durable handles that let a receiver answer a request
  * with a task, which the requestor then polls and later collects the result of. Experimental; the wire format follows the reference
  * extension and may change.
  */
object TasksExtension:
  val Id: String = "io.modelcontextprotocol/tasks"

/** State of a task. Terminal states are `Completed`, `Failed` and `Cancelled`. */
enum TaskStatus:
  case Working, InputRequired, Completed, Failed, Cancelled

object TaskStatus:
  private val toWire: Map[TaskStatus, String] = Map(
    Working -> "working",
    InputRequired -> "input_required",
    Completed -> "completed",
    Failed -> "failed",
    Cancelled -> "cancelled"
  )
  private val fromWire: Map[String, TaskStatus] = toWire.map((k, v) => v -> k)

  def isTerminal(status: TaskStatus): Boolean = status match
    case Completed | Failed | Cancelled => true
    case Working | InputRequired        => false

  given Encoder[TaskStatus] = Encoder.instance(status => Json.fromString(toWire(status)))
  given Decoder[TaskStatus] = Decoder.decodeString.emap(s => fromWire.get(s).toRight(s"Unknown task status: $s"))

/** Result returned when a receiver answers a request with a task instead of the request's normal result. */
final case class CreateTaskResult(
    taskId: String,
    status: TaskStatus,
    createdAt: Option[String] = None,
    lastUpdatedAt: Option[String] = None,
    ttlMs: Option[Long] = None,
    pollIntervalMs: Option[Long] = None,
    statusMessage: Option[String] = None,
    resultType: String = "task",
    _meta: Option[Map[String, Json]] = None
) derives Codec

final case class GetTaskParams(taskId: String, _meta: Option[Map[String, Json]] = None) derives Codec
final case class GetTaskRequest(method: String = "tasks/get", params: GetTaskParams) derives Codec

/** Detailed task state returned by `tasks/get`. `result` is present once the task is `Completed`, `error` once it has `Failed`, and
  * `inputRequests` while it is `InputRequired`. `result` and `inputRequests` are left as raw JSON, since their shape depends on the request
  * the task stands for.
  */
final case class GetTaskResult(
    taskId: String,
    status: TaskStatus,
    createdAt: Option[String] = None,
    lastUpdatedAt: Option[String] = None,
    ttlMs: Option[Long] = None,
    pollIntervalMs: Option[Long] = None,
    statusMessage: Option[String] = None,
    result: Option[Json] = None,
    error: Option[Json] = None,
    inputRequests: Option[Json] = None,
    resultType: Option[String] = None,
    _meta: Option[Map[String, Json]] = None
) derives Codec

final case class CancelTaskParams(taskId: String, _meta: Option[Map[String, Json]] = None) derives Codec
final case class CancelTaskRequest(method: String = "tasks/cancel", params: CancelTaskParams) derives Codec

final case class UpdateTaskParams(taskId: String, inputResponses: Json, _meta: Option[Map[String, Json]] = None) derives Codec
final case class UpdateTaskRequest(method: String = "tasks/update", params: UpdateTaskParams) derives Codec

/** Acknowledgement returned by `tasks/cancel` and `tasks/update`. */
final case class TaskAck(
    taskId: Option[String] = None,
    status: Option[TaskStatus] = None,
    resultType: String = "complete",
    _meta: Option[Map[String, Json]] = None
) derives Codec

/** Notification pushed by a receiver that supports task subscriptions; carries the same fields as a `tasks/get` result. */
final case class TaskStatusNotification(
    method: String = "notifications/tasks",
    params: GetTaskResult
) derives Codec
