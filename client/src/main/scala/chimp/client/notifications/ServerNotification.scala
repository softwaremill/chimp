package chimp.client.notifications

import chimp.protocol.*
import io.circe.Json

/** A parsed, strongly-typed view of an incoming server-to-client notification. */
enum ServerNotification:
  case Progress(params: ProgressParams)
  case Cancelled(params: CancelledParams)
  case ResourceUpdated(params: ResourceUpdatedParams)
  case ToolsListChanged
  case PromptsListChanged
  case ResourcesListChanged
  case LoggingMessage(params: LoggingMessageParams)
  case Unknown(method: String, params: Option[Json])

object ServerNotification:
  def parse(n: JSONRPCMessage.Notification): ServerNotification =
    val params = n.params
    n.method match
      case "notifications/progress" =>
        params.flatMap(_.as[ProgressParams].toOption).map(Progress(_)).getOrElse(Unknown(n.method, params))
      case "notifications/cancelled" =>
        params.flatMap(_.as[CancelledParams].toOption).map(Cancelled(_)).getOrElse(Unknown(n.method, params))
      case "notifications/resources/updated" =>
        params.flatMap(_.as[ResourceUpdatedParams].toOption).map(ResourceUpdated(_)).getOrElse(Unknown(n.method, params))
      case "notifications/tools/list_changed"     => ToolsListChanged
      case "notifications/prompts/list_changed"   => PromptsListChanged
      case "notifications/resources/list_changed" => ResourcesListChanged
      case "notifications/message" =>
        params.flatMap(_.as[LoggingMessageParams].toOption).map(LoggingMessage(_)).getOrElse(Unknown(n.method, params))
      case other => Unknown(other, params)
