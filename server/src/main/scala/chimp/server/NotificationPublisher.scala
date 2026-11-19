package chimp.server

import chimp.protocol.*
import io.circe.Json
import io.circe.syntax.*

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}
import scala.jdk.CollectionConverters.*

/** Publishes list changes and resource updates to the clients which hold a `subscriptions/listen` stream (2026-07-28) open. Give it to a
  * server with `withPublisher`. A streaming server then advertises `listChanged` for its tools, prompts and resources, and `subscribe` for
  * its resources.
  *
  * The methods are thread-safe and can be called from any part of the application. Each one returns after it has put the notification on
  * every open subscription which asked for it.
  */
final class NotificationPublisher:
  private val subscriptions = ConcurrentHashMap[Long, Subscription]()
  private val nextKey = AtomicLong()
  private val closed = AtomicBoolean(false)

  /** Sends `notifications/tools/list_changed`. */
  def toolsListChanged(): Unit = publish(ServerEvent.ToolsListChanged)

  /** Sends `notifications/prompts/list_changed`. */
  def promptsListChanged(): Unit = publish(ServerEvent.PromptsListChanged)

  /** Sends `notifications/resources/list_changed`. */
  def resourcesListChanged(): Unit = publish(ServerEvent.ResourcesListChanged)

  /** Sends `notifications/resources/updated` to the subscriptions which listed `uri`. */
  def resourceUpdated(uri: String): Unit = publish(ServerEvent.ResourceUpdated(uri))

  /** Ends each open subscription with a successful `subscriptions/listen` result, for example when the server shuts down. A subscription
    * opened after this ends at once.
    */
  def close(): Unit =
    closed.set(true)
    subscriptions.values.asScala.toList.foreach(_.end())

  private def publish(event: ServerEvent): Unit = subscriptions.values.asScala.foreach(_.deliver(event))

  private[server] def openSubscriptions: Int = subscriptions.size

  private[server] def open(id: RequestId, filter: SubscriptionFilter, output: SubscriptionOutput): Subscription =
    val key = nextKey.getAndIncrement()
    val subscription = Subscription(id, filter, output, () => { val _ = subscriptions.remove(key) })
    val _ = subscriptions.put(key, subscription)
    subscription.acknowledge()
    if closed.get then subscription.end()
    subscription

/** Where the messages of one subscription go. Each transport supplies its own. */
private[server] trait SubscriptionOutput:
  def send(message: Json): Unit

  /** Called after the server sent the final `subscriptions/listen` result. */
  def close(): Unit

private[server] enum ServerEvent:
  case ToolsListChanged, PromptsListChanged, ResourcesListChanged
  case ResourceUpdated(uri: String)

  def accepted(filter: SubscriptionFilter): Boolean = this match
    case ToolsListChanged     => filter.toolsListChanged.contains(true)
    case PromptsListChanged   => filter.promptsListChanged.contains(true)
    case ResourcesListChanged => filter.resourcesListChanged.contains(true)
    case ResourceUpdated(uri) => filter.resourceSubscriptions.exists(_.contains(uri))

  def notification(meta: Json): JSONRPCMessage.Notification = this match
    case ToolsListChanged     => JSONRPCMessage.Notification(method = "notifications/tools/list_changed", params = Some(meta))
    case PromptsListChanged   => JSONRPCMessage.Notification(method = "notifications/prompts/list_changed", params = Some(meta))
    case ResourcesListChanged => JSONRPCMessage.Notification(method = "notifications/resources/list_changed", params = Some(meta))
    case ResourceUpdated(uri) =>
      JSONRPCMessage.Notification(method = "notifications/resources/updated", params = Some(meta.deepMerge(Json.obj("uri" -> uri.asJson))))

/** One `subscriptions/listen` stream. Every message on it carries the id of the listen request in `_meta`. */
private[server] final class Subscription(
    val id: RequestId,
    filter: SubscriptionFilter,
    output: SubscriptionOutput,
    unregister: () => Unit
):
  private val active = AtomicBoolean(true)
  private val acknowledged = AtomicBoolean(false)
  private val meta: Map[String, Json] = Map(ProtocolMeta.SubscriptionId -> id.asJson)
  private val metaParams: Json = Json.obj("_meta" -> meta.asJson)

  def isActive: Boolean = active.get

  def acknowledge(): Unit = synchronized:
    if active.get then
      send(
        JSONRPCMessage.Notification(
          method = "notifications/subscriptions/acknowledged",
          params = Some(SubscriptionsAcknowledgedParams(filter, Some(meta)).asJson)
        )
      )
      acknowledged.set(true)

  def deliver(event: ServerEvent): Unit =
    if acknowledged.get && active.get && event.accepted(filter) then send(event.notification(metaParams))

  /** Ends the subscription because the client ended it; sends nothing. */
  def cancel(): Unit =
    if active.compareAndSet(true, false) then unregister()

  /** Ends the subscription on the server's initiative, with a successful `subscriptions/listen` result. */
  def end(): Unit = synchronized:
    if active.compareAndSet(true, false) then
      unregister()
      send(JSONRPCMessage.Response(id = id, result = SubscriptionsListenResult(_meta = Some(meta)).asJson))
      output.close()

  private def send(message: JSONRPCMessage): Unit = output.send(message.asJson.deepDropNullValues)

/** A `subscriptions/listen` request which the server accepted, with the subset of the filter it honours. */
private[server] final case class ListenRequest(id: RequestId, filter: SubscriptionFilter, publisher: NotificationPublisher):
  def open(output: SubscriptionOutput): Subscription = publisher.open(id, filter, output)

/** The subscriptions of one stdio connection. They share its single output and are told apart by the subscription id. */
private[server] final class StdioSubscriptions(handler: McpHandler[?, ?], write: Json => Unit):
  private val open = ConcurrentHashMap[RequestId, Subscription]()

  /** Opens a subscription if `message` is a `subscriptions/listen` request which the server accepts, and then gives `true`. Otherwise ends
    * the subscription which a `notifications/cancelled` names, and gives `false`: the server handles the message as usual.
    */
  def accept(message: Json): Boolean =
    handler.listenRequest(message, Nil) match
      case Some(request) =>
        listen(request)
        true
      case None =>
        cancel(message)
        false

  def listen(request: ListenRequest): Unit =
    val output = new SubscriptionOutput:
      def send(message: Json): Unit = write(message)
      def close(): Unit =
        val _ = open.values.removeIf(!_.isActive)
        write(
          (JSONRPCMessage.Notification(
            method = "notifications/cancelled",
            params = Some(CancelledParams(request.id).asJson)
          ): JSONRPCMessage).asJson.deepDropNullValues
        )
    val subscription = request.open(output)
    if subscription.isActive then
      val _ = open.put(request.id, subscription)

  /** Ends the subscription which a `notifications/cancelled` from the client names; ignores any other message. */
  def cancel(message: Json): Unit =
    message.as[JSONRPCMessage] match
      case Right(JSONRPCMessage.Notification(_, "notifications/cancelled", Some(params))) =>
        params.as[CancelledParams].toOption.flatMap(cancelled => Option(open.remove(cancelled.requestId))).foreach(_.cancel())
      case _ => ()

  /** Ends each open subscription of the connection, for example when its input ends. */
  def endAll(): Unit =
    open.values.asScala.toList.foreach(_.end())
    open.clear()
