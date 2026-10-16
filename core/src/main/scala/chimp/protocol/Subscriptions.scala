package chimp.protocol

import io.circe.{Codec, Json}

/** The notification types a client opts into on a `subscriptions/listen` stream (2026-07-28). Each type is opt-in: the server MUST NOT send
  * a type the client has not requested. `resourceSubscriptions` replaces the former `resources/subscribe` RPC.
  */
final case class SubscriptionFilter(
    promptsListChanged: Option[Boolean] = None,
    resourceSubscriptions: Option[List[String]] = None,
    resourcesListChanged: Option[Boolean] = None,
    toolsListChanged: Option[Boolean] = None
) derives Codec

/** Params of a `subscriptions/listen` request: the notification filter the client opts into on the stream. */
final case class SubscriptionsListenParams(notifications: SubscriptionFilter, _meta: Option[Map[String, Json]] = None) derives Codec

final case class SubscriptionsListenRequest(method: String = "subscriptions/listen", params: SubscriptionsListenParams) derives Codec

/** Result of `subscriptions/listen`, sent only when the server tears the subscription down gracefully (e.g. on shutdown); the body is
  * otherwise empty. The subscription id travels in `_meta` under `io.modelcontextprotocol/subscriptionId`.
  */
final case class SubscriptionsListenResult(resultType: ResultType = ResultType.Complete, _meta: Option[Map[String, Json]] = None)
    derives Codec

/** Params of the `notifications/subscriptions/acknowledged` notification: the subset of requested notification types the server will honor.
  */
final case class SubscriptionsAcknowledgedParams(notifications: SubscriptionFilter, _meta: Option[Map[String, Json]] = None) derives Codec

/** The `notifications/subscriptions/acknowledged` notification the server sends first on a listen stream. */
final case class SubscriptionsAcknowledgedNotification(
    method: String = "notifications/subscriptions/acknowledged",
    params: SubscriptionsAcknowledgedParams
) derives Codec
