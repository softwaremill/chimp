package chimp.client.internal

import chimp.client.capabilities.{Elicitation, Roots, Sampling}
import chimp.protocol.{CreateMessageRequest, ElicitRequest}
import io.circe.syntax.*
import io.circe.{Json, parser}
import sttp.monad.MonadError
import sttp.monad.syntax.*

private[chimp] final case class CapabilityHandlers[F[_]](methods: Map[String, Json => F[Json]]):
  def ++(other: CapabilityHandlers[F]): CapabilityHandlers[F] =
    CapabilityHandlers(methods ++ other.methods)

private[chimp] object CapabilityHandlers:
  def empty[F[_]]: CapabilityHandlers[F] = CapabilityHandlers(Map.empty)

  def roots[F[_]](r: Roots[F])(using me: MonadError[F]): CapabilityHandlers[F] =
    CapabilityHandlers(Map("roots/list" -> (_ => r.list().map(_.asJson))))

  def sampling[F[_]](s: Sampling[F])(using me: MonadError[F]): CapabilityHandlers[F] =
    CapabilityHandlers(Map("sampling/createMessage" -> { params =>
      params.as[CreateMessageRequest] match
        case Right(req) => s.createMessage(req).map(_.asJson)
        case Left(e)    => me.error(IllegalArgumentException(s"Failed to decode CreateMessageRequest: ${e.getMessage}"))
    }))

  def elicitation[F[_]](e: Elicitation[F])(using me: MonadError[F]): CapabilityHandlers[F] =
    CapabilityHandlers(Map("elicitation/create" -> { params =>
      params.as[ElicitRequest] match
        case Right(req) => e.elicit(req).map(_.asJson)
        case Left(err)  => me.error(IllegalArgumentException(s"Failed to decode ElicitRequest: ${err.getMessage}"))
    }))
