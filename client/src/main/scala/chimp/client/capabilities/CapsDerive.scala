package chimp.client.capabilities

import chimp.client.internal.CapabilityHandlers
import chimp.protocol.{ClientCapabilities, ClientRootsCapability}
import io.circe.Json
import sttp.monad.MonadError

/** Derives the wire-level [[ClientCapabilities]] and the runtime handler table for a `Caps` intersection type.
  *
  * `Caps = Any` produces empty capabilities; each capability typeclass (`Roots[F]`, `Sampling[F]`, `Elicitation[F]`) listed in
  * `Caps` requires a matching `given` instance in scope.
  */
trait CapsDerive[F[_], Caps]:
  def wire: ClientCapabilities
  def handlers(using MonadError[F]): CapabilityHandlers[F]

object CapsDerive:

  given empty[F[_]]: CapsDerive[F, Any] with
    def wire = ClientCapabilities()
    def handlers(using MonadError[F]) = CapabilityHandlers.empty[F]

  given roots[F[_]](using r: Roots[F]): CapsDerive[F, Roots[F]] with
    def wire = ClientCapabilities(roots = Some(ClientRootsCapability(listChanged = Some(true))))
    def handlers(using MonadError[F]) = CapabilityHandlers.roots(r)

  given sampling[F[_]](using s: Sampling[F]): CapsDerive[F, Sampling[F]] with
    def wire = ClientCapabilities(sampling = Some(Json.obj()))
    def handlers(using MonadError[F]) = CapabilityHandlers.sampling(s)

  given elicitation[F[_]](using e: Elicitation[F]): CapsDerive[F, Elicitation[F]] with
    def wire = ClientCapabilities(elicitation = Some(Json.obj()))
    def handlers(using MonadError[F]) = CapabilityHandlers.elicitation(e)

  given rootsSampling[F[_]](using r: Roots[F], s: Sampling[F]): CapsDerive[F, Roots[F] & Sampling[F]] with
    def wire = ClientCapabilities(
      roots    = Some(ClientRootsCapability(listChanged = Some(true))),
      sampling = Some(Json.obj())
    )
    def handlers(using MonadError[F]) = CapabilityHandlers.roots(r) ++ CapabilityHandlers.sampling(s)

  given rootsElicitation[F[_]](using r: Roots[F], e: Elicitation[F]): CapsDerive[F, Roots[F] & Elicitation[F]] with
    def wire = ClientCapabilities(
      roots       = Some(ClientRootsCapability(listChanged = Some(true))),
      elicitation = Some(Json.obj())
    )
    def handlers(using MonadError[F]) = CapabilityHandlers.roots(r) ++ CapabilityHandlers.elicitation(e)

  given samplingElicitation[F[_]](using s: Sampling[F], e: Elicitation[F]): CapsDerive[F, Sampling[F] & Elicitation[F]] with
    def wire = ClientCapabilities(
      sampling    = Some(Json.obj()),
      elicitation = Some(Json.obj())
    )
    def handlers(using MonadError[F]) = CapabilityHandlers.sampling(s) ++ CapabilityHandlers.elicitation(e)

  given all[F[_]](using r: Roots[F], s: Sampling[F], e: Elicitation[F]): CapsDerive[F, Roots[F] & Sampling[F] & Elicitation[F]] with
    def wire = ClientCapabilities(
      roots       = Some(ClientRootsCapability(listChanged = Some(true))),
      sampling    = Some(Json.obj()),
      elicitation = Some(Json.obj())
    )
    def handlers(using MonadError[F]) =
      CapabilityHandlers.roots(r) ++ CapabilityHandlers.sampling(s) ++ CapabilityHandlers.elicitation(e)
