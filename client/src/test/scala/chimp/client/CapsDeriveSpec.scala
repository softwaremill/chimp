package chimp.client

import chimp.client.capabilities.{CapsDerive, Elicitation, Roots, Sampling}
import chimp.protocol.{ClientCapabilities, ElicitRequest, ElicitResult, ListRootsResult}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.monad.IdentityMonad
import sttp.shared.Identity

class CapsDeriveSpec extends AnyFlatSpec with Matchers:

  given sttp.monad.MonadError[Identity] = IdentityMonad

  it should "produce empty ClientCapabilities for Caps = Any" in:
    val d = summon[CapsDerive[Identity, Any]]
    d.wire shouldBe ClientCapabilities()
    d.handlers.methods shouldBe Map.empty

  it should "advertise roots when only Roots is provided" in:
    given Roots[Identity] = () => ListRootsResult(roots = Nil)
    val d = summon[CapsDerive[Identity, Roots[Identity]]]
    d.wire.roots.isDefined shouldBe true
    d.wire.sampling shouldBe None
    d.wire.elicitation shouldBe None
    d.handlers.methods.keySet shouldBe Set("roots/list")

  it should "advertise multiple capabilities for an intersection type" in:
    given Roots[Identity] = () => ListRootsResult(roots = Nil)
    given Elicitation[Identity] = (_: ElicitRequest) =>
      ElicitResult(action = chimp.protocol.ElicitAction.Cancel)
    val d = summon[CapsDerive[Identity, Roots[Identity] & Elicitation[Identity]]]
    d.wire.roots.isDefined shouldBe true
    d.wire.elicitation.isDefined shouldBe true
    d.wire.sampling shouldBe None
    d.handlers.methods.keySet shouldBe Set("roots/list", "elicitation/create")
