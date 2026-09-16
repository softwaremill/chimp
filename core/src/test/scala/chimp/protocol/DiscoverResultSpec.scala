package chimp.protocol

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.*

class DiscoverResultSpec extends AnyFlatSpec with Matchers:

  it should "parse supportedVersions into known (Right) and unrecognised (Left)" in:
    val discover = DiscoverResult(
      supportedVersions = List("2026-07-28", "1900-01-01"),
      capabilities = ServerCapabilities(),
      ttlMs = 0.millis,
      cacheScope = CacheScope.Private
    )
    discover.getSupportedVersions shouldBe List(Right(ProtocolVersion.V2026_07_28), Left("1900-01-01"))
