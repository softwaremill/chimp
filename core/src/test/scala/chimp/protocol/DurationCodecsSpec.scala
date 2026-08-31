package chimp.protocol

import chimp.protocol.DurationCodecs.given
import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.*

class DurationCodecsSpec extends AnyFlatSpec with Matchers:

  it should "encode a FiniteDuration as an ISO-8601 string" in:
    (5.seconds: FiniteDuration).asJson shouldBe Json.fromString("PT5S")
    (90.minutes: FiniteDuration).asJson shouldBe Json.fromString("PT1H30M")

  it should "decode an ISO-8601 string to a FiniteDuration" in:
    decode[FiniteDuration]("\"PT5S\"") shouldBe Right(5.seconds)
    decode[FiniteDuration]("\"PT1H30M\"") shouldBe Right(90.minutes)

  it should "also decode a bare number as milliseconds" in:
    decode[FiniteDuration]("1500") shouldBe Right(1500.millis)

  it should "reject a malformed duration string" in:
    decode[FiniteDuration]("\"not-a-duration\"").isLeft shouldBe true
