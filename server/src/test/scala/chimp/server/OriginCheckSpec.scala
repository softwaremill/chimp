package chimp.server

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OriginCheckSpec extends AnyFlatSpec with Matchers:
  private val check = OriginCheck.localhostOnly

  "OriginCheck.localhostOnly" should "allow localhost Host and Origin" in:
    check.validate(Some("localhost:8080"), Some("http://localhost:8080")) shouldBe true

  it should "allow 127.0.0.1 with a port" in:
    check.validate(Some("127.0.0.1:8080"), None) shouldBe true

  it should "allow a bracketed IPv6 loopback Host and Origin" in:
    check.validate(Some("[::1]:8080"), Some("http://[::1]:8080")) shouldBe true

  it should "allow requests with no Host or Origin" in:
    check.validate(None, None) shouldBe true

  it should "reject a non-localhost Host" in:
    check.validate(Some("evil.example.com"), None) shouldBe false

  it should "reject a non-localhost Origin even when Host is localhost" in:
    check.validate(Some("localhost:8080"), Some("http://evil.example.com")) shouldBe false

  "A disabled OriginCheck" should "allow any host" in:
    OriginCheck.disabled.validate(Some("evil.example.com"), Some("http://evil.example.com")) shouldBe true
