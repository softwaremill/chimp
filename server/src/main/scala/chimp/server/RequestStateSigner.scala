package chimp.server

import io.circe.syntax.*
import io.circe.{parser, Json}

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.time.Clock
import java.util.Base64
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import scala.concurrent.duration.*
import scala.util.Try

/** Protects the integrity of the request state of [[InputRequired]] outcomes. A server with a signer signs each request state with
  * HMAC-SHA256, together with an expiry and the identity of the request (its method, its tool or prompt name or resource URI, a digest of
  * its arguments, and on a secured server the principal). On a retry, the server verifies the state before the handler runs, and gives the
  * handler the original state. A state which was changed, has expired, or belongs to another request or principal fails with `-32602`.
  *
  * The client can read the state, so do not put secrets in it. Within its lifetime, a state can be used again on the same request.
  */
final class RequestStateSigner private (secret: Array[Byte], ttl: FiniteDuration, clock: Clock):
  private val encoder = Base64.getUrlEncoder.withoutPadding()
  private val decoder = Base64.getUrlDecoder

  private def mac(data: String): Array[Byte] =
    val hmac = Mac.getInstance("HmacSHA256")
    hmac.init(SecretKeySpec(secret, "HmacSHA256"))
    hmac.doFinal(data.getBytes(UTF_8))

  private[server] def sign(state: String, request: Json): String =
    val expiresAt = clock.millis() + ttl.toMillis
    val body =
      encoder.encodeToString(Json.obj("state" -> state.asJson, "exp" -> expiresAt.asJson, "req" -> request).noSpaces.getBytes(UTF_8))
    s"$body.${encoder.encodeToString(mac(body))}"

  /** The original state, or why `signed` fails verification for `request`. */
  private[server] def verify(signed: String, request: Json): Either[String, String] =
    signed.split('.') match
      case Array(body, signature) if Try(decoder.decode(signature)).toOption.exists(MessageDigest.isEqual(_, mac(body))) =>
        val payload = Try(String(decoder.decode(body), UTF_8)).toOption.flatMap(parser.parse(_).toOption).map(_.hcursor)
        val fields =
          for
            c <- payload
            state <- c.get[String]("state").toOption
            expiresAt <- c.get[Long]("exp").toOption
            issuedFor <- c.get[Json]("req").toOption
          yield (state, expiresAt, issuedFor)
        fields match
          case Some((_, expiresAt, _)) if expiresAt <= clock.millis() => Left("the request state has expired")
          case Some((_, _, issuedFor)) if issuedFor != request        => Left("the request state belongs to another request")
          case Some((state, _, _))                                    => Right(state)
          case None                                                   => Left("the request state is malformed")
      case _ => Left("the request state failed the integrity check")

object RequestStateSigner:
  /** @param secret
    *   The HMAC key, at least 32 bytes. Keep it secret, and use the same one on all instances of the server.
    * @param ttl
    *   How long a request state stays valid.
    */
  def apply(secret: Array[Byte], ttl: FiniteDuration = 10.minutes, clock: Clock = Clock.systemUTC()): RequestStateSigner =
    require(secret.length >= 32, s"The request state secret must have at least 32 bytes, got ${secret.length}")
    new RequestStateSigner(secret.clone(), ttl, clock)

  /** A digest of a request's salient parameters, so a state can be bound to them without carrying them. */
  private[server] def digest(json: Json): String =
    val bytes = json.printWith(io.circe.Printer.noSpacesSortKeys).getBytes(UTF_8)
    Base64.getUrlEncoder.withoutPadding().encodeToString(MessageDigest.getInstance("SHA-256").digest(bytes))
