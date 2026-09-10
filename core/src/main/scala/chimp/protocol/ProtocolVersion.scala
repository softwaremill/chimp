package chimp.protocol

import io.circe.{Decoder, Encoder, Json}

// cases are declared oldest-to-newest, so `ordinal` gives release order and code can branch with `version >= V2026_07_28`
enum ProtocolVersion(val name: String):
  case V2025_06_18 extends ProtocolVersion("2025-06-18")
  case V2025_11_25 extends ProtocolVersion("2025-11-25")
  case V2026_07_28 extends ProtocolVersion("2026-07-28")

object ProtocolVersion:
  /** Newest protocol revision chimp knows about. */
  val Latest: ProtocolVersion = V2026_07_28

  /** Newest legacy (handshake-based) revision; the `initialize` negotiation answers this. */
  val LatestLegacy: ProtocolVersion = V2025_11_25

  /** All revisions the server supports, newest first; reported by `server/discover`. */
  val supported: List[ProtocolVersion] = List(V2026_07_28, V2025_11_25, V2025_06_18)

  def from(s: String): Option[ProtocolVersion] = values.find(_.name == s)

  /** Negotiates the legacy `initialize` handshake: the requested revision if known, otherwise the newest legacy one. */
  def negotiate(requested: String): ProtocolVersion = from(requested).getOrElse(LatestLegacy)

  given Ordering[ProtocolVersion] = Ordering.by(_.ordinal)

  extension (version: ProtocolVersion)
    def >=(other: ProtocolVersion): Boolean = version.ordinal >= other.ordinal
    def isModern: Boolean = version >= V2026_07_28

  given Encoder[ProtocolVersion] = Encoder.instance(v => Json.fromString(v.name))
  given Decoder[ProtocolVersion] = Decoder.decodeString.emap(s => from(s).toRight(s"Unsupported protocol version: $s"))
