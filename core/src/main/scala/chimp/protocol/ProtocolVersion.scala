package chimp.protocol

import io.circe.{Decoder, Encoder, Json}

enum ProtocolVersion(val name: String):
  case V2025_06_18 extends ProtocolVersion("2025-06-18")
  case V2025_11_25 extends ProtocolVersion("2025-11-25")
  case V2026_07_28 extends ProtocolVersion("2026-07-28")

  /** Whether this is a "modern" (per-request `_meta`, no `initialize` handshake) revision, as opposed to a legacy handshake one. */
  def isModern: Boolean = this match
    case V2025_06_18 | V2025_11_25 => false
    case V2026_07_28               => true

object ProtocolVersion:
  /** Latest legacy revision, proposed during the `initialize` handshake. Modern revisions are selected per-request, not here. */
  val Latest: ProtocolVersion = V2025_11_25

  /** All revisions the server supports, newest first; reported by `server/discover`. */
  val supported: List[ProtocolVersion] = List(V2026_07_28, V2025_11_25, V2025_06_18)

  def from(s: String): Option[ProtocolVersion] = values.find(_.name == s)
  def negotiate(requested: String): ProtocolVersion = from(requested).getOrElse(Latest)

  given Encoder[ProtocolVersion] = Encoder.instance(v => Json.fromString(v.name))
  given Decoder[ProtocolVersion] = Decoder.decodeString.emap(s => from(s).toRight(s"Unsupported protocol version: $s"))
