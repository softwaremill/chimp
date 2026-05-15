package chimp.protocol

import io.circe.{Decoder, Encoder, Json}

enum ProtocolVersion(val name: String):
  case V2025_06_18 extends ProtocolVersion("2025-06-18")
  case V2025_11_25 extends ProtocolVersion("2025-11-25")

object ProtocolVersion:
  val Latest: ProtocolVersion = V2025_11_25

  def from(s: String): Option[ProtocolVersion] = values.find(_.name == s)
  def negotiate(requested: String): ProtocolVersion = from(requested).getOrElse(Latest)

  given Encoder[ProtocolVersion] = Encoder.instance(v => Json.fromString(v.name))
  given Decoder[ProtocolVersion] = Decoder.decodeString.emap(s => from(s).toRight(s"Unsupported protocol version: $s"))
