package chimp.protocol

object ProtocolVersion:
  val Latest: String = "2025-11-25"

  val Supported: Set[String] = Set("2025-06-18", "2025-11-25")

  def negotiate(requested: String): String =
    if Supported.contains(requested) then requested else Latest
