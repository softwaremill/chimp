package chimp.server

case class OriginCheck(allowedHosts: Set[String], enabled: Boolean = true):
  def validate(host: Option[String], origin: Option[String]): Boolean =
    if !enabled then true
    else host.forall(allowed) && origin.forall(allowed)

  private def allowed(headerValue: String): Boolean = allowedHosts.contains(OriginCheck.hostName(headerValue))

object OriginCheck:
  private val localhostHosts: Set[String] = Set("localhost", "127.0.0.1", "[::1]", "::1")

  val localhostOnly: OriginCheck = OriginCheck(localhostHosts)
  val disabled: OriginCheck = OriginCheck(Set.empty, enabled = false)

  private def hostName(headerValue: String): String =
    val trimmed = headerValue.trim
    val schemeIdx = trimmed.indexOf("://")
    val authority = if schemeIdx >= 0 then trimmed.substring(schemeIdx + 3) else trimmed
    if authority.startsWith("[") then
      val close = authority.indexOf("]")
      if close >= 0 then authority.substring(0, close + 1) else ""
    else authority.takeWhile(_ != ':')
