package chimp.server

import io.circe.Json
import sttp.monad.MonadError
import sttp.monad.syntax.*
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint
import sttp.model.{Header, StatusCode}

/** DNS-rebinding protection: validates the request's `Host` and `Origin` headers against an allow-list of host names. A present header whose
  * host is not allowed is rejected with `403 Forbidden`; absent headers are accepted (non-browser clients may omit `Origin`).
  *
  * @param allowedHosts
  *   Allowed host names (without port; IPv6 addresses bracketed, e.g. `[::1]`).
  * @param enabled
  *   When `false`, all requests pass (use behind a trusted proxy / TLS with authentication).
  */
case class OriginCheck(allowedHosts: Set[String], enabled: Boolean = true):
  def validate(host: Option[String], origin: Option[String]): Boolean =
    if !enabled then true
    else host.forall(allowed) && origin.forall(allowed)

  private def allowed(headerValue: String): Boolean = allowedHosts.contains(OriginCheck.hostName(headerValue))

object OriginCheck:
  val localhostHosts: Set[String] = Set("localhost", "127.0.0.1", "[::1]", "::1")

  /** Allows only localhost host names. The default for chimp servers. */
  val localhostOnly: OriginCheck = OriginCheck(localhostHosts)

  /** Disables the check entirely. */
  val disabled: OriginCheck = OriginCheck(Set.empty, enabled = false)

  private def hostName(headerValue: String): String =
    val trimmed = headerValue.trim
    val schemeIdx = trimmed.indexOf("://")
    val authority = if schemeIdx >= 0 then trimmed.substring(schemeIdx + 3) else trimmed
    if authority.startsWith("[") then
      val close = authority.indexOf("]")
      if close >= 0 then authority.substring(0, close + 1) else ""
    else authority.takeWhile(_ != ':')

private[server] def buildEndpoint[F[_]](server: McpServer[F], path: List[String]): ServerEndpoint[Any, F] =
  val mcpHandler = new McpHandler(server)
  val e = infallibleEndpoint.post
    .in(path.foldLeft(emptyInput)((inputSoFar, pathComponent) => inputSoFar / pathComponent))
    .in(extractFromRequest(_.headers))
    .in(jsonBody[Json])
    .out(statusCode)
    .out(jsonBody[Option[Json]])

  ServerEndpoint.public(
    e,
    me => { (input: (Seq[Header], Json)) =>
      val (headers, json) = input
      given MonadError[F] = me
      val host = headers.find(_.name.equalsIgnoreCase("Host")).map(_.value)
      val origin = headers.find(_.name.equalsIgnoreCase("Origin")).map(_.value)
      if !server.originCheck.validate(host, origin) then me.unit(Right((StatusCode.Forbidden, None)))
      else
        mcpHandler
          .handleJsonRpc(json, headers)
          .map(response => Right((response.statusCode, response.body)))
    }
  )
