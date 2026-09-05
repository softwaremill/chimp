package chimp.protocol

import io.circe.syntax.*
import io.circe.{Codec, Decoder, Encoder, Json}

import scala.concurrent.duration.{DurationLong, FiniteDuration}

// the wire encodes ttlMs as an integer number of milliseconds; file-private so it does not leak into the wider protocol scope
private given Codec[FiniteDuration] =
  Codec.from(Decoder.decodeLong.map(_.millis), Encoder.encodeLong.contramap(_.toMillis))

/** Reserved `_meta` keys carrying the per-request / per-response protocol fields of modern (2026-07-28+) revisions, where version, identity
  * and capabilities travel with each request instead of an `initialize` handshake.
  */
object ProtocolMeta:
  val ProtocolVersion: String = "io.modelcontextprotocol/protocolVersion"
  val ClientInfo: String = "io.modelcontextprotocol/clientInfo"
  val ClientCapabilities: String = "io.modelcontextprotocol/clientCapabilities"
  val ServerInfo: String = "io.modelcontextprotocol/serverInfo"

  /** The protocol version a modern request declares in its `_meta`, if any. Its absence marks a legacy (handshake-based) request. */
  def requestedVersion(meta: Option[Map[String, Json]]): Option[String] =
    meta.flatMap(_.get(ProtocolVersion)).flatMap(_.asString)

  /** An `UnsupportedProtocolVersion` error (`-32022`) naming the versions the server supports, so the client can retry with one of them. */
  def unsupportedVersionError(requested: String, supported: List[String]): JSONRPCErrorObject =
    JSONRPCErrorObject(
      code = JSONRPCErrorCodes.UnsupportedProtocolVersion.code,
      message = "Unsupported protocol version",
      data = Some(Json.obj("requested" -> requested.asJson, "supported" -> supported.asJson))
    )

/** Whether a cached `server/discover` response may be shared across authorization contexts (`Public`) or not (`Private`). */
enum CacheScope:
  case Private, Public

object CacheScope:
  given Encoder[CacheScope] = Encoder.instance(scope => Json.fromString(scope.toString.toLowerCase))
  given Decoder[CacheScope] = Decoder.decodeString.emap:
    case "private" => Right(Private)
    case "public"  => Right(Public)
    case other     => Left(s"Unknown cache scope: $other")

/** Result of `server/discover` (2026-07-28): the server's supported protocol versions, capabilities and identity, learned without a
  * handshake. `serverInfo` travels in `_meta` under [[ProtocolMeta.ServerInfo]].
  */
final case class DiscoverResult(
    supportedVersions: List[String],
    capabilities: ServerCapabilities,
    ttlMs: FiniteDuration,
    cacheScope: CacheScope,
    instructions: Option[String] = None,
    resultType: String = "complete",
    _meta: Option[Map[String, Json]] = None
) derives Codec
