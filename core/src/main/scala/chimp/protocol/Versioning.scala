package chimp.protocol

import io.circe.syntax.*
import io.circe.{Codec, Decoder, Encoder, Json}

import scala.concurrent.duration.{Duration, DurationLong, FiniteDuration}

/** The wire encodes `ttlMs` as an integer number of milliseconds; file-private so it does not leak into the wider protocol scope. */
private given Codec[FiniteDuration] =
  Codec.from(Decoder.decodeLong.map(_.millis), Encoder.encodeLong.contramap(_.toMillis))

/** Reserved `_meta` keys carrying the per-request / per-response protocol fields of modern (2026-07-28+) revisions, where version, identity
  * and capabilities travel with each request instead of an `initialize` handshake.
  */
object ProtocolMeta:
  val ProtocolVersionKey: String = "io.modelcontextprotocol/protocolVersion"
  val ClientInfo: String = "io.modelcontextprotocol/clientInfo"
  val ClientCapabilities: String = "io.modelcontextprotocol/clientCapabilities"
  val ServerInfo: String = "io.modelcontextprotocol/serverInfo"
  val SubscriptionId: String = "io.modelcontextprotocol/subscriptionId"

  /** An `UnsupportedProtocolVersion` error (`-32022`) naming the versions the server supports, so the client can retry with one of them. */
  def unsupportedVersionError(requested: String, supported: List[String]): JSONRPCErrorObject =
    JSONRPCErrorObject(
      code = JSONRPCErrorCodes.UnsupportedProtocolVersion.code,
      message = "Unsupported protocol version",
      data = Some(Json.obj("requested" -> requested.asJson, "supported" -> supported.asJson))
    )

/** The type of a modern (2026-07-28+) result. A closed set on the wire; only `complete` exists today. */
enum ResultType(val name: String):
  case Complete extends ResultType("complete")

object ResultType:
  given Encoder[ResultType] = Encoder.instance(resultType => Json.fromString(resultType.name))
  given Decoder[ResultType] = Decoder.decodeString.emap:
    case "complete" => Right(Complete)
    case other      => Left(s"Unknown result type: $other")

/** Whether a cached response may be shared across authorization contexts (`Public`) or not (`Private`). */
enum CacheScope:
  case Private, Public

object CacheScope:
  given Encoder[CacheScope] = Encoder.instance(scope => Json.fromString(scope.toString.toLowerCase))
  given Decoder[CacheScope] = Decoder.decodeString.emap:
    case "private" => Right(Private)
    case "public"  => Right(Public)
    case other     => Left(s"Unknown cache scope: $other")

/** Cache hints a modern cacheable result carries: how long it may be cached (`ttlMs`) and in what scope. */
final case class CacheHints(ttlMs: FiniteDuration, cacheScope: CacheScope) derives Codec

object CacheHints:
  /** chimp does not cache results yet: a zero TTL in the private scope. */
  val Default: CacheHints = CacheHints(Duration.Zero, CacheScope.Private)

/** Result of `server/discover` (2026-07-28): the server's supported protocol versions, capabilities and identity, learned without a
  * handshake. `serverInfo` travels in `_meta` under [[ProtocolMeta.ServerInfo]].
  */
final case class DiscoverResult(
    supportedVersions: List[String],
    capabilities: ServerCapabilities,
    ttlMs: FiniteDuration,
    cacheScope: CacheScope,
    instructions: Option[String] = None,
    resultType: ResultType = ResultType.Complete,
    _meta: Option[Map[String, Json]] = None
) derives Codec:
  /** [[supportedVersions]] parsed into [[ProtocolVersion]] values: `Right` for a revision this build recognises, `Left` with the raw string
    * for one it does not (e.g. a newer revision). The wire keeps the raw strings, so an unknown version never fails to decode.
    */
  def parsedSupportedVersions: List[Either[String, ProtocolVersion]] =
    supportedVersions.map(version => ProtocolVersion.from(version).toRight(version))
