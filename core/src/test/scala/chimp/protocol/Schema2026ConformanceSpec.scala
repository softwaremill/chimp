package chimp.protocol

import io.circe.syntax.*

import scala.concurrent.duration.*

/** Schema conformance for the modern 2026-07-28 revision: the defs it adds or changes. */
class Schema2026ConformanceSpec extends SchemaConformance:

  override def schemaResourcePath: String = "/schema/2026-07-28/schema.json"

  it should "produce a DiscoverResult that matches the spec schema" in:
    validate(
      "DiscoverResult",
      DiscoverResult(
        supportedVersions = List("2026-07-28", "2025-11-25"),
        capabilities = ServerCapabilities(tools = Some(ServerToolsCapability(listChanged = Some(false)))),
        ttlMs = 0.millis,
        cacheScope = CacheScope.Private,
        instructions = Some("welcome"),
        _meta = Some(Map(ProtocolMeta.ServerInfo -> Implementation(name = "chimp", version = "1.0").asJson.deepDropNullValues))
      )
    )

  it should "produce an UnsupportedProtocolVersionError envelope that matches the spec schema" in:
    val msg: JSONRPCMessage =
      JSONRPCMessage.Error(id = RequestId(1), error = ProtocolMeta.unsupportedVersionError("1900-01-01", List("2026-07-28", "2025-11-25")))
    validate("UnsupportedProtocolVersionError", msg)
