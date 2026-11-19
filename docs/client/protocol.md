# Protocol versions

chimp's client speaks the legacy protocol versions (`2025-06-18`, `2025-11-25`) and the stateless version `2026-07-28`. The `protocolVersion` parameter of `McpClient.apply` and `McpClient.bidirectional` sets the version policy, once, on the client. The client gives the version to the transport, so a transport has no version parameter.

- `DetectVersion`, the default: the client detects the era of the server (see below).
- A fixed `ProtocolVersion`: the client uses that version and does no detection. Pin a version when you know the server, or to skip the detection request.

```scala mdoc:compile-only
import chimp.client.*
import chimp.client.transport.ClientHttpTransport
import chimp.protocol.{Implementation, LoggingLevel, ProtocolVersion}
import sttp.client4.DefaultSyncBackend
import sttp.model.Uri.UriContext
import sttp.shared.Identity

object StatelessClient:
  def main(args: Array[String]): Unit =
    val backend = DefaultSyncBackend()
    val transport = ClientHttpTransport[Identity](backend, uri"http://localhost:8080/mcp")
    val client = McpClient[Identity](
      transport,
      Implementation("my-client", "0.1.0"),
      protocolVersion = ProtocolVersion.Latest,
      logLevel = Some(LoggingLevel.Info)
    )

    println(client.discover().supportedVersions)
    client.close()
```

## Era detection

With `DetectVersion`, the first request of the client is `server/discover` at `2026-07-28`, over HTTP and over STDIO:

- A `server/discover` result, or a `2026-07-28` error (`-32020`, `-32021` or `-32022`), means that the server is modern. The client uses the stateless wire and negotiates the version on `-32022` (see [Version negotiation](#version-negotiation)).
- Any other outcome means that the server is legacy: another JSON-RPC error, an HTTP error (for example 400, 404 or 405) whose body is not a `2026-07-28` error, a failed send, or a timeout. The client then sends `initialize` with the latest legacy version, `2025-11-25`.

The decision does not depend on one specific error code. The client makes it once, when it connects, and keeps it for its lifetime. `McpClient.protocolVersion` shows the result.

If a modern server lists no stateless version that chimp supports, but a common legacy version, the client sends `initialize` with that version. If the server shares no version with chimp, the client fails with `McpUnsupportedProtocolVersionException`, which names both version lists. If a legacy server rejects `initialize`, the client fails as with a fixed legacy version.

A legacy STDIO server that does not answer an unknown request makes the client wait for the timeout of the transport before it sends `initialize`. For such a server, pin a legacy version.

## Legacy versions

At a legacy version, the client sends `initialize` and `notifications/initialized`. It then uses the version that the server returns in the `initialize` result.

## The stateless version

At `2026-07-28` there is no handshake:

- The client sends `server/discover` first. `serverCapabilities` and `serverInfo` come from its result. If the result has no `serverInfo`, both of its values are `unknown`.
- Every request carries `_meta` with the protocol version, the client info and the client capabilities. The capabilities come from the handlers that you give to `McpClient.bidirectional`.
- Over HTTP, the `MCP-Protocol-Version` header always equals the version in `_meta`.
- `discover()` sends `server/discover` again and returns the full result. At a legacy version it fails with `McpProtocolException`.

## Version negotiation

When a server rejects a request with `-32022` (unsupported protocol version), the client reads the versions in the error. It sends the request again, one time, with the newest version in that list that chimp also supports on the stateless wire. Later requests use that version too; `McpClient.protocolVersion` shows the current version.

If the two lists have no common version, or the server rejects the second request too, the request fails with `McpUnsupportedProtocolVersionException`. Its message, `serverVersions` and `clientVersions` name both lists.

Over HTTP, the server sends `-32022` with HTTP 400. The HTTP transports give the JSON-RPC error in the body of an HTTP error response to the client as the response to the request. A body without a JSON-RPC error fails with `McpTransportException`.

## Results

A result without `resultType` is complete. A result with an unknown `resultType` fails with `McpProtocolException`.

A result with `resultType` `input_required` also fails with `McpProtocolException`: the client does not send the request again with the input yet.

## Operations for legacy versions only

`2026-07-28` removed these operations. At the stateless version they fail with `McpProtocolException` and the client sends nothing:

- `ping`
- `setLoggingLevel`
- `subscribeResource`, `unsubscribeResource`
- `sendProgress`
- `sendRootsListChanged`

## Log level

The `logLevel` option of `McpClient.apply` and `McpClient.bidirectional` sets the minimum level of the log messages (`notifications/message`) that the server sends:

- At the stateless version, every request carries the level in `_meta`. Without the option the client sends no level, and the server sends no log messages.
- At a legacy version, the client sends `logging/setLevel` once after `initialize`, if the server has the `logging` capability.
