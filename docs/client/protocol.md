# Protocol versions

chimp's client speaks the legacy protocol versions (`2025-06-18`, `2025-11-25`) and the stateless version `2026-07-28`. You set the version once, on the client. The client gives it to the transport, so a transport has no version parameter.

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
