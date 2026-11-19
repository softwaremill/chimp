# Resources

- Use `resource(uri)` for a fixed [resource](https://modelcontextprotocol.io/specification/2025-11-25/server/resources), or `resourceTemplate(uriTemplate)` for a `{variable}` URI template.
- Add metadata: `name`, `title`, `description`, `mimeType`, `size`.
- Provide the read logic, returning `Either[ResourceError, List[ResourceContents]]`:
  - `handle` — synchronous read logic.
  - `handleWithHeaders` — synchronous read logic that also receives the request headers.
  - `serverLogic` — effectful read logic, with the request headers.
  - `handleSecured` (synchronous) or `securedServerLogic` (effectful) — logic that also receives the principal made by the server's security logic; usable only on a secured server — see [transport security](transport.md).
- Register with `.addResource` / `.addResourceTemplate`. Subscriptions are wired with `.withSubscriptions`; they serve legacy clients only, as `2026-07-28` removed `resources/{,un}subscribe` (see [protocol versions in server capabilities](capabilities.md)).
- On `2026-07-28`, a client subscribes to resources with the `resourceSubscriptions` filter of `subscriptions/listen`, which replaces `resources/subscribe`. Send the updates with `NotificationPublisher.resourceUpdated(uri)`; the server sends each update only to the streams which list its URI (see [publishing list changes and resource updates](capabilities.md#publishing-list-changes-and-resource-updates)).

```scala mdoc:compile-only
import chimp.server.*
import chimp.protocol.ResourceContents

val readme = resource("file:///readme.txt")
  .name("readme")
  .mimeType("text/plain")
  .handle(() => Right(List(ResourceContents.Text(uri = "file:///readme.txt", text = "Hello!", mimeType = Some("text/plain")))))

val item = resourceTemplate("item://{id}")
  .name("item")
  .handle((vars, uri) => Right(List(ResourceContents.Text(uri = uri, text = s"item ${vars("id")}"))))

val endpoint = McpServer(resources = List(readme), resourceTemplates = List(item)).endpoint(List("mcp"))
```

Returning `Left(ResourceError(...))` (or reading an unknown URI) responds with a JSON-RPC error carrying the offending `uri`.
