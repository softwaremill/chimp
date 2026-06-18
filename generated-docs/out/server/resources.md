# Resources

- Use `resource(uri)` for a fixed [resource](https://modelcontextprotocol.io/specification/2025-11-25/server/resources), or `resourceTemplate(uriTemplate)` for a `{variable}` URI template.
- Add metadata: `name`, `title`, `description`, `mimeType`, `size`.
- Provide the read logic, returning `Either[ResourceError, List[ResourceContents]]`:
  - `handle` — synchronous; `handleWithHeaders` — synchronous, with headers; `serverLogic` — effectful.
- Register with `.addResource` / `.addResourceTemplate`. Subscriptions are wired with `.withSubscriptions`.

```scala
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
