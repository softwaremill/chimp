# Defining tools and server logic

- Use `tool(name)` to start defining a tool.
- Add a description and annotations for metadata and hints.
- Specify the input type (must have a Circe `Codec` and Tapir `Schema`).
- Provide the server logic as a function from input to `ToolResult` (or a generic effect type).
  - Use `handle` to connect the tool definition with the server logic when the use of headers is not required.
  - Use `handleWithHeaders` to connect the tool definition with the server logic when headers are required.
- Assemble your tools into an `McpServer` and call `.endpoint(path)` to create a Tapir endpoint.
- Start an HTTP server using your preferred Tapir server interpreter.
