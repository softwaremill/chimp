# Prompts

- Use `prompt(name)` to start defining a [prompt](https://modelcontextprotocol.io/specification/2025-11-25/server/prompts).
- Add a description and declare the arguments it accepts.
- Provide the logic that turns the supplied argument values into a `GetPromptResult`:
  - `handle` — synchronous logic from the argument values to `GetPromptResult`.
  - `handleWithHeaders` — synchronous logic that also receives the request headers.
  - `serverLogic` — effectful logic, with the request headers.
- Register prompts with `.addPrompt` / `.addPrompts`.

```scala mdoc:compile-only
import chimp.server.*
import chimp.protocol.{GetPromptResult, PromptMessage, Role, ToolContent}

val greeting = prompt("greeting")
  .description("Greets a person")
  .argument("name", required = true)
  .handle: args =>
    val name = args.getOrElse("name", "world")
    GetPromptResult(messages = List(PromptMessage(Role.User, ToolContent.Text(text = s"Hello, $name!"))))

val endpoint = McpServer(prompts = List(greeting)).endpoint(List("mcp"))
```

Prompt messages reuse the `ToolContent` content types, so they can embed images (`ToolContent.Image`) and resources (`ToolContent.ResourceContent`) just like tool results.
