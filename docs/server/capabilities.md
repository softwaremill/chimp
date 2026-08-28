# Server capabilities

Most tools just answer a request, so `serverLogic`/`handle` expose no context. A tool that needs to **push to the client while it runs** uses `streamingServerLogic`, which receives a `StreamingServerContext[F]`:

- `reportProgress` — [progress](https://modelcontextprotocol.io/specification/2025-11-25/basic/utilities/progress) notifications, auto-wired to the request's progress token.
- `log` — [logging](https://modelcontextprotocol.io/specification/2025-11-25/server/utilities/logging) notifications.

```{note}
Pushing to the client requires an open stream, so a `streamingServerLogic` tool is registered with `addStreamingTool` on a `StreamingMcpServer`, and will not compile on the plain request/response endpoint.
```

```scala mdoc:compile-only
import chimp.server.*
import chimp.protocol.LoggingLevel
import io.circe.{Codec, Json}
import sttp.shared.Identity
import sttp.tapir.*

case class WorkInput(steps: Int) derives Codec, Schema

val work = tool("work")
  .input[WorkInput]
  .streamingServerLogic[Identity]: (_, ctx, _) =>
    ctx.reportProgress(0.5, total = Some(1.0))
    ctx.log(LoggingLevel.Info, Json.fromString("halfway"))
    ToolResult.text("done")

val server = StreamingMcpServer[Identity]().addStreamingTool(work)
```

Server-wide capabilities are enabled by registering a handler — only what you wire up is advertised: `.withCompletion`, `.withLoggingLevel`, `.withSubscriptions`.

## Tasks (experimental)

With the [Tasks extension](https://github.com/modelcontextprotocol/modelcontextprotocol/blob/main/seps/2663-tasks-extension.md) (`io.modelcontextprotocol/tasks`) the server answers a long-running `tools/call` with a durable task handle instead of blocking. The client (which must declare the extension in its per-request `_meta`) then polls with `tasks/get` and collects the result once the task is `completed`.

Enable it with `.withTasks`, providing a `TaskStore` and a `TaskExecutor` for the effect type. The synchronous server uses a thread-pool executor:

```scala mdoc:compile-only
import chimp.server.*
import io.circe.Codec
import sttp.monad.{IdentityMonad, MonadError}
import sttp.shared.Identity
import sttp.tapir.Schema

given MonadError[Identity] = IdentityMonad

case class ReportInput(days: Int) derives Codec, Schema

val report = tool("report").input[ReportInput].handle(in => ToolResult.text(s"report for ${in.days} days"))

val server = McpServer(tools = List(report))
  .withTasks(TaskSupport(TaskStore.inMemory[Identity], TaskExecutor.threadPool()))
```

The server runs the tool in the background, transitions the task to `completed` (with the tool's `CallToolResult`) or `failed`, and answers `tasks/cancel` by interrupting the worker. `useTask` and `requireTask` on `TaskSupport` control, per tool, whether a task is offered or required; a required task with no client support fails with `-32003`. Full server-initiated `input_required` flows are not yet implemented.
