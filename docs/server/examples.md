# Examples

Each example builds an `McpServer` (or `StreamingMcpServer`) and serves it over a transport. The sync HTTP example uses `chimp-server`; the ZIO examples additionally use `chimp-server-zio`.

## HTTP server

A synchronous server exposed with the Tapir Netty interpreter:

```scala mdoc:compile-only
import chimp.server.*
import io.circe.Codec
import sttp.tapir.*
import sttp.tapir.server.netty.sync.NettySyncServer

case class SyncAddInput(a: Int, b: Int) derives Codec, Schema

object HttpSyncServer:
  def main(args: Array[String]): Unit =
    val adder = tool("adder").description("Adds two numbers").input[SyncAddInput].handle(in => ToolResult.text(s"The result is ${in.a + in.b}"))
    val endpoint = McpServer(tools = List(adder)).endpoint(List("mcp"))
    NettySyncServer().port(8080).addEndpoint(endpoint).startAndWait()
```

## HTTP server (ZIO)

The Tapir-ZIO integration requires a `RIO[R, A]` effect (error channel fixed to `Throwable`), so the effect type is stated explicitly:

```scala mdoc:compile-only
import chimp.server.{McpServer, ToolResult, tool}
import io.circe.Codec
import sttp.tapir.*
import sttp.tapir.server.ziohttp.ZioHttpInterpreter
import zio.{RIO, ZIO, ZIOAppDefault}
import zio.http.Server

case class ZioAddInput(a: Int, b: Int) derives Codec, Schema

object HttpZioServer extends ZIOAppDefault:
  val adder = tool("adder").description("Adds two numbers").input[ZioAddInput].serverLogic[[X] =>> RIO[Any, X]]: (in, _) =>
    ZIO.succeed(ToolResult.text(s"The result is ${in.a + in.b}"))
  val endpoint = McpServer(tools = List(adder)).endpoint(List("mcp"))
  override def run = Server.serve(ZioHttpInterpreter().toHttp(endpoint)).provide(Server.default)
```

## Streaming HTTP server (ZIO)

A streaming tool that pushes progress and log notifications over SSE while it runs, served with `ZioServerHttpTransport`:

```scala mdoc:compile-only
import chimp.server.{StreamingMcpServer, ToolResult, tool}
import chimp.server.zio.ZioServerHttpTransport
import chimp.protocol.LoggingLevel
import io.circe.{Codec, Json}
import sttp.tapir.*
import sttp.tapir.server.ziohttp.ZioHttpInterpreter
import zio.{Task, ZIO, ZIOAppDefault}
import zio.http.Server

case class ProgressInput(steps: Int) derives Codec, Schema

object StreamingZioServer extends ZIOAppDefault:
  val work = tool("work").input[ProgressInput].streamingServerLogic[Task]: (_, ctx, _) =>
    for
      _ <- ctx.reportProgress(0.5, total = Some(1.0))
      _ <- ctx.log(LoggingLevel.Info, Json.fromString("halfway"))
    yield ToolResult.text("done")
  val server = StreamingMcpServer[Task]().withLoggingLevel(_ => ZIO.unit).addStreamingTool(work)
  val endpoint = ZioServerHttpTransport(List("mcp")).serve(server)
  override def run = Server.serve(ZioHttpInterpreter().toHttp(endpoint)).provide(Server.default)
```

## STDIO server (ZIO)

A server that exchanges line-delimited JSON-RPC over stdin/stdout, served with `ZioServerStdioTransport`:

```scala mdoc:compile-only
import chimp.server.{StreamingMcpServer, ToolResult, tool}
import chimp.server.zio.ZioServerStdioTransport
import io.circe.Codec
import sttp.tapir.*
import zio.{Task, ZIO, ZIOAppDefault}

case class EchoInput(message: String) derives Codec, Schema

object StdioZioServer extends ZIOAppDefault:
  val echo = tool("echo").input[EchoInput].serverLogic[Task]((in, _) => ZIO.succeed(ToolResult.text(in.message)))
  val server = StreamingMcpServer[Task]().addTool(echo)
  override def run = ZioServerStdioTransport().serve(server)
```

More runnable examples live in [`examples/`](https://github.com/softwaremill/chimp/tree/master/examples/src/main/scala/examples).
