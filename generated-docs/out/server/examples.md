# Examples

Each example builds an `McpServer` (or `StreamingMcpServer`) and serves it over a transport. The sync HTTP example uses `chimp-server`; the ZIO, Ox and Pekko examples additionally use the integration module for that effect system.

## HTTP server

A synchronous server exposed with the Tapir Netty interpreter:

```scala
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

```scala
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

```scala
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

```scala
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

## Streaming HTTP server (Ox)

The same streaming server in direct style, served with `OxServerHttpTransport` on `tapir-netty-server-sync`:

```scala
import chimp.server.{StreamingMcpServer, ToolResult, tool}
import chimp.server.ox.OxServerHttpTransport
import chimp.protocol.LoggingLevel
import io.circe.{Codec, Json}
import sttp.shared.Identity
import sttp.tapir.*
import sttp.tapir.server.netty.sync.NettySyncServer

case class OxProgressInput(steps: Int) derives Codec, Schema

object StreamingOxServer:
  def main(args: Array[String]): Unit =
    val work = tool("work").input[OxProgressInput].streamingServerLogic[Identity]: (_, ctx, _) =>
      ctx.reportProgress(0.5, total = Some(1.0))
      ctx.log(LoggingLevel.Info, Json.fromString("halfway"))
      ToolResult.text("done")
    val server = StreamingMcpServer[Identity]().withLoggingLevel(_ => ()).addStreamingTool(work)
    val endpoint = OxServerHttpTransport(List("mcp")).serve(server)
    NettySyncServer().port(8080).addEndpoint(endpoint).startAndWait()
```

## STDIO server (Ox)

A direct-style server exchanging line-delimited JSON-RPC over stdin/stdout, served with `OxServerStdioTransport`:

```scala
import chimp.server.{StreamingMcpServer, ToolResult, tool}
import chimp.server.ox.OxServerStdioTransport
import io.circe.Codec
import sttp.shared.Identity
import sttp.tapir.*

case class OxEchoInput(message: String) derives Codec, Schema

object StdioOxServer:
  def main(args: Array[String]): Unit =
    val echo = tool("echo").input[OxEchoInput].handle(in => ToolResult.text(in.message))
    val server = StreamingMcpServer[Identity]().addTool(echo)
    OxServerStdioTransport().serve(server)
```

## Streaming HTTP server (Pekko)

The same streaming server with `F = Future`, served with `PekkoServerHttpTransport` on `tapir-pekko-http-server`:

```scala
import chimp.protocol.LoggingLevel
import chimp.server.pekko.PekkoServerHttpTransport
import chimp.server.{StreamingMcpServer, ToolResult, tool}
import io.circe.{Codec, Json}
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.Http
import sttp.tapir.*
import sttp.tapir.server.pekkohttp.PekkoHttpServerInterpreter

import scala.concurrent.{ExecutionContext, Future}

case class PekkoProgressInput(steps: Int) derives Codec, Schema

object StreamingPekkoServer:
  def main(args: Array[String]): Unit =
    given system: ActorSystem = ActorSystem("mcp")
    given ExecutionContext = system.dispatcher

    val work = tool("work").input[PekkoProgressInput].streamingServerLogic[Future]: (_, ctx, _) =>
      for
        _ <- ctx.reportProgress(0.5, total = Some(1.0))
        _ <- ctx.log(LoggingLevel.Info, Json.fromString("halfway"))
      yield ToolResult.text("done")
    val server = StreamingMcpServer[Future]().withLoggingLevel(_ => Future.unit).addStreamingTool(work)
    val endpoint = PekkoServerHttpTransport(List("mcp")).serve(server)
    val _ = Http().newServerAt("localhost", 8080).bind(PekkoHttpServerInterpreter().toRoute(endpoint))
```

## STDIO server (Pekko)

A server exchanging line-delimited JSON-RPC over stdin/stdout, served with `PekkoServerStdioTransport`. The effect it returns completes when the standard input ends:

```scala
import chimp.server.pekko.PekkoServerStdioTransport
import chimp.server.{StreamingMcpServer, ToolResult, tool}
import io.circe.Codec
import org.apache.pekko.actor.ActorSystem
import sttp.tapir.*

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

case class PekkoEchoInput(message: String) derives Codec, Schema

object StdioPekkoServer:
  def main(args: Array[String]): Unit =
    given ActorSystem = ActorSystem("mcp")

    val echo = tool("echo").input[PekkoEchoInput].serverLogic[Future]((in, _) => Future.successful(ToolResult.text(in.message)))
    val server = StreamingMcpServer[Future]().addTool(echo)
    Await.result(PekkoServerStdioTransport().serve(server), Duration.Inf)
```

More runnable examples live in [`examples/`](https://github.com/softwaremill/chimp/tree/master/examples/src/main/scala/examples).
