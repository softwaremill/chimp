package chimp.server

import chimp.client.BidirectionalMcpClient
import chimp.client.notifications.ServerNotification
import chimp.protocol.*
import io.circe.{Codec, Json}
import org.scalatest.Assertion
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.monad.syntax.*
import sttp.tapir.Schema

import java.util.concurrent.ConcurrentLinkedQueue
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

/** Tests for server→client streaming behavior: notifications emitted by tool logic mid-call must reach the client over the open SSE stream
  * before the call's final result. Run only by streaming-capable backends.
  */
trait StreamingMcpServerTests[F[_]] extends AsyncFlatSpec with Matchers:
  this: ToFuture[F] =>

  protected def withStreamingServer(server: StreamingMcpServer[F])(test: BidirectionalMcpClient[F] => F[Assertion]): Future[Assertion]

  private case class NoInput() derives Codec, Schema

  protected def streamingServer: StreamingMcpServer[F] =
    StreamingMcpServer[F]()
      .withLogging(_ => monad.unit(()))
      .addStreamingTool(
        tool("noisy")
          .description("Logs several messages, then returns")
          .input[NoInput]
          .streamingServerLogic[F]: (_, ctx, _) =>
            ctx
              .log(LoggingLevel.Info, Json.fromString("one"))
              .flatMap(_ => ctx.log(LoggingLevel.Info, Json.fromString("two")))
              .flatMap(_ => ctx.log(LoggingLevel.Info, Json.fromString("three")))
              .map(_ => ToolResult.text("done"))
      )

  "a streaming MCP server" should "deliver log notifications emitted during a tool call" in
    withStreamingServer(streamingServer): client =>
      val messages = ConcurrentLinkedQueue[Json]()
      val listener: ServerNotification => F[Unit] = {
        case ServerNotification.LoggingMessage(params) => messages.add(params.data); monad.unit(())
        case _                                         => monad.unit(())
      }
      client
        .onServerNotification(n => listener(n))
        .flatMap(_ => client.callTool("noisy", Json.obj()))
        .flatMap: result =>
          waitUntil(messages.size >= 3).map: _ =>
            result.content shouldBe List(ToolContent.Text("text", "done"))
            messages.asScala.toList shouldBe List(Json.fromString("one"), Json.fromString("two"), Json.fromString("three"))
