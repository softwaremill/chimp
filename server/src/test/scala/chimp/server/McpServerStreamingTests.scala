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

trait McpServerStreamingTests[F[_]] extends AsyncFlatSpec with Matchers:
  this: ToFuture[F] =>

  protected def withStreamingServer(server: StreamingMcpServer[F])(test: BidirectionalMcpClient[F] => F[Assertion]): Future[Assertion]

  /** As [[withStreamingServer]], with a client that has the `logLevel` option. */
  protected def withLoggingStreamingServer(server: StreamingMcpServer[F], logLevel: LoggingLevel)(
      test: BidirectionalMcpClient[F] => F[Assertion]
  ): Future[Assertion]

  /** The protocol version of the spec's client. A modern client sets no `_meta` logLevel, so it must get no log notifications. */
  protected def clientProtocolVersion: ProtocolVersion

  private case class NoInput() derives Codec, Schema

  protected def streamingServer: StreamingMcpServer[F] =
    StreamingMcpServer[F]()
      .withLoggingLevel(_ => monad.unit(()))
      .addStreamingTool(
        tool("noisy")
          .description("Logs several messages, then returns")
          .input[NoInput]
          .streamingServerLogic[F]: (_, ctx, _) =>
            for
              _ <- ctx.log(LoggingLevel.Info, Json.fromString("one"))
              _ <- ctx.log(LoggingLevel.Info, Json.fromString("two"))
              _ <- ctx.log(LoggingLevel.Info, Json.fromString("three"))
            yield ToolResult.text("done")
      )

  private case class RegionInput(region: String, limit: Option[Int]) derives Codec, Schema

  private val headerServer: StreamingMcpServer[F] =
    McpServer[F]()
      .addTool(
        tool("region")
          .input[RegionInput]
          .withMcpHeader("region", "Region")
          .withMcpHeader("limit", "Limit")
          .serverLogic[F]((in, _) => monad.unit(ToolResult.text(s"region ${in.region} ${in.limit}")))
      )
      .streaming

  "a streaming MCP server" should "serve a client that mirrors x-mcp-header arguments into Mcp-Param headers, with Base64 values" in
    withStreamingServer(headerServer): client =>
      client
        .listTools()
        .flatMap(_ => client.callTool("region", Json.obj("region" -> Json.fromString(" Zürich"), "limit" -> Json.fromInt(5))))
        .flatMap: first =>
          client
            .callTool("region", Json.obj("region" -> Json.fromString("eu"), "limit" -> Json.Null))
            .map: second =>
              List(first, second).map(_.isError) shouldBe List(false, false)
              first.content shouldBe List(ToolContent.Text("text", "region  Zürich Some(5)"))
              second.content shouldBe List(ToolContent.Text("text", "region eu None"))

  it should "deliver log notifications to a client with the log level option, at a legacy and a modern version" in
    withLoggingStreamingServer(streamingServer, LoggingLevel.Info): client =>
      val messages = ConcurrentLinkedQueue[Json]()
      client
        .onServerNotification:
          case ServerNotification.LoggingMessage(params) => messages.add(params.data); monad.unit(())
          case _                                         => monad.unit(())
        .flatMap(_ => client.callTool("noisy", Json.obj()))
        .flatMap: result =>
          waitUntil(messages.size >= 3).map: _ =>
            result.content shouldBe List(ToolContent.Text("text", "done"))
            messages.asScala.toList shouldBe List(Json.fromString("one"), Json.fromString("two"), Json.fromString("three"))

  it should "deliver log notifications emitted during a tool call, but none to a modern request without a log level" in
    withStreamingServer(streamingServer): client =>
      val messages = ConcurrentLinkedQueue[Json]()
      val listener: ServerNotification => F[Unit] = {
        case ServerNotification.LoggingMessage(params) => messages.add(params.data); monad.unit(())
        case _                                         => monad.unit(())
      }
      client
        .onServerNotification(notification => listener(notification))
        .flatMap(_ => client.callTool("noisy", Json.obj()))
        .flatMap: result =>
          val expected =
            if clientProtocolVersion.isModern then Nil else List(Json.fromString("one"), Json.fromString("two"), Json.fromString("three"))
          waitUntil(messages.size >= expected.size)
            .flatMap(_ => sleep(50))
            .map: _ =>
              result.content shouldBe List(ToolContent.Text("text", "done"))
              messages.asScala.toList shouldBe expected
