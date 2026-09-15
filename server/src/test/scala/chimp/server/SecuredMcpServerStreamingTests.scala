package chimp.server

import chimp.client.{BidirectionalMcpClient, McpAuthorizationException}
import chimp.client.notifications.ServerNotification
import chimp.protocol.*
import io.circe.{Codec, Json}
import org.scalatest.{Assertion, RecoverMethods}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.model.StatusCode
import sttp.monad.syntax.*
import sttp.tapir.*

import java.util.concurrent.ConcurrentLinkedQueue
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

trait SecuredMcpServerTestFixtures[F[_]]:
  protected case class User(email: String)

  protected val validToken = "s3cret"

trait SecuredMcpServerStreamingTests[F[_]] extends AsyncFlatSpec with Matchers with RecoverMethods with SecuredMcpServerTestFixtures[F]:
  this: ToFuture[F] =>

  /** @param token
    *   The bearer token the client authenticates with; defaults to [[validToken]] so most tests need not pass it.
    */
  protected def withSecuredStreamingServer(server: SecuredStreamingMcpServer[F, String, String, User], token: String = validToken)(
      test: BidirectionalMcpClient[F] => F[Assertion]
  ): Future[Assertion]

  private case class NoInput() derives Codec, Schema

  private def securityLogic(token: String): Either[String, User] =
    if token == validToken then Right(User("employee@example.com")) else Left("Invalid token")

  private def securityLogicEffectful(token: String): F[Either[String, User]] =
    monad.unit(securityLogic(token))

  private def whoAmITool: ServerTool[NoInput, NoStructuredOutput, F, SecuredStreamingServerContext[F, User]] =
    tool("whoAmI")
      .description("Logs the caller's email, then returns it")
      .input[NoInput]
      .securedStreamingServerLogic[F, User]: (_, user, ctx, _) =>
        ctx.log(LoggingLevel.Info, Json.fromString(user.email)).map(_ => ToolResult.text(user.email))

  protected def securedStreamingServer: SecuredStreamingMcpServer[F, String, String, User] =
    McpServer[F]()
      .serverSecurityLogicPure(auth.bearer[String](), statusCode(StatusCode.Unauthorized).and(stringBody))(securityLogic)
      .withLoggingLevel(_ => monad.unit(()))
      .streaming
      .addStreamingTool(whoAmITool)

  protected def securedStreamingServerWithEffectfulSecurityLogic: SecuredStreamingMcpServer[F, String, String, User] =
    McpServer[F]()
      .serverSecurityLogic(auth.bearer[String](), statusCode(StatusCode.Unauthorized).and(stringBody))(securityLogicEffectful)
      .withLoggingLevel(_ => monad.unit(()))
      .streaming
      .addStreamingTool(whoAmITool)

  private def assertWhoAmIDeliversPrincipal(client: BidirectionalMcpClient[F]): F[Assertion] =
    val messages = ConcurrentLinkedQueue[Json]()
    val listener: ServerNotification => F[Unit] = {
      case ServerNotification.LoggingMessage(params) => messages.add(params.data); monad.unit(())
      case _                                         => monad.unit(())
    }
    client
      .onServerNotification(notification => listener(notification))
      .flatMap(_ => client.callTool("whoAmI", Json.obj()))
      .flatMap: result =>
        waitUntil(messages.size >= 1).map: _ =>
          result.content shouldBe List(ToolContent.Text("text", "employee@example.com"))
          messages.asScala.toList shouldBe List(Json.fromString("employee@example.com"))

  "a secured streaming MCP server" should "give the principal to a streaming tool, and deliver its log notifications" in
    withSecuredStreamingServer(securedStreamingServer)(assertWhoAmIDeliversPrincipal)

  it should "give the principal to a streaming tool when the security logic is effectful" in
    withSecuredStreamingServer(securedStreamingServerWithEffectfulSecurityLogic)(assertWhoAmIDeliversPrincipal)

  it should "reject an invalid security input with the error output, before any tool logic runs" in
    recoverToExceptionIf[McpAuthorizationException] {
      Future(withSecuredStreamingServer(securedStreamingServer, token = "wrong")(_ => monad.unit(succeed))).flatten
    }.map(_.statusCode shouldBe StatusCode.Unauthorized.code)
