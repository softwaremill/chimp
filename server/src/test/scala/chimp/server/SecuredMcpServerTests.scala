package chimp.server

import chimp.client.{McpAuthorizationException, McpClient}
import chimp.protocol.{Implementation, ResourceContents, ToolContent}
import io.circe.{Codec, Json}
import org.scalatest.{Assertion, RecoverMethods}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.monad.syntax.*
import sttp.model.StatusCode
import sttp.tapir.*

import scala.concurrent.Future

trait SecuredMcpServerTests[F[_]] extends AsyncFlatSpec with Matchers with RecoverMethods with SecuredMcpServerTestFixtures[F]:
  this: ToFuture[F] =>

  protected def withSecuredServer(server: SecuredMcpServer[F, String, String, User], token: String = validToken)(
      test: McpClient[F] => F[Assertion]
  ): Future[Assertion]

  private case class EchoInput(message: String) derives Codec, Schema

  private def securityLogic(token: String): Either[String, User] =
    if token == validToken then Right(User("employee@example.com")) else Left("Invalid token")

  private def securityLogicEffectful(token: String): F[Either[String, User]] =
    monad.unit(securityLogic(token))

  private def echoTool: ServerTool[EchoInput, NoStructuredOutput, F, ServerContext[F]] =
    tool("echo")
      .description("Echoes a message.")
      .input[EchoInput]
      .serverLogic[F]((in, _) => monad.unit(ToolResult.text(in.message)))

  private def whoAmITool: ServerTool[EchoInput, NoStructuredOutput, F, SecuredServerContext[F, User]] =
    tool("whoAmI")
      .description("Echoes a message and the caller's email.")
      .input[EchoInput]
      .securedServerLogic[F, User]((in, user, _) => monad.unit(ToolResult.text(s"${in.message} ${user.email}")))

  private def greetingResource: ServerResource[F] =
    resource("test://greeting")
      .name("greeting")
      .mimeType("text/plain")
      .serverLogic[F](_ =>
        monad.unit(Right(List(ResourceContents.Text(uri = "test://greeting", text = "hello", mimeType = Some("text/plain")))))
      )

  private def securedServer: SecuredMcpServer[F, String, String, User] =
    McpServer[F]()
      .addTool(echoTool)
      .serverSecurityLogicPure(auth.bearer[String](), statusCode(StatusCode.Unauthorized).and(stringBody))(securityLogic)
      .addTool(whoAmITool)

  private def securedServerWithEffectfulSecurityLogic: SecuredMcpServer[F, String, String, User] =
    McpServer[F]()
      .addTool(echoTool)
      .serverSecurityLogic(auth.bearer[String](), statusCode(StatusCode.Unauthorized).and(stringBody))(securityLogicEffectful)
      .addTool(whoAmITool)

  private def serverConfiguredAfterSecurity: SecuredMcpServer[F, String, String, User] =
    McpServer[F]()
      .serverSecurityLogicPure(auth.bearer[String](), statusCode(StatusCode.Unauthorized).and(stringBody))(securityLogic)
      .name("secured-server")
      .version("2.0.0")
      .addResource(greetingResource)
      .addTool(whoAmITool)

  private def assertWhoAmIDeliversPrincipal(client: McpClient[F]): F[Assertion] =
    client
      .callTool("whoAmI", Json.obj("message" -> Json.fromString("hi")))
      .map: result =>
        result.isError shouldBe false
        result.content shouldBe List(ToolContent.Text("text", "hi employee@example.com"))

  "a secured MCP server" should "give the principal to the tool logic" in
    withSecuredServer(securedServer)(assertWhoAmIDeliversPrincipal)

  it should "give the principal to the tool logic when the security logic is effectful" in
    withSecuredServer(securedServerWithEffectfulSecurityLogic)(assertWhoAmIDeliversPrincipal)

  it should "also serve tools which do not need the principal" in
    withSecuredServer(securedServer): client =>
      client
        .listTools()
        .flatMap: tools =>
          tools.tools.map(_.name) should contain allOf ("echo", "whoAmI")
          client
            .callTool("echo", Json.obj("message" -> Json.fromString("hi")))
            .map: result =>
              result.content shouldBe List(ToolContent.Text("text", "hi"))

  it should "use the identity which the builders set after the security logic" in
    withSecuredServer(serverConfiguredAfterSecurity): client =>
      monad.unit(client.serverInfo shouldBe Implementation("secured-server", "2.0.0"))

  it should "serve a resource which was added after the security logic" in
    withSecuredServer(serverConfiguredAfterSecurity): client =>
      client.serverCapabilities.resources shouldBe defined
      client
        .listResources()
        .flatMap: listed =>
          listed.resources.map(_.uri) shouldBe List("test://greeting")
          client
            .readResource("test://greeting")
            .map: result =>
              result.contents.head match
                case ResourceContents.Text(_, text, _, _) => text shouldBe "hello"
                case other                                => fail(s"expected text contents, got $other")

  it should "reject an invalid security input with HTTP 401 before any tool logic runs" in
    recoverToExceptionIf[McpAuthorizationException] {
      Future(withSecuredServer(securedServer, token = "wrong")(_ => monad.unit(succeed))).flatten
    }.map(_.statusCode shouldBe StatusCode.Unauthorized.code)

  it should "reject invalid security input with HTTP 401 when the security logic is effectful" in
    recoverToExceptionIf[McpAuthorizationException] {
      Future(withSecuredServer(securedServerWithEffectfulSecurityLogic, token = "wrong")(_ => monad.unit(succeed))).flatten
    }.map(_.statusCode shouldBe StatusCode.Unauthorized.code)
