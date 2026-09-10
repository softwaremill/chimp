package chimp.server

import chimp.client.{McpAuthorizationException, McpClient}
import chimp.client.transport.ClientHttpTransport
import chimp.protocol.{Implementation, ResourceContents, ToolContent}
import io.circe.{Codec, Json}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ox.supervised
import sttp.client4.*
import sttp.model.{Header, StatusCode}
import sttp.shared.Identity
import sttp.tapir.*
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.netty.sync.NettySyncServer

class SecuredHttpMcpServerSpec extends AnyFlatSpec with Matchers:
  private case class EchoInput(message: String) derives Codec, Schema
  private case class User(email: String)

  private val clientInfo = Implementation("chimp-server-test", "0.0.1")
  private val validToken = "s3cret"

  private val echoTool = tool("echo")
    .description("Echoes a message.")
    .input[EchoInput]
    .handle(in => ToolResult.text(in.message))

  private val whoAmITool = tool("whoAmI")
    .description("Echoes a message and the caller's email.")
    .input[EchoInput]
    .handleSecured[User]((in, user) => ToolResult.text(s"${in.message} ${user.email}"))

  private val greetingResource = resource("test://greeting")
    .name("greeting")
    .mimeType("text/plain")
    .handle(() => Right(List(ResourceContents.Text(uri = "test://greeting", text = "hello", mimeType = Some("text/plain")))))

  private def securityLogic(token: String): Either[String, User] =
    if token == validToken then Right(User("employee@example.com")) else Left("Invalid token")

  private def securityLogicEffectful(token: String): Identity[Either[String, User]] = securityLogic(token)

  private val securedServer = McpServer[Identity]()
    .addTool(echoTool)
    .serverSecurityLogicPure(auth.bearer[String](), statusCode(StatusCode.Unauthorized).and(stringBody))(securityLogic)
    .addTool(whoAmITool)

  private val securedServerWithEffectfulSecurityLogic = McpServer[Identity]()
    .addTool(echoTool)
    .serverSecurityLogic(auth.bearer[String](), statusCode(StatusCode.Unauthorized).and(stringBody))(securityLogicEffectful)
    .addTool(whoAmITool)

  private val serverConfiguredAfterSecurity = McpServer[Identity]()
    .serverSecurityLogicPure(auth.bearer[String](), statusCode(StatusCode.Unauthorized).and(stringBody))(securityLogic)
    .name("secured-server")
    .version("2.0.0")
    .addResource(greetingResource)
    .addTool(whoAmITool)

  private def withServer[T](mcpEndpoint: ServerEndpoint[Any, Identity])(test: (Int, SyncBackend) => T): T =
    supervised:
      val binding = NettySyncServer().port(0).addEndpoint(mcpEndpoint).start()
      try
        val backend = DefaultSyncBackend()
        try test(binding.port, backend)
        finally backend.close()
      finally binding.stop()

  private def withClient[T](port: Int, backend: SyncBackend, token: String)(test: McpClient[Identity] => T): T =
    val transport =
      ClientHttpTransport[Identity](backend, uri"http://localhost:$port/mcp", headers = List(Header.authorization("Bearer", token)))
    try test(McpClient(transport, clientInfo))
    finally transport.close()

  private val securedEndpoint = securedServer.endpoint(List("mcp"))
  private val endpointConfiguredAfterSecurity = serverConfiguredAfterSecurity.endpoint(List("mcp"))
  private val endpointWithEffectfulSecurityLogic = securedServerWithEffectfulSecurityLogic.endpoint(List("mcp"))

  "a secured MCP server" should "give the principal to the tool logic" in withServer(securedEndpoint): (port, backend) =>
    withClient(port, backend, validToken): client =>
      val result = client.callTool("whoAmI", Json.obj("message" -> Json.fromString("hi")))
      result.isError shouldBe false
      result.content shouldBe List(ToolContent.Text("text", "hi employee@example.com"))

  it should "give the principal to the tool logic when the security logic is effectful" in
    withServer(endpointWithEffectfulSecurityLogic): (port, backend) =>
      withClient(port, backend, validToken): client =>
        val result = client.callTool("whoAmI", Json.obj("message" -> Json.fromString("hi")))
        result.isError shouldBe false
        result.content shouldBe List(ToolContent.Text("text", "hi employee@example.com"))

  it should "reject an invalid security input with the error output when the security logic is effectful" in
    withServer(endpointWithEffectfulSecurityLogic): (port, backend) =>
      val exception = intercept[McpAuthorizationException](withClient(port, backend, "wrong")(_ => ()))
      exception.statusCode shouldBe StatusCode.Unauthorized.code

  it should "also serve the tools which do not need the principal" in withServer(securedEndpoint): (port, backend) =>
    withClient(port, backend, validToken): client =>
      client.listTools().tools.map(_.name) should contain allOf ("echo", "whoAmI")
      client.callTool("echo", Json.obj("message" -> Json.fromString("hi"))).content shouldBe List(ToolContent.Text("text", "hi"))

  it should "use the identity which the builders set after the security logic" in
    withServer(endpointConfiguredAfterSecurity): (port, backend) =>
      withClient(port, backend, validToken): client =>
        client.serverInfo shouldBe Implementation("secured-server", "2.0.0")

  it should "serve a resource which was added after the security logic" in
    withServer(endpointConfiguredAfterSecurity): (port, backend) =>
      withClient(port, backend, validToken): client =>
        client.serverCapabilities.resources shouldBe defined
        client.listResources().resources.map(_.uri) shouldBe List("test://greeting")
        client.readResource("test://greeting").contents.head match
          case ResourceContents.Text(_, text, _, _) => text shouldBe "hello"
          case other                                => fail(s"expected text contents, got $other")

  it should "reject an invalid security input with the error output, before any tool logic runs" in
    withServer(securedEndpoint): (port, backend) =>
      val exception = intercept[McpAuthorizationException](withClient(port, backend, "wrong")(_ => ()))
      exception.statusCode shouldBe StatusCode.Unauthorized.code

  it should "reject a request with no security input with the error output" in withServer(securedEndpoint): (port, backend) =>
    val response = basicRequest
      .post(uri"http://localhost:$port/mcp")
      .header("Content-Type", "application/json")
      .body("""{"jsonrpc":"2.0","id":"1","method":"tools/call","params":{"name":"whoAmI","arguments":{"message":"hi"}}}""")
      .send(backend)
    response.code shouldBe StatusCode.Unauthorized
    response.body shouldBe Left("Invalid value for: header Authorization (missing)")
