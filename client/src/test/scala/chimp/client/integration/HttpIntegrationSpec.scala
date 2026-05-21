package chimp.client.integration

import chimp.client.McpClient
import chimp.client.transport.Transport
import chimp.protocol.{Implementation, ProtocolVersion, ToolContent}
import io.circe.Json
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Assertion, BeforeAndAfterAll}
import sttp.model.Uri
import sttp.monad.syntax.*

import scala.concurrent.Future

abstract class HttpIntegrationSpec[F[_], B] extends AsyncFlatSpec with Matchers with BeforeAndAfterAll with IntegrationSpec:
  this: ToFuture[F] =>

  protected val container: MCPEverythingContainer = new MCPEverythingContainer

  override def beforeAll(): Unit =
    super.beforeAll()
    container.start()

  override def afterAll(): Unit =
    try container.stop()
    finally super.afterAll()

  def usingBackend[A](use: B => F[A]): F[A]
  def usingTransport[A](backend: B, uri: Uri)(use: Transport[F] => F[A]): F[A]

  private val clientInfo = Implementation(name = "chimp-integration", version = "0.0.1")

  "an HTTP transport" should "expose server info from initialize" in withClient: client =>
    monad.unit(client.serverInfo.name should not be empty)

  it should "negotiate tools capability" in withClient: client =>
    monad.unit(client.serverCapabilities.tools.isDefined shouldBe true)

  it should "list tools" in withClient: client =>
    client.listTools().map(_.tools should not be empty)

  it should "call the echo tool" in withClient: client =>
    val arguments = Json.obj("message" -> Json.fromString("hello chimp"))
    client
      .callTool("echo", arguments)
      .map: result =>
        result.isError shouldBe false
        result.content should not be empty

  it should "call the get-tiny-image tool and receive image content" in withClient: client =>
    client
      .callTool("get-tiny-image", Json.obj())
      .map: result =>
        result.isError shouldBe false
        result.content.collect { case image: ToolContent.Image => image } should not be empty

  it should "call the get-structured-content tool and receive structured content" in withClient: client =>
    val arguments = Json.obj("location" -> Json.fromString("New York"))
    client
      .callTool("get-structured-content", arguments)
      .map: result =>
        result.isError shouldBe false
        result.structuredContent.isDefined shouldBe true

  it should "call the get-resource-links tool and receive resource links" in withClient: client =>
    val arguments = Json.obj("count" -> Json.fromInt(3))
    client
      .callTool("get-resource-links", arguments)
      .map: result =>
        result.isError shouldBe false
        result.content.collect { case link: ToolContent.ResourceLink => link } should not be empty

  it should "call the get-resource-reference tool" in withClient: client =>
    val arguments = Json.obj("resourceType" -> Json.fromString("Text"), "resourceId" -> Json.fromInt(1))
    client
      .callTool("get-resource-reference", arguments)
      .map: result =>
        result.isError shouldBe false
        result.content should not be empty

  it should "list prompts" in withClient: client =>
    client.listPrompts().map(_.prompts should not be empty)

  it should "list resources" in withClient: client =>
    client.listResources().map(_.resources should not be empty)

  it should "read the first listed resource" in withClient: client =>
    client.listResources().flatMap: resources =>
      val first = resources.resources.head
      client
        .readResource(first.uri)
        .map: result =>
          result.contents should not be empty

  it should "list resource templates" in withClient: client =>
    client.listResourceTemplates().map(_.resourceTemplates should not be empty)

  it should "ping" in withClient: client =>
    client.ping().map(_ => succeed)

  protected def withClient(test: McpClient[F] => F[Assertion]): Future[Assertion] =
    toFuture(
      usingBackend: backend =>
        usingTransport(backend, container.mcpUri): transport =>
          McpClient(transport, clientInfo, ProtocolVersion.Latest).flatMap: client =>
            test(client).flatMap(assertion => client.close().map(_ => assertion))
    )
