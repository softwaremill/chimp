package chimp.client

import chimp.client.transport.ClientTransport
import chimp.protocol.*
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.monad.{IdentityMonad, MonadError}
import sttp.shared.Identity

import scala.collection.mutable
import scala.concurrent.duration.Duration

class McpClientEraDetectionSpec extends AnyFlatSpec with Matchers:

  private val clientInfo = Implementation(name = "chimp-test", version = "0.0.1")
  private val tools = ServerCapabilities(tools = Some(ServerToolsCapability(listChanged = Some(false))))

  /** A transport whose server answers each request with `answer`; a `Left` fails the send, as a transport error does. */
  private final class ScriptedServer(answer: JSONRPCMessage.Request => Either[Throwable, JSONRPCMessage]) extends ClientTransport[Identity]:
    given monad: MonadError[Identity] = IdentityMonad
    val methods: mutable.ListBuffer[String] = mutable.ListBuffer.empty
    val versions: mutable.ListBuffer[ProtocolVersion] = mutable.ListBuffer.empty

    override def send(msg: JSONRPCMessage): Identity[Option[JSONRPCMessage]] = msg match
      case request: JSONRPCMessage.Request =>
        methods += request.method
        answer(request).fold(throw _, Some(_))
      case JSONRPCMessage.Notification(_, method, _) =>
        methods += method
        None
      case _ => None

    override def close(): Identity[Unit] = ()

    override def setProtocolVersion(version: ProtocolVersion): Identity[Unit] = versions += version

  private def result(request: JSONRPCMessage.Request, json: Json): Either[Throwable, JSONRPCMessage] =
    Right(JSONRPCMessage.Response(id = request.id, result = json))

  private def error(request: JSONRPCMessage.Request, error: JSONRPCErrorObject): Either[Throwable, JSONRPCMessage] =
    Right(JSONRPCMessage.Error(id = request.id, error = error))

  private def methodNotFound(request: JSONRPCMessage.Request) =
    error(request, JSONRPCErrorObject(JSONRPCErrorCodes.MethodNotFound.code, s"Method not found: ${request.method}"))

  private def discoverResult(supported: List[ProtocolVersion]): Json =
    DiscoverResult(
      supportedVersions = supported.map(_.name),
      capabilities = tools,
      ttlMs = Duration.Zero,
      cacheScope = CacheScope.Private,
      _meta = Some(Map(ProtocolMeta.ServerInfo -> Implementation("modern", "1").asJson))
    ).asJson.deepDropNullValues

  private def initializeResult(version: ProtocolVersion): Json =
    InitializeResult(protocolVersion = version.name, capabilities = tools, serverInfo = Implementation("legacy", "1")).asJson

  private def stateless(request: JSONRPCMessage.Request): Boolean =
    request.params.flatMap(_.hcursor.downField("_meta").get[String](ProtocolMeta.ProtocolVersionKey).toOption).isDefined

  private val callResult = CallToolResult(content = List(ToolContent.Text(text = "ok"))).asJson

  /** Serves only 2026-07-28: it has no `initialize`, and it rejects a request without `_meta`. */
  private def modernServer = ScriptedServer: request =>
    request.method match
      case "server/discover"       => result(request, discoverResult(List(ProtocolVersion.Latest)))
      case "initialize"            => methodNotFound(request)
      case _ if stateless(request) => result(request, callResult)
      case _                       => error(request, JSONRPCErrorObject(JSONRPCErrorCodes.InvalidRequest.code, "no _meta"))

  /** Serves only 2025-11-25: `server/discover` is an unknown method. */
  private def legacyServer = ScriptedServer: request =>
    request.method match
      case "initialize" => result(request, initializeResult(ProtocolVersion.LatestLegacy))
      case "tools/call" => result(request, callResult)
      case _            => methodNotFound(request)

  /** Serves both eras. */
  private def dualServer = ScriptedServer: request =>
    request.method match
      case "server/discover" => result(request, discoverResult(List(ProtocolVersion.Latest, ProtocolVersion.LatestLegacy)))
      case "initialize"      => result(request, initializeResult(ProtocolVersion.LatestLegacy))
      case _                 => result(request, callResult)

  private val modernClient: VersionPolicy = ProtocolVersion.Latest
  private val legacyClient: VersionPolicy = ProtocolVersion.LatestLegacy
  private val dualClient: VersionPolicy = DetectVersion

  private def connect(server: ScriptedServer, policy: VersionPolicy): McpClient[Identity] =
    McpClient[Identity](server, clientInfo, policy)

  "The compatibility matrix" should "connect a modern client to a modern server on the stateless wire" in:
    val server = modernServer
    connect(server, modernClient).protocolVersion shouldBe ProtocolVersion.Latest
    server.methods.toList shouldBe List("server/discover")

  it should "fail a modern client against a legacy server" in:
    val ex = intercept[McpProtocolException](connect(legacyServer, modernClient))
    ex.getMessage should include("server/discover failed")

  it should "fail a legacy client against a modern server as today" in:
    val ex = intercept[McpProtocolException](connect(modernServer, legacyClient))
    ex.getMessage should include("Initialize failed")

  it should "connect a legacy client to a legacy server with initialize" in:
    val server = legacyServer
    connect(server, legacyClient).protocolVersion shouldBe ProtocolVersion.LatestLegacy
    server.methods.toList shouldBe List("initialize", "notifications/initialized")

  it should "connect a dual-era client to a modern server on the stateless wire" in:
    val server = modernServer
    val client = connect(server, dualClient)
    client.protocolVersion shouldBe ProtocolVersion.Latest
    client.callTool("echo", Json.obj()).isError shouldBe false
    server.methods.toList shouldBe List("server/discover", "tools/call")

  it should "connect a dual-era client to a legacy server with initialize at the latest legacy version, and decide only once" in:
    val server = legacyServer
    val client = connect(server, dualClient)
    client.protocolVersion shouldBe ProtocolVersion.LatestLegacy
    client.callTool("echo", Json.obj()).isError shouldBe false
    client.callTool("echo", Json.obj()).isError shouldBe false
    server.methods.toList shouldBe List("server/discover", "initialize", "notifications/initialized", "tools/call", "tools/call")
    server.versions.last shouldBe ProtocolVersion.LatestLegacy

  it should "connect a dual-era client to a dual-era server on the stateless wire" in:
    val server = dualServer
    connect(server, dualClient).protocolVersion shouldBe ProtocolVersion.Latest
    server.methods.toList shouldBe List("server/discover")

  it should "connect fixed-version clients to a dual-era server at their version" in:
    connect(dualServer, modernClient).protocolVersion shouldBe ProtocolVersion.Latest
    connect(dualServer, legacyClient).protocolVersion shouldBe ProtocolVersion.LatestLegacy

  "Era detection" should "decide legacy on a failed send, such as an HTTP 400, 404 or 405 without a modern error, or a timeout" in:
    val failures = List(McpTransportException("Unexpected HTTP response: 405"), McpTimeoutException(RequestId(1)))
    failures.foreach: failure =>
      val server = ScriptedServer: request =>
        request.method match
          case "server/discover" => Left(failure)
          case _                 => result(request, initializeResult(ProtocolVersion.LatestLegacy))
      connect(server, dualClient).protocolVersion shouldBe ProtocolVersion.LatestLegacy
      server.methods.toList shouldBe List("server/discover", "initialize", "notifications/initialized")

  it should "decide legacy on any error that is not a modern one" in:
    val server = ScriptedServer: request =>
      request.method match
        case "server/discover" => error(request, JSONRPCErrorObject(-32000, "Bad Request: Server not initialized"))
        case _                 => result(request, initializeResult(ProtocolVersion.LatestLegacy))
    connect(server, dualClient).protocolVersion shouldBe ProtocolVersion.LatestLegacy

  it should "decide legacy on a result that is not a discover result" in:
    val server = ScriptedServer: request =>
      request.method match
        case "server/discover" => result(request, Json.obj("ok" -> Json.True))
        case _                 => result(request, initializeResult(ProtocolVersion.LatestLegacy))
    connect(server, dualClient).protocolVersion shouldBe ProtocolVersion.LatestLegacy

  it should "decide modern on -32022 and retry discover with a common stateless version" in:
    val discovers = java.util.concurrent.atomic.AtomicInteger()
    val server = ScriptedServer: request =>
      request.method match
        case "server/discover" if discovers.incrementAndGet() == 1 =>
          error(request, ProtocolMeta.unsupportedVersionError("2026-07-28", List("2026-07-28")))
        case "server/discover" => result(request, discoverResult(List(ProtocolVersion.Latest)))
        case _                 => methodNotFound(request)
    connect(server, dualClient).protocolVersion shouldBe ProtocolVersion.Latest
    server.methods.toList shouldBe List("server/discover", "server/discover")

  it should "fall back to initialize when -32022 offers only a common legacy version" in:
    val server = ScriptedServer: request =>
      request.method match
        case "server/discover" => error(request, ProtocolMeta.unsupportedVersionError("2026-07-28", List("2099-01-01", "2025-06-18")))
        case _                 => result(request, initializeResult(ProtocolVersion.V2025_06_18))
    connect(server, dualClient).protocolVersion shouldBe ProtocolVersion.V2025_06_18
    server.methods.toList shouldBe List("server/discover", "initialize", "notifications/initialized")

  it should "fail with both version lists when a modern server shares no version" in:
    val server = ScriptedServer: request =>
      error(request, ProtocolMeta.unsupportedVersionError("2026-07-28", List("2099-01-01")))
    val ex = intercept[McpUnsupportedProtocolVersionException](connect(server, dualClient))
    ex.serverVersions shouldBe List("2099-01-01")
    ex.clientVersions shouldBe ProtocolVersion.supported.map(_.name)
    server.methods.toList shouldBe List("server/discover")

  it should "not fall back on another modern error" in:
    val server = ScriptedServer: request =>
      error(request, ProtocolMeta.missingClientCapabilityError(ClientCapabilities(sampling = Some(Json.obj()))))
    val ex = intercept[McpProtocolException](connect(server, dualClient))
    ex.getMessage should include("-32021")
    server.methods.toList shouldBe List("server/discover")

  it should "fail as today when a legacy server rejects initialize" in:
    val server = ScriptedServer: request =>
      request.method match
        case "initialize" => error(request, JSONRPCErrorObject(JSONRPCErrorCodes.InvalidParams.code, "bad version"))
        case _            => methodNotFound(request)
    val ex = intercept[McpProtocolException](connect(server, dualClient))
    ex.getMessage should include("Initialize failed")
