//> using dep com.softwaremill.chimp::core:0.1.6
//> using dep com.softwaremill.sttp.tapir::tapir-zio-http-server:1.11.33
//> using dep ch.qos.logback:logback-classic:1.5.20

package chimp

import io.circe.Codec
import sttp.tapir.*
import sttp.tapir.server.ziohttp.ZioHttpInterpreter
import zio.RIO
import zio.ZIO
import zio.ZIOAppDefault
import zio.http.Server

case class ZioInput(a: Int, b: Int) derives Codec, Schema

object Main extends ZIOAppDefault:
  val adderTool = tool("adder")
    .description("Adds two numbers")
    .withAnnotations(ToolAnnotations(idempotentHint = Some(true)))
    .input[ZioInput]

  // note that here we need to explicitly state the effect type, as the Tapir-ZIO integration requires a `RIO[R, A]`
  // effect (with the error channel fixed to `Throwable`)
  val adderServerTool = adderTool.serverLogic[[X] =>> RIO[Any, X]]: (input, _) =>
    ZIO.succeed(Right(s"The result is ${input.a + input.b}"))

  val mcpServerEndpoint = mcpEndpoint(List(adderServerTool), List("mcp"))

  val routes = ZioHttpInterpreter().toHttp(mcpServerEndpoint)

  override def run =
    Server.serve(routes).provide(Server.default)
