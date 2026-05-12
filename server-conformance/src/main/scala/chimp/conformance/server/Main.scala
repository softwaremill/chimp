package chimp.conformance.server

import chimp.server.*
import io.circe.Codec
import ox.supervised
import sttp.tapir.Schema
import sttp.tapir.server.netty.sync.NettySyncServer

object Main:

  case class AddNumbersInput(a: Double, b: Double) derives Codec, Schema
  case class NoInput() derives Codec, Schema

  private val addNumbers = tool("add_numbers")
    .description("Adds two numbers and returns the result as text.")
    .input[AddNumbersInput]
    .handle(in => Right((in.a + in.b).toString))

  private val simpleText = tool("test_simple_text")
    .description("Returns a fixed text string.")
    .input[NoInput]
    .handle(_ => Right("This is a simple text response for testing."))

  private val errorTool = tool("test_error_handling")
    .description("Always returns an error result.")
    .input[NoInput]
    .handle(_ => Left("This tool intentionally returns an error for testing"))

  private val tools = List(addNumbers, simpleText, errorTool)

  def main(args: Array[String]): Unit =
    val requestedPort = args
      .collectFirst { case s"--port=$p" => p.toInt }
      .orElse(sys.env.get("CHIMP_CONFORMANCE_PORT").map(_.toInt))
      .getOrElse(0)

    val endpoint = mcpEndpoint(tools, List("mcp"), name = "chimp-conformance-server", version = "0.1.0")

    supervised:
      val binding = NettySyncServer().port(requestedPort).addEndpoint(endpoint).start()
      println(s"http://127.0.0.1:${binding.port}/mcp")
      System.out.flush()
      Thread.currentThread.join()
