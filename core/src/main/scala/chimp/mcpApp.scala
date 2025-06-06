package chimp

import sttp.tapir.Schema
import sttp.tapir.json.circe.*
import io.circe.Codec

case class Input(a: Int, b: Int) derives Codec, Schema

@main def mcpApp(): Unit =
  val t = tool("adder")
    .description("Adds two numbers")
    .withAnnotations(ToolAnnotations(idempotentHint = Some(true)))
    .input[Input]

  def logic(i: Input): Either[String, String] = Right(s"The result is ${i.a + i.b}")

  val st = t.handle(logic)
