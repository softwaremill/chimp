package chimp

import sttp.tapir.Schema
import sttp.tapir.generic.auto._

case class Input(a: Int, b: Int)

@main def mcpApp(): Unit =
  val t = tool
    .description("Adds two numbers")
    .withAnnotations(ToolAnnotations(title = Some("Adder")))
    .input[Input]

  def logic(i: Input): Either[String, String] = Right(s"The result is ${i.a + i.b}")

  val st = t.handle(logic)
