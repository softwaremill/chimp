package chimp.transport

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

class StdioLineReaderSpec extends AnyFlatSpec with Matchers:

  private def reader(input: String, maxLineLength: Int = 1024): StdioLineReader =
    StdioLineReader(ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8)), maxLineLength)

  "a stdio line reader" should "read newline-delimited lines" in:
    reader("one\ntwo\nthree\n").lines.toList shouldBe List("one", "two", "three")

  it should "read the last line when the input does not end with a newline" in:
    reader("one\ntwo").lines.toList shouldBe List("one", "two")

  it should "keep the empty lines" in:
    reader("one\n\ntwo\n").lines.toList shouldBe List("one", "", "two")

  it should "drop a carriage return before the newline" in:
    reader("one\r\ntwo\r\n").lines.toList shouldBe List("one", "two")

  it should "decode multi-byte characters" in:
    reader("""{"message":"zażółć gęślą jaźń"}""" + "\n").lines.toList shouldBe List("""{"message":"zażółć gęślą jaźń"}""")

  it should "report the end of the input" in:
    val r = reader("one\n")
    r.readLine() shouldBe Some("one")
    r.readLine() shouldBe None

  it should "report the end of an empty input" in:
    reader("").readLine() shouldBe None

  it should "accept a line of exactly the maximum length" in:
    val line = "x" * 64
    reader(s"$line\n", maxLineLength = 64).lines.toList shouldBe List(line)

  it should "fail on a line longer than the maximum length" in:
    val failure = intercept[McpLineTooLongException](reader("x" * 65 + "\n", maxLineLength = 64).lines.toList)
    failure.maxLineLength shouldBe 64
    failure.getMessage should include("64")

  it should "fail on a line longer than the maximum length which never ends" in:
    intercept[McpLineTooLongException](reader("x" * 65, maxLineLength = 64).lines.toList)

  it should "count the bytes of multi-byte characters" in:
    intercept[McpLineTooLongException](reader("ą" * 33 + "\n", maxLineLength = 64).lines.toList)
