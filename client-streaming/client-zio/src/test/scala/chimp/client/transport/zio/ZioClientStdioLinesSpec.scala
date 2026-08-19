package chimp.client.transport.zio

import chimp.transport.McpLineTooLongException
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import zio.Chunk
import zio.stream.ZStream

import java.nio.charset.StandardCharsets
import scala.concurrent.Future

class ZioClientStdioLinesSpec extends AsyncFlatSpec with Matchers with ZioToFuture:

  private def lines(text: String, maxLineLength: Int = 1024, chunkSize: Int = Int.MaxValue): Future[List[String]] =
    toFuture(ZioClientStdioTransport.lines(bytes(text, chunkSize), maxLineLength).runCollect).map(_.toList)

  private def failure(text: String, maxLineLength: Int, chunkSize: Int = Int.MaxValue): Future[Throwable] =
    toFuture(ZioClientStdioTransport.lines(bytes(text, chunkSize), maxLineLength).runCollect.either).map:
      case Left(thrown) => thrown
      case Right(lines) => fail(s"Expected a failure, got the lines: $lines")

  private def bytes(text: String, chunkSize: Int): ZStream[Any, Throwable, Byte] =
    val all = Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8))
    if chunkSize == Int.MaxValue then ZStream.fromChunk(all) else ZStream.fromChunk(all).rechunk(chunkSize)

  "the zio stdio framing" should "split the newline-delimited lines" in:
    lines("one\ntwo\nthree\n").map(_ shouldBe List("one", "two", "three"))

  it should "emit the last line when the input does not end with a newline" in:
    lines("one\ntwo").map(_ shouldBe List("one", "two"))

  it should "skip the empty lines" in:
    lines("one\n\ntwo\n").map(_ shouldBe List("one", "two"))

  it should "drop a carriage return before the newline" in:
    lines("one\r\ntwo\r\n").map(_ shouldBe List("one", "two"))

  it should "join the lines split across chunks" in:
    lines("""{"message":"zażółć gęślą jaźń"}""" + "\nnext\n", chunkSize = 1)
      .map(_ shouldBe List("""{"message":"zażółć gęślą jaźń"}""", "next"))

  it should "accept a line of exactly the maximum length" in:
    val line = "x" * 64
    lines(s"$line\n", maxLineLength = 64).map(_ shouldBe List(line))

  it should "fail on a line longer than the maximum length" in:
    failure("x" * 65 + "\n", maxLineLength = 64).map: thrown =>
      thrown shouldBe a[McpLineTooLongException]
      thrown.asInstanceOf[McpLineTooLongException].maxLineLength shouldBe 64

  it should "fail on a line longer than the maximum length which never ends" in:
    failure("x" * 65, maxLineLength = 64, chunkSize = 1).map(_ shouldBe a[McpLineTooLongException])

  it should "count the bytes of multi-byte characters" in:
    failure("ą" * 33 + "\n", maxLineLength = 64).map(_ shouldBe a[McpLineTooLongException])
