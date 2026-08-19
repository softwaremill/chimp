package chimp.transport

import java.io.{BufferedInputStream, ByteArrayOutputStream, InputStream}
import java.nio.charset.StandardCharsets

final class McpLineTooLongException(val maxLineLength: Int)
    extends RuntimeException(s"An incoming line is longer than the maximum of $maxLineLength bytes")

object StdioFraming:

  val defaultMaxLineLength: Int = 10 * 1024 * 1024

  private[chimp] val newline: Byte = '\n'.toByte

  private[chimp] def decodeLine(bytes: Array[Byte]): String =
    val end = if bytes.length > 0 && bytes(bytes.length - 1) == '\r'.toByte then bytes.length - 1 else bytes.length
    String(bytes, 0, end, StandardCharsets.UTF_8)

private[chimp] final class StdioLineReader(in: InputStream, maxLineLength: Int):
  private val buffered = BufferedInputStream(in)
  private val line = ByteArrayOutputStream()

  def readLine(): Option[String] =
    var next = buffered.read()
    if next < 0 then None
    else
      line.reset()
      while next >= 0 && next != StdioFraming.newline do
        if line.size() >= maxLineLength then throw McpLineTooLongException(maxLineLength)
        line.write(next)
        next = buffered.read()
      Some(StdioFraming.decodeLine(line.toByteArray))

  def lines: Iterator[String] = Iterator.continually(readLine()).takeWhile(_.isDefined).flatten
