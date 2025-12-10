//> using dep com.softwaremill.chimp::core:0.1.6
//> using dep com.softwaremill.sttp.tapir::tapir-netty-server-sync:1.11.33
//> using dep ch.qos.logback:logback-classic:1.5.20

package chimp

import io.circe.Codec
import sttp.tapir.*
import sttp.tapir.server.netty.sync.NettySyncServer

case class IsPrimeInput(n: Int) derives Codec, Schema
case class IsFibonacciInput(n: Int) derives Codec, Schema

@main def twoToolsMcp(): Unit =
  val isPrimeTool = tool("isPrime")
    .description("Checks if a number is prime")
    .input[IsPrimeInput]
    .handle(i =>
      if i.n <= 0
      then Left("Only positive numbers can be prime-checked")
      else Right(isPrimeWithDescription(i.n))
    )

  val isFibonacci = tool("isFibonacci")
    .description("Checks if a number is a Fibonacci number")
    .input[IsFibonacciInput]
    .handle(i => Right(isFibonacciWithDescription(i.n)))

  val mcpServerEndpoint = mcpEndpoint(List(isPrimeTool, isFibonacci), List("mcp"))
  NettySyncServer().port(8080).addEndpoint(mcpServerEndpoint).startAndWait()

private def smallestDivisor(n: Int): Int =
  if n <= 1 then 1
  else if n % 2 == 0 then 2
  else if n % 3 == 0 then 3
  else
    var i = 5
    while i * i <= n do
      if n % i == 0 then return i
      if n % (i + 2) == 0 then return i + 2
      i += 6
    n

private def isPrimeWithDescription(n: Int) =
  val sd = smallestDivisor(n)
  if sd == n then s"$n is prime" else s"$n is not prime, it is divisible by $sd"

private def isFibonacciWithDescription(n: Int) =
  // check if n is a Fibonacci number by testing if 5*n^2 + 4 or 5*n^2 - 4 is a perfect square
  def isPerfectSquare(x: Long): Boolean =
    val sqrt = math.sqrt(x.toDouble).toLong
    sqrt * sqrt == x

  val nSquared = n.toLong * n.toLong
  val test1 = 5 * nSquared + 4
  val test2 = 5 * nSquared - 4

  if isPerfectSquare(test1) || isPerfectSquare(test2) then s"$n is a Fibonacci number"
  else s"$n is not a Fibonacci number"
