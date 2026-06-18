package chimp.client.transport

import chimp.client.internal.SyncPendingRequests
import chimp.protocol.JSONRPCMessage
import org.slf4j.LoggerFactory
import sttp.monad.{IdentityMonad, MonadError}
import sttp.shared.Identity

import java.io.*
import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*

/** A synchronous implementation of MCP Stdio transport. Launches a local MCP server as a subprocess and exchanges line-delimited JSON-RPC
  * messages over its standard input and output. Standard error from the subprocess is drained and forwarded to the logger.
  *
  * @param command
  *   The command and arguments used to start the subprocess.
  * @param env
  *   Additional environment variables to set for the subprocess.
  * @param workDir
  *   Optional working directory for the subprocess.
  * @param timeout
  *   Maximum time to wait for a response to each request before raising an [[chimp.client.McpTimeoutException]].
  */
final class ClientStdioTransport(
    command: List[String],
    env: Map[String, String] = Map.empty,
    workDir: Option[File] = None,
    timeout: FiniteDuration = ClientTransport.defaultTimeout
) extends ClientBidirectionalTransport[Identity]:

  private val log = LoggerFactory.getLogger(classOf[ClientStdioTransport])

  given monad: MonadError[Identity] = IdentityMonad

  private val proc: Process =
    val pb = ProcessBuilder(command.asJava)
    workDir.foreach(pb.directory)
    if env.nonEmpty then
      val procEnv = pb.environment()
      env.foreach { case (k, v) => procEnv.put(k, v) }
    pb.redirectErrorStream(false)
    pb.start()

  private val writer = BufferedWriter(OutputStreamWriter(proc.getOutputStream, StandardCharsets.UTF_8))
  private val reader = BufferedReader(InputStreamReader(proc.getInputStream, StandardCharsets.UTF_8))
  private val errReader = BufferedReader(InputStreamReader(proc.getErrorStream, StandardCharsets.UTF_8))

  private val pending = SyncPendingRequests()
  private val incomingHandler = AtomicReference[JSONRPCMessage => Identity[Unit]](_ => ())
  private val closed = AtomicBoolean(false)

  private val readerThread = startDaemon("mcp-stdio-reader", readLoop _)
  private val stderrThread = startDaemon("mcp-stdio-stderr", drainStderr _)

  private def startDaemon(name: String, body: () => Unit): Thread =
    val thread = Thread(() => body())
    thread.setName(name)
    thread.setDaemon(true)
    thread.start()
    thread

  private def readLoop(): Unit =
    try
      var line: String = reader.readLine()
      while line != null do
        if line.nonEmpty then
          ClientTransport.decode(line) match
            case Right(msg) => dispatch(msg)
            case Left(e)    => log.warn(s"Failed to parse JSON-RPC line: ${e.getMessage}; raw: $line")
        line = reader.readLine()
    catch case e: Exception => if !closed.get() then log.warn(s"Reader loop ended: ${e.getMessage}")
    finally pending.closeAll("Transport closed")

  private def drainStderr(): Unit =
    try
      var line: String = errReader.readLine()
      while line != null do
        log.info(s"stdio-server: $line")
        line = errReader.readLine()
    catch case _: Exception => ()

  private def dispatch(msg: JSONRPCMessage): Unit = msg match
    case response: JSONRPCMessage.Response =>
      val _ = pending.complete(response.id, response)
    case err: JSONRPCMessage.Error =>
      val _ = pending.complete(err.id, err)
    case other =>
      incomingHandler.get()(other)

  override def send(msg: JSONRPCMessage): Identity[Option[JSONRPCMessage]] =
    if closed.get() then throw chimp.client.McpTransportException("Stdio transport is closed")
    msg match
      case request: JSONRPCMessage.Request =>
        val await = pending.register(request.id, timeout)
        writeLine(request)
        Some(await())
      case other =>
        writeLine(other)
        None

  private def writeLine(msg: JSONRPCMessage): Unit =
    writer.synchronized:
      writer.write(ClientTransport.encode(msg))
      writer.newLine()
      writer.flush()

  override def onIncoming(handler: JSONRPCMessage => Identity[Unit]): Identity[Unit] =
    incomingHandler.set(handler)

  override def close(): Identity[Unit] =
    if closed.compareAndSet(false, true) then
      try writer.close()
      catch case _: Exception => ()
      if proc.isAlive then
        if !proc.waitFor(2, TimeUnit.SECONDS) then proc.destroy()
        if !proc.waitFor(2, TimeUnit.SECONDS) then { val _ = proc.destroyForcibly() }
      readerThread.interrupt()
      stderrThread.interrupt()
