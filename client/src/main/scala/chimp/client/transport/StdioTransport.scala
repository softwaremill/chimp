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
import scala.jdk.CollectionConverters.*

final class StdioTransport(
    command: List[String],
    env: Map[String, String] = Map.empty,
    workDir: Option[File] = None
) extends BidirectionalTransport[Identity]:

  private val log = LoggerFactory.getLogger(classOf[StdioTransport])

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
    val t = Thread(() => body())
    t.setName(name)
    t.setDaemon(true)
    t.start()
    t

  private def readLoop(): Unit =
    try
      var line: String = reader.readLine()
      while line != null do
        if line.nonEmpty then
          Transport.decode(line) match
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
    case r: JSONRPCMessage.Response =>
      val _ = pending.complete(r.id, r)
    case e: JSONRPCMessage.Error =>
      val _ = pending.complete(e.id, e)
    case other =>
      incomingHandler.get()(other)

  override def send(msg: JSONRPCMessage): Identity[Option[JSONRPCMessage]] =
    if closed.get() then throw chimp.client.McpTransportException("Stdio transport is closed")
    msg match
      case r: JSONRPCMessage.Request =>
        val await = pending.register(r.id)
        writeLine(r)
        Some(await())
      case other =>
        writeLine(other)
        None

  private def writeLine(msg: JSONRPCMessage): Unit =
    writer.synchronized:
      writer.write(Transport.encode(msg))
      writer.newLine()
      writer.flush()

  override def onIncoming(handler: JSONRPCMessage => Identity[Unit]): Identity[Unit] =
    incomingHandler.set(handler)

  override def close(): Identity[Unit] =
    if closed.compareAndSet(false, true) then
      drainPending()
      try writer.close()
      catch case _: Exception => ()
      try proc.getInputStream.close()
      catch case _: Exception => ()
      if proc.isAlive then
        if !proc.waitFor(2, TimeUnit.SECONDS) then proc.destroy()
        if !proc.waitFor(2, TimeUnit.SECONDS) then { val _ = proc.destroyForcibly() }
      readerThread.interrupt()
      stderrThread.interrupt()
