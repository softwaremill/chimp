package chimp.client.transport

import chimp.protocol.{JSONRPCMessage, RequestId}
import io.circe.parser
import io.circe.syntax.*
import org.slf4j.LoggerFactory
import sttp.monad.{IdentityMonad, MonadError}
import sttp.shared.Identity

import java.io.{BufferedReader, BufferedWriter, File, InputStreamReader, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import java.util.concurrent.{ConcurrentHashMap, SynchronousQueue, TimeUnit}

import scala.jdk.CollectionConverters.*

/** Synchronous stdio transport spawning the MCP server as a subprocess.
  *
  * For fiber-based effects (ZIO, cats-effect), use a `StreamingStdioTransport` impl from a per-effect chimp client subproject.
  */
final class StdioTransport(
    command: List[String],
    env: Map[String, String] = Map.empty,
    workDir: Option[File] = None
) extends Transport[Identity]:

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

  private val pending = ConcurrentHashMap[RequestId, SynchronousQueue[JSONRPCMessage]]()
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
          parser.decode[JSONRPCMessage](line) match
            case Right(msg) => dispatch(msg)
            case Left(e)    => log.warn(s"Failed to parse JSON-RPC line: ${e.getMessage}; raw: $line")
        line = reader.readLine()
    catch case e: Exception => if !closed.get() then log.warn(s"Reader loop ended: ${e.getMessage}")
    finally drainPending()

  private def drainStderr(): Unit =
    try
      var line: String = errReader.readLine()
      while line != null do
        log.info(s"stdio-server: $line")
        line = errReader.readLine()
    catch case _: Exception => ()

  private def dispatch(msg: JSONRPCMessage): Unit = msg match
    case r: JSONRPCMessage.Response =>
      val q = pending.remove(r.id)
      if q != null then q.offer(r, 1, TimeUnit.SECONDS)
    case e: JSONRPCMessage.Error =>
      val q = pending.remove(e.id)
      if q != null then q.offer(e, 1, TimeUnit.SECONDS)
    case other =>
      incomingHandler.get()(other)

  private def drainPending(): Unit =
    val it = pending.entrySet().iterator()
    while it.hasNext do
      val entry = it.next()
      val poison = JSONRPCMessage.Error(
        id = entry.getKey,
        error = chimp.protocol.JSONRPCErrorObject(code = -32000, message = "Transport closed")
      )
      entry.getValue.offer(poison, 100, TimeUnit.MILLISECONDS)
      it.remove()

  override def send(msg: JSONRPCMessage): Identity[Option[JSONRPCMessage]] =
    if closed.get() then throw chimp.client.McpTransportException("Stdio transport is closed")
    msg match
      case r: JSONRPCMessage.Request =>
        val q = SynchronousQueue[JSONRPCMessage]()
        pending.put(r.id, q)
        writeLine(r)
        Some(q.take())
      case other =>
        writeLine(other)
        None

  private def writeLine(msg: JSONRPCMessage): Unit =
    writer.synchronized:
      writer.write(msg.asJson.deepDropNullValues.noSpaces)
      writer.newLine()
      writer.flush()

  override def onIncoming(handler: JSONRPCMessage => Identity[Unit]): Identity[Unit] =
    incomingHandler.set(handler)

  override def close(): Identity[Unit] =
    if closed.compareAndSet(false, true) then
      try writer.close() catch case _: Exception => ()
      if proc.isAlive then
        if !proc.waitFor(2, TimeUnit.SECONDS) then proc.destroy()
        if !proc.waitFor(2, TimeUnit.SECONDS) then proc.destroyForcibly()
      readerThread.interrupt()
      stderrThread.interrupt()
