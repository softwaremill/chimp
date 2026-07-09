package chimp.client.transport.ox

import chimp.client.McpTransportException
import chimp.client.internal.SyncPendingRequests
import chimp.client.transport.{ClientStreamingStdioTransport, ClientTransport}
import chimp.protocol.JSONRPCMessage
import org.slf4j.LoggerFactory
import ox.*
import ox.channels.{Actor, ActorRef}
import sttp.monad.{IdentityMonad, MonadError}
import sttp.shared.Identity

import java.io.{BufferedReader, BufferedWriter, File, InputStreamReader, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*

final class OxClientStdioTransport private (
    command: List[String],
    env: Map[String, String],
    workDir: Option[File],
    timeout: FiniteDuration,
    process: Process,
    pending: SyncPendingRequests
)(using Ox)
    extends ClientStreamingStdioTransport[Identity](command, env, workDir, timeout):

  private val log = LoggerFactory.getLogger(classOf[OxClientStdioTransport])

  private final class State:
    var incoming: JSONRPCMessage => Unit = _ => ()
    var closed: Boolean = false

    def beginClosing(): Boolean =
      if closed then false
      else
        closed = true
        true

  private val state: ActorRef[State] = Actor.create(new State)

  given monad: MonadError[Identity] = IdentityMonad

  private val writer = BufferedWriter(OutputStreamWriter(process.getOutputStream, StandardCharsets.UTF_8))

  override def send(msg: JSONRPCMessage): Identity[Option[JSONRPCMessage]] =
    if state.ask(_.closed) then throw McpTransportException("Stdio transport is closed")
    msg match
      case request: JSONRPCMessage.Request =>
        val await = pending.register(request.id, timeout)
        writeLine(request)
        Some(await())
      case other =>
        writeLine(other)
        None

  override def onIncoming(handler: JSONRPCMessage => Identity[Unit]): Identity[Unit] = state.tell(_.incoming = handler)

  override def close(): Identity[Unit] =
    if state.ask(_.beginClosing()) then
      try writer.close()
      catch case _: Exception => ()
      if process.isAlive then
        if !process.waitFor(2, TimeUnit.SECONDS) then process.destroy()
        if !process.waitFor(2, TimeUnit.SECONDS) then { val _ = process.destroyForcibly() }
      pending.closeAll("Transport closed")

  private def writeLine(msg: JSONRPCMessage): Unit =
    writer.synchronized:
      writer.write(ClientTransport.encode(msg))
      writer.newLine()
      writer.flush()

  private def dispatch(msg: JSONRPCMessage): Unit = msg match
    case response: JSONRPCMessage.Response => val _ = pending.complete(response.id, response)
    case err: JSONRPCMessage.Error         => val _ = pending.complete(err.id, err)
    case other                             => state.ask(_.incoming)(other)

  private def readLoop(reader: BufferedReader): Unit =
    try
      var line = reader.readLine()
      while line != null do
        if line.nonEmpty then
          ClientTransport.decode(line) match
            case Right(msg) => dispatch(msg)
            case Left(e)    => log.warn(s"Failed to parse JSON-RPC line: ${e.getMessage}; raw: $line")
        line = reader.readLine()
    catch case e: Exception => if !state.ask(_.closed) then log.warn(s"Reader loop ended: ${e.getMessage}")
    finally pending.closeAll("Transport closed")

  private def drainStderr(errReader: BufferedReader): Unit =
    try
      var line = errReader.readLine()
      while line != null do
        log.info(s"stdio-server: $line")
        line = errReader.readLine()
    catch case _: Exception => ()

object OxClientStdioTransport:

  def apply(
      command: List[String],
      env: Map[String, String] = Map.empty,
      workDir: Option[File] = None,
      timeout: FiniteDuration = ClientTransport.defaultTimeout
  )(using Ox): OxClientStdioTransport =
    val pb = ProcessBuilder(command.asJava)
    workDir.foreach(pb.directory)
    if env.nonEmpty then
      val procEnv = pb.environment()
      env.foreach { (k, v) =>
        val _ = procEnv.put(k, v)
      }
    pb.redirectErrorStream(false)
    val process = pb.start()

    val transport = new OxClientStdioTransport(command, env, workDir, timeout, process, SyncPendingRequests())

    val reader = BufferedReader(InputStreamReader(process.getInputStream, StandardCharsets.UTF_8))
    val errReader = BufferedReader(InputStreamReader(process.getErrorStream, StandardCharsets.UTF_8))
    forkDiscard(transport.readLoop(reader))
    forkDiscard(transport.drainStderr(errReader))
    transport
