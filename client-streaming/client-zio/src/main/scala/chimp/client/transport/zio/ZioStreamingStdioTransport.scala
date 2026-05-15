package chimp.client.transport.zio

import chimp.client.transport.{StreamingStdioTransport, Transport}
import chimp.protocol.JSONRPCMessage
import org.slf4j.LoggerFactory
import sttp.capabilities.zio.ZioStreams
import sttp.client4.impl.zio.RIOMonadAsyncError
import sttp.monad.MonadError
import zio.process.{Command, Process, ProcessInput}
import zio.stream.ZStream
import zio.{Queue, Ref, Scope, Task, ZIO}

import java.io.File
import java.nio.charset.StandardCharsets

final class ZioStreamingStdioTransport private (
    command: List[String],
    env: Map[String, String],
    workDir: Option[File],
    scope: Scope,
    process: Process,
    writeQueue: Queue[JSONRPCMessage],
    pending: ZioPendingRequests,
    incomingRef: Ref[JSONRPCMessage => Task[Unit]]
) extends StreamingStdioTransport[Task, ZioStreams](command, env, workDir, ZioStreams):

  override given monad: MonadError[Task] = new RIOMonadAsyncError[Any]

  override def send(msg: JSONRPCMessage): Task[Option[JSONRPCMessage]] =
    msg match
      case r: JSONRPCMessage.Request =>
        pending
          .register(r.id)
          .flatMap: await =>
            writeQueue.offer(msg) *> await().map(Some(_))
      case _ =>
        writeQueue.offer(msg).as(None)

  override def onIncoming(handler: JSONRPCMessage => Task[Unit]): Task[Unit] =
    incomingRef.set(handler)

  override def close(): Task[Unit] =
    writeQueue.shutdown *> process.kill.ignore

  private def dispatch(msg: JSONRPCMessage): Task[Unit] = msg match
    case r: JSONRPCMessage.Response => pending.complete(r.id, r).unit
    case e: JSONRPCMessage.Error    => pending.complete(e.id, e).unit
    case other                      => incomingRef.get.flatMap(_(other))

  private[zio] def startReader: Task[Unit] =
    val drain = process.stdout.linesStream
      .filter(_.nonEmpty)
      .mapZIO: line =>
        Transport.decode(line) match
          case Right(msg) => dispatch(msg)
          case Left(err)  => ZIO.logWarning(s"Failed to parse JSON-RPC line: ${err.getMessage}; raw: $line")
      .runDrain
      .ensuring(pending.closeAll("Transport closed").orDie)
    drain.catchAll(t => ZIO.logWarning(s"Reader fiber ended: ${t.getMessage}")).forkIn(scope).unit

  private[zio] def startStderr: Task[Unit] =
    process.stderr.linesStream
      .runForeach(line => ZIO.succeed(ZioStreamingStdioTransport.log.info(s"stdio-server: $line")))
      .catchAll(_ => ZIO.unit)
      .forkIn(scope)
      .unit

object ZioStreamingStdioTransport:
  private val log = LoggerFactory.getLogger(classOf[ZioStreamingStdioTransport])

  def make(
      command: List[String],
      env: Map[String, String] = Map.empty,
      workDir: Option[File] = None
  ): ZIO[Scope, Throwable, ZioStreamingStdioTransport] =
    for
      scope <- ZIO.scope
      writeQueue <- Queue.unbounded[JSONRPCMessage]
      pending <- ZioPendingRequests.make
      incomingRef <- Ref.make[JSONRPCMessage => Task[Unit]](_ => ZIO.unit)
      stdinBytes = ZStream
        .fromQueue(writeQueue)
        .map(msg => Transport.encode(msg) + "\n")
        .flatMap(s => ZStream.fromIterable(s.getBytes(StandardCharsets.UTF_8)))
      baseCmd = Command(command.head, command.tail*)
      withEnv = if env.isEmpty then baseCmd else baseCmd.env(env)
      withDir = workDir.fold(withEnv)(withEnv.workingDirectory)
      cmd = withDir.stdin(ProcessInput.fromStream(stdinBytes))
      process <- cmd.run
      transport = new ZioStreamingStdioTransport(command, env, workDir, scope, process, writeQueue, pending, incomingRef)
      _ <- transport.startReader
      _ <- transport.startStderr
    yield transport
