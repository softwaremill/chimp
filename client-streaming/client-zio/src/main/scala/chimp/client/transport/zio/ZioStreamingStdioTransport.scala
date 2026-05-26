package chimp.client.transport.zio

import chimp.client.transport.{StreamingStdioTransport, Transport}
import chimp.protocol.JSONRPCMessage
import org.slf4j.LoggerFactory
import sttp.capabilities.zio.ZioStreams
import sttp.client4.impl.zio.RIOMonadAsyncError
import sttp.monad.MonadError
import zio.process.{Command, Process, ProcessInput}
import zio.stream.ZStream
import zio.{Chunk, Exit, Queue, Ref, Scope, Task, ZIO, ZLayer}

import java.io.File
import java.nio.charset.StandardCharsets
import scala.concurrent.duration.FiniteDuration

final class ZioStreamingStdioTransport private (
    command: List[String],
    env: Map[String, String],
    workDir: Option[File],
    timeout: FiniteDuration,
    scope: Scope.Closeable,
    process: Process,
    writeQueue: Queue[JSONRPCMessage],
    pending: ZioPendingRequests,
    incomingRef: Ref[JSONRPCMessage => Task[Unit]]
) extends StreamingStdioTransport[Task, ZioStreams](command, env, workDir):

  private val log = LoggerFactory.getLogger(classOf[ZioStreamingStdioTransport])

  override given monad: MonadError[Task] = new RIOMonadAsyncError[Any]

  override def send(msg: JSONRPCMessage): Task[Option[JSONRPCMessage]] =
    msg match
      case request: JSONRPCMessage.Request =>
        pending
          .register(request.id, timeout)
          .flatMap: await =>
            writeQueue.offer(msg) *> await().map(Some(_))
      case _ =>
        writeQueue.offer(msg).as(None)

  override def onIncoming(handler: JSONRPCMessage => Task[Unit]): Task[Unit] =
    incomingRef.set(handler)

  override def close(): Task[Unit] =
    writeQueue.shutdown *> pending.closeAll("Transport closed").ignore *> process.kill.ignore *> scope.close(Exit.unit).ignore

  private def dispatch(msg: JSONRPCMessage): Task[Unit] = msg match
    case response: JSONRPCMessage.Response => pending.complete(response.id, response).unit
    case err: JSONRPCMessage.Error         => pending.complete(err.id, err).unit
    case other                             => incomingRef.get.flatMap(_(other))

  private[zio] def startReader: Task[Unit] =
    val drain = process.stdout.linesStream
      .filter(_.nonEmpty)
      .mapZIO: line =>
        Transport.decode(line) match
          case Right(msg) => dispatch(msg)
          case Left(err)  => ZIO.succeed(log.warn(s"Failed to parse JSON-RPC line: ${err.getMessage}, raw: $line"))
      .runDrain
      .ensuring(pending.closeAll("Transport closed").orDie)
    drain.catchAll(t => ZIO.succeed(log.warn(s"Reader fiber ended: ${t.getMessage}"))).forkIn(scope).unit

  private[zio] def startStderr: Task[Unit] =
    process.stderr.linesStream
      .runForeach(line => ZIO.succeed(log.info(s"stdio-server: $line")))
      .catchAll(_ => ZIO.unit)
      .forkIn(scope)
      .unit

object ZioStreamingStdioTransport:
  import scala.concurrent.duration.DurationInt
  private val defaultTimeout: FiniteDuration = 60.seconds

  def apply(
      command: List[String],
      env: Map[String, String] = Map.empty,
      workDir: Option[File] = None,
      timeout: FiniteDuration = defaultTimeout
  ): Task[ZioStreamingStdioTransport] =
    for
      scope <- Scope.make
      writeQueue <- Queue.bounded[JSONRPCMessage](256)
      pending <- ZioPendingRequests.make
      incomingRef <- Ref.make[JSONRPCMessage => Task[Unit]](_ => ZIO.unit)
      stdinBytes = ZStream
        .fromQueue(writeQueue)
        .map(msg => Chunk.fromArray((Transport.encode(msg) + "\n").getBytes(StandardCharsets.UTF_8)))
        .flattenChunks
      baseCmd = Command(command.head, command.tail*)
      withEnv = if env.isEmpty then baseCmd else baseCmd.env(env)
      withDir = workDir.fold(withEnv)(withEnv.workingDirectory)
      cmd = withDir.stdin(ProcessInput.fromStream(stdinBytes, flushChunksEagerly = true))
      process <- cmd.run.provideEnvironment(zio.ZEnvironment(scope))
      transport = new ZioStreamingStdioTransport(command, env, workDir, timeout, scope, process, writeQueue, pending, incomingRef)
      _ <- transport.startReader
      _ <- transport.startStderr
    yield transport

  def scoped(
      command: List[String],
      env: Map[String, String] = Map.empty,
      workDir: Option[File] = None,
      timeout: FiniteDuration = defaultTimeout
  ): ZIO[Scope, Throwable, ZioStreamingStdioTransport] =
    ZIO.acquireRelease(apply(command, env, workDir, timeout))(_.close().ignore)

  def layer(
      command: List[String],
      env: Map[String, String] = Map.empty,
      workDir: Option[File] = None,
      timeout: FiniteDuration = defaultTimeout
  ): ZLayer[Any, Throwable, ZioStreamingStdioTransport] =
    ZLayer.scoped(scoped(command, env, workDir, timeout))
