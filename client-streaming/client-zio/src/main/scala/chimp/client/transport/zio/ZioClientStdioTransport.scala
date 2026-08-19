package chimp.client.transport.zio

import chimp.client.transport.{ClientStreamingStdioTransport, ClientTransport}
import chimp.protocol.JSONRPCMessage
import chimp.transport.{McpLineTooLongException, StdioFraming}
import org.slf4j.LoggerFactory
import sttp.client4.impl.zio.RIOMonadAsyncError
import sttp.monad.MonadError
import zio.process.{Command, Process, ProcessInput}
import zio.stream.ZStream
import zio.{Chunk, Exit, Queue, Ref, Scope, Task, ZIO, ZLayer}

import java.io.File
import java.nio.charset.StandardCharsets
import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration

final class ZioClientStdioTransport private (
    command: List[String],
    env: Map[String, String],
    workDir: Option[File],
    timeout: FiniteDuration,
    scope: Scope.Closeable,
    process: Process,
    maxLineLength: Int,
    writeQueue: Queue[JSONRPCMessage],
    pending: ZioPendingRequests,
    incomingRef: Ref[JSONRPCMessage => Task[Unit]],
    closingRef: Ref[Boolean]
) extends ClientStreamingStdioTransport[Task](command, env, workDir):

  private val log = LoggerFactory.getLogger(classOf[ZioClientStdioTransport])

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
    closingRef.set(true) *> writeQueue.shutdown *> pending.closeAll("Transport closed").ignore *> process.kill.ignore *>
      scope.close(Exit.unit).ignore

  private def dispatch(msg: JSONRPCMessage): Task[Unit] = msg match
    case response: JSONRPCMessage.Response => pending.complete(response.id, response).unit
    case err: JSONRPCMessage.Error         => pending.complete(err.id, err).unit
    case other                             => incomingRef.get.flatMap(_(other))

  private[zio] def startReader: Task[Unit] =
    val drain = ZioClientStdioTransport
      .lines(process.stdout.stream, maxLineLength)
      .mapZIO: line =>
        ClientTransport.decode(line) match
          case Right(msg) => dispatch(msg)
          case Left(err)  => ZIO.succeed(log.warn(s"Failed to parse JSON-RPC line: ${err.getMessage}, raw: $line"))
      .runDrain
      .ensuring(pending.closeAll("Transport closed").orDie)
    drain.catchAll(t => warnUnlessClosing(s"Reader fiber ended: ${t.getMessage}")).forkIn(scope).unit

  private def warnUnlessClosing(message: => String): Task[Unit] =
    closingRef.get.map(closing => if !closing then log.warn(message))

  private[zio] def startStderr: Task[Unit] =
    process.stderr.linesStream
      .runForeach(line => ZIO.succeed(log.info(s"stdio-server: $line")))
      .catchAll(_ => ZIO.unit)
      .forkIn(scope)
      .unit

object ZioClientStdioTransport:

  def apply(
      command: List[String],
      env: Map[String, String] = Map.empty,
      workDir: Option[File] = None,
      timeout: FiniteDuration = ClientTransport.defaultTimeout,
      maxLineLength: Int = StdioFraming.defaultMaxLineLength
  ): Task[ZioClientStdioTransport] =
    for
      scope <- Scope.make
      writeQueue <- Queue.bounded[JSONRPCMessage](256)
      pending <- ZioPendingRequests.make
      incomingRef <- Ref.make[JSONRPCMessage => Task[Unit]](_ => ZIO.unit)
      closingRef <- Ref.make(false)
      stdinBytes = ZStream
        .fromQueue(writeQueue)
        .map(msg => Chunk.fromArray((ClientTransport.encode(msg) + "\n").getBytes(StandardCharsets.UTF_8)))
        .flattenChunks
      baseCmd = Command(command.head, command.tail*)
      withEnv = if env.isEmpty then baseCmd else baseCmd.env(env)
      withDir = workDir.fold(withEnv)(withEnv.workingDirectory)
      cmd = withDir.stdin(ProcessInput.fromStream(stdinBytes, flushChunksEagerly = true))
      process <- cmd.run.provideEnvironment(zio.ZEnvironment(scope))
      transport = new ZioClientStdioTransport(
        command,
        env,
        workDir,
        timeout,
        scope,
        process,
        maxLineLength,
        writeQueue,
        pending,
        incomingRef,
        closingRef
      )
      _ <- transport.startReader
      _ <- transport.startStderr
    yield transport

  def scoped(
      command: List[String],
      env: Map[String, String] = Map.empty,
      workDir: Option[File] = None,
      timeout: FiniteDuration = ClientTransport.defaultTimeout,
      maxLineLength: Int = StdioFraming.defaultMaxLineLength
  ): ZIO[Scope, Throwable, ZioClientStdioTransport] =
    ZIO.acquireRelease(apply(command, env, workDir, timeout, maxLineLength))(_.close().ignore)

  def layer(
      command: List[String],
      env: Map[String, String] = Map.empty,
      workDir: Option[File] = None,
      timeout: FiniteDuration = ClientTransport.defaultTimeout,
      maxLineLength: Int = StdioFraming.defaultMaxLineLength
  ): ZLayer[Any, Throwable, ZioClientStdioTransport] =
    ZLayer.scoped(scoped(command, env, workDir, timeout, maxLineLength))

  private[zio] def lines(bytes: ZStream[Any, Throwable, Byte], maxLineLength: Int): ZStream[Any, Throwable, String] =
    bytes.chunks
      .concat(ZStream.succeed(Chunk.single(StdioFraming.newline)))
      .mapAccumZIO(Chunk.empty[Byte]): (buffered, chunk) =>
        split(buffered ++ chunk, maxLineLength) match
          case Some(framed) => ZIO.succeed(framed)
          case None         => ZIO.fail(McpLineTooLongException(maxLineLength))
      .flattenChunks
      .filter(_.nonEmpty)

  private def split(bytes: Chunk[Byte], maxLineLength: Int): Option[(Chunk[Byte], Chunk[String])] =
    @tailrec
    def loop(remaining: Chunk[Byte], lines: Chunk[String]): Option[(Chunk[Byte], Chunk[String])] =
      remaining.indexWhere(_ == StdioFraming.newline) match
        case -1                       => if remaining.size > maxLineLength then None else Some((remaining, lines))
        case at if at > maxLineLength => None
        case at                       =>
          val (line, rest) = remaining.splitAt(at)
          loop(rest.drop(1), lines :+ StdioFraming.decodeLine(line.toArray))

    loop(bytes, Chunk.empty)
