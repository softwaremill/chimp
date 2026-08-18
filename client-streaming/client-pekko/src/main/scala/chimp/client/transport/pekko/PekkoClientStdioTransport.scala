package chimp.client.transport.pekko

import chimp.client.McpTransportException
import chimp.client.transport.pekko.internal.{PekkoPendingRequests, StateActor}
import chimp.client.transport.{ClientStreamingStdioTransport, ClientTransport}
import chimp.protocol.JSONRPCMessage
import org.apache.pekko.stream.scaladsl.{Framing, Sink, Source, StreamConverters}
import org.apache.pekko.stream.{BoundedSourceQueue, Materializer, QueueOfferResult}
import org.apache.pekko.util.ByteString
import org.slf4j.LoggerFactory
import sttp.monad.{FutureMonad, MonadError}

import java.io.{File, InputStream}
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters.*
import scala.jdk.FutureConverters.*

final class PekkoClientStdioTransport private (
    command: List[String],
    env: Map[String, String],
    workDir: Option[File],
    timeout: FiniteDuration,
    process: Process,
    outbound: BoundedSourceQueue[JSONRPCMessage],
    pending: PekkoPendingRequests
)(using mat: Materializer)
    extends ClientStreamingStdioTransport[Future](command, env, workDir, timeout):

  private val log = LoggerFactory.getLogger(classOf[PekkoClientStdioTransport])

  private given ExecutionContext = mat.executionContext

  private final class State:
    var incoming: JSONRPCMessage => Future[Unit] = _ => Future.unit
    var closed: Boolean = false

    def beginClosing(): Boolean =
      if closed then false
      else
        closed = true
        true

  private val state = StateActor(new State, "chimp-mcp-client-stdio-transport")

  override given monad: MonadError[Future] = FutureMonad()

  override def send(msg: JSONRPCMessage): Future[Option[JSONRPCMessage]] =
    state
      .ask(_.closed)
      .flatMap:
        case true  => Future.failed(McpTransportException("Stdio transport is closed"))
        case false =>
          msg match
            case request: JSONRPCMessage.Request =>
              pending
                .register(request.id, timeout)
                .flatMap(await => offer(msg).flatMap(_ => await()))
                .map(Some(_))
            case other => offer(other).map(_ => None)

  override def onIncoming(handler: JSONRPCMessage => Future[Unit]): Future[Unit] =
    state.tell(_.incoming = handler)
    Future.unit

  override def close(): Future[Unit] =
    state
      .ask(_.beginClosing())
      .flatMap:
        case false => Future.unit
        case true  =>
          outbound.complete()
          val terminate = mat.scheduleOnce(PekkoClientStdioTransport.exitTimeout, () => if process.isAlive then process.destroy())
          val kill = mat.scheduleOnce(
            PekkoClientStdioTransport.exitTimeout * 2,
            () => if process.isAlive then { val _ = process.destroyForcibly() }
          )
          process
            .onExit()
            .asScala
            .map(_ => ())
            .andThen { case _ =>
              val _ = terminate.cancel()
              val _ = kill.cancel()
            }
            .flatMap(_ => pending.closeAll("Transport closed"))
            .map: _ =>
              pending.stop()
              state.stopWhenIdle()

  private def offer(msg: JSONRPCMessage): Future[Unit] =
    outbound.offer(msg) match
      case QueueOfferResult.Enqueued    => Future.unit
      case QueueOfferResult.Dropped     => Future.failed(McpTransportException("The outbound message buffer is full"))
      case QueueOfferResult.QueueClosed => Future.failed(McpTransportException("Stdio transport is closed"))
      case QueueOfferResult.Failure(t)  => Future.failed(McpTransportException("Failed to write to the subprocess", t))

  private def dispatch(msg: JSONRPCMessage): Future[Unit] = msg match
    case response: JSONRPCMessage.Response => pending.complete(response.id, response).map(_ => ())
    case err: JSONRPCMessage.Error         => pending.complete(err.id, err).map(_ => ())
    case other                             => state.ask(_.incoming).flatMap(handler => handler(other))

  private[pekko] def startReader(): Unit =
    val done = PekkoClientStdioTransport
      .lines(process.getInputStream)
      .mapAsync(1): line =>
        ClientTransport.decode(line) match
          case Right(msg) => dispatch(msg)
          case Left(err)  =>
            log.warn(s"Failed to parse JSON-RPC line: ${err.getMessage}; raw: $line")
            Future.unit
      .runWith(Sink.ignore)
    done.onComplete: result =>
      result.failed.foreach(logReaderFailure)
      pending.closeAll("Transport closed")

  private def logReaderFailure(t: Throwable): Unit =
    state.ask(_.closed).foreach(closed => if !closed then log.warn(s"Reader stream ended: ${t.getMessage}"))

  private[pekko] def startStderr(): Unit =
    val _ = PekkoClientStdioTransport
      .lines(process.getErrorStream)
      .runForeach(line => log.info(s"stdio-server: $line"))

object PekkoClientStdioTransport:
  private val maxLineLength = 8 * 1024 * 1024
  private val outboundBufferSize = 256
  private val exitTimeout = 2.seconds

  /** Starts the subprocess described by `command` and creates a transport communicating with it over its standard input and output.
    *
    * @param command
    *   The command starting the MCP server, given as the executable and its arguments.
    * @param env
    *   Additional environment variables for the subprocess.
    * @param workDir
    *   The working directory of the subprocess; by default inherited from the current process.
    * @param timeout
    *   How long to wait for a response to a request sent to the server.
    */
  def apply(
      command: List[String],
      env: Map[String, String] = Map.empty,
      workDir: Option[File] = None,
      timeout: FiniteDuration = ClientTransport.defaultTimeout
  )(using Materializer): PekkoClientStdioTransport =
    val builder = ProcessBuilder(command.asJava)
    workDir.foreach(builder.directory)
    if env.nonEmpty then
      val processEnv = builder.environment()
      env.foreach { (name, value) =>
        val _ = processEnv.put(name, value)
      }
    builder.redirectErrorStream(false)
    val process = builder.start()

    val outbound = Source
      .queue[JSONRPCMessage](outboundBufferSize)
      .map(msg => ByteString(ClientTransport.encode(msg) + "\n"))
      .to(StreamConverters.fromOutputStream(() => process.getOutputStream, autoFlush = true))
      .run()

    val transport = new PekkoClientStdioTransport(command, env, workDir, timeout, process, outbound, PekkoPendingRequests())
    transport.startReader()
    transport.startStderr()
    transport

  private def lines(in: InputStream): Source[String, ?] =
    StreamConverters
      .fromInputStream(() => in)
      .via(Framing.delimiter(ByteString("\n"), maximumFrameLength = maxLineLength, allowTruncation = true))
      .map(_.utf8String.trim)
      .filter(_.nonEmpty)
