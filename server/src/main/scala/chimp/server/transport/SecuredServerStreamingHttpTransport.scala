package chimp.server.transport

import chimp.protocol.ProgressToken
import chimp.server.*
import io.circe.Json
import sttp.model.Header
import sttp.monad.MonadError
import sttp.monad.syntax.*
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint

/** Implementation of bidirectional MCP server using Streamable HTTP, protected by the security logic of a [[SecuredMcpServer]]. The
  * security logic runs before any MCP message is handled. If it gives a rejection, the server sends the error output and no tool logic
  * runs. If it gives a principal, the principal goes to the tool logic, together with the [[StreamingServerContext]].
  *
  * Wraps a [[StreamingBackend]] instead of extending it, so that the same backend instance - for example an
  * `chimp.server.ox.OxServerHttpTransport` - serves both a plain [[StreamingMcpServer]] and a [[SecuredStreamingMcpServer]].
  *
  * @param path
  *   The MCP endpoint path.
  * @param backend
  *   The streaming machinery for the effect type `F` and the streaming capability `Caps`.
  */
final case class SecuredServerStreamingHttpTransport[F[_], Caps, S, E, P](path: List[String], backend: StreamingBackend[F, Caps]):
  def serve(server: SecuredStreamingMcpServer[F, S, E, P]): ServerEndpoint[Caps, F] =
    val handler = new McpHandler[F, SecuredStreamingServerContext[F, P]](server)
    val mcpEndpoint = endpoint.post
      .securityIn(server.securityInput)
      .in(path.foldLeft(emptyInput)((inputSoFar, pathComponent) => inputSoFar / pathComponent))
      .in(extractFromRequest(_.headers))
      .in(jsonBody[Json])
      .errorOut(server.errorOutput)
      .out(statusCode)
      .out(backend.sseBody)

    ServerEndpoint(
      mcpEndpoint,
      server.securityLogic,
      me => { (principal: P) => (input: (Seq[Header], Json)) =>
        val (headers, json) = input
        given MonadError[F] = me
        respondWithEventStream(server.originCheck, headers, backend) { sink =>
          val makeContext: Option[ProgressToken] => SecuredStreamingServerContext[F, P] =
            token => SinkSecuredStreamingServerContext(sink, token, principal)
          handler.handleJsonRpc(json, headers, makeContext).map(_.body)
        }.map(Right(_))
      }
    )
