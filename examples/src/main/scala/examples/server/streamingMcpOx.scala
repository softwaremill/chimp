//> using dep com.softwaremill.chimp::chimp-server-ox:0.3.0
//> using dep ch.qos.logback:logback-classic:1.5.37

package examples.server

import chimp.protocol.LoggingLevel
import chimp.server.*
import chimp.server.ox.OxServerHttpTransport
import io.circe.{Codec, Json}
import sttp.shared.Identity
import sttp.tapir.*
import sttp.tapir.server.netty.sync.NettySyncServer

case class WorkInput(steps: Int) derives Codec, Schema

@main def mcpStreamingOxApp(): Unit =
  val work = tool("work")
    .description("Reports progress and logs while running")
    .input[WorkInput]
    .streamingServerLogic[Identity]: (in, ctx, _) =>
      for step <- 1 to in.steps do
        ctx.reportProgress(step.toDouble / in.steps, total = Some(1.0))
        ctx.log(LoggingLevel.Info, Json.fromString(s"step $step of ${in.steps}"))
      ToolResult.text("done")

  val server = StreamingMcpServer[Identity]().withLoggingLevel(_ => ()).addStreamingTool(work)
  val endpoint = OxServerHttpTransport(List("mcp")).serve(server)

  NettySyncServer().port(8080).addEndpoint(endpoint).startAndWait()
