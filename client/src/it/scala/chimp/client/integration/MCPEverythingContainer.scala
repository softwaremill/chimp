package chimp.client.integration

import com.dimafeng.testcontainers.GenericContainer
import org.testcontainers.containers.wait.strategy.Wait
import sttp.model.Uri

import java.time.Duration

class MCPEverythingContainer
    extends GenericContainer(
      dockerImage = "node:22-alpine",
      exposedPorts = Seq(3001),
      command = Seq("npx", "-y", "@modelcontextprotocol/server-everything", "streamableHttp"),
      waitStrategy = Some(Wait.forListeningPort().withStartupTimeout(Duration.ofMinutes(2)))
    ):
  def mcpUri: Uri = Uri.unsafeParse(s"http://$containerIpAddress:${mappedPort(3001)}/mcp")
