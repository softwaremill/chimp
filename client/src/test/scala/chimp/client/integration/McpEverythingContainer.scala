package chimp.client.integration

import com.dimafeng.testcontainers.GenericContainer
import org.testcontainers.containers.Network
import org.testcontainers.containers.wait.strategy.{Wait, WaitAllStrategy, WaitStrategy}
import sttp.model.Uri

import java.time.Duration

class McpEverythingContainer(network: Option[Network] = None, networkAlias: String = "everything")
    extends GenericContainer(
      dockerImage = "node:24-alpine",
      exposedPorts = Seq(McpEverythingContainer.port),
      command = Seq("npx", "-y", "@modelcontextprotocol/server-everything@2026.1.26", "streamableHttp"),
      waitStrategy = Some(McpEverythingContainer.waitStrategy)
    ):
  network.foreach: n =>
    container.withNetwork(n)
    container.withNetworkAliases(networkAlias)

  def mcpUri: Uri = Uri.unsafeParse(s"http://$containerIpAddress:${mappedPort(McpEverythingContainer.port)}/mcp")

  def alias: String = networkAlias

object McpEverythingContainer:
  private val port = 3001

  private def waitStrategy: WaitStrategy =
    WaitAllStrategy()
      .withStartupTimeout(Duration.ofMinutes(2))
      .withStrategy(Wait.forLogMessage(s".*MCP Streamable HTTP Server listening on port $port.*", 1))
      .withStrategy(Wait.forListeningPort())
