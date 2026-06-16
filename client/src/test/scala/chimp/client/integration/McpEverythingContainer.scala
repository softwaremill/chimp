package chimp.client.integration

import com.dimafeng.testcontainers.GenericContainer
import org.testcontainers.containers.Network
import org.testcontainers.containers.wait.strategy.Wait
import sttp.model.Uri

import java.time.Duration

class McpEverythingContainer(network: Option[Network] = None, networkAlias: String = "everything")
    extends GenericContainer(
      dockerImage = "node:24-alpine",
      exposedPorts = Seq(3001),
      command = Seq("npx", "-y", "@modelcontextprotocol/server-everything@2026.1.26", "streamableHttp"),
      waitStrategy = Some(Wait.forListeningPort().withStartupTimeout(Duration.ofMinutes(2)))
    ):
  network.foreach: n =>
    container.withNetwork(n)
    container.withNetworkAliases(networkAlias)

  def mcpUri: Uri = Uri.unsafeParse(s"http://$containerIpAddress:${mappedPort(3001)}/mcp")

  def alias: String = networkAlias
