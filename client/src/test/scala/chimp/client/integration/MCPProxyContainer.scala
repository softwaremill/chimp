package chimp.client.integration

import com.dimafeng.testcontainers.ToxiproxyContainer
import org.testcontainers.containers.Network
import org.testcontainers.utility.DockerImageName
import sttp.model.Uri

class MCPProxyContainer(network: Network, upstreamAlias: String, upstreamPort: Int)
    extends ToxiproxyContainer(DockerImageName.parse("ghcr.io/shopify/toxiproxy:2.9.0")):
  container.withNetwork(network)

  @volatile private var containerProxy: Option[ToxiproxyContainer.ContainerProxy] = None

  override def start(): Unit =
    super.start()
    containerProxy = Some(proxy(upstreamAlias, upstreamPort))

  def mcpUri: Uri =
    val p = containerProxy.getOrElse(throw IllegalStateException("MCPProxyContainer not started"))
    Uri.unsafeParse(s"http://$containerIpAddress:${p.getProxyPort}/mcp")

  def cutConnections(): Unit = containerProxy.foreach(_.setConnectionCut(true))

  def restoreConnections(): Unit = containerProxy.foreach(_.setConnectionCut(false))
