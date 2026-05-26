package chimp.client.integration

import com.dimafeng.testcontainers.ToxiproxyContainer
import eu.rekawek.toxiproxy.model.ToxicDirection
import org.testcontainers.containers.Network
import org.testcontainers.utility.DockerImageName
import sttp.model.Uri

import scala.jdk.CollectionConverters.*

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

  def addLatencyMs(durationMs: Long): Unit = containerProxy.foreach: p =>
    val _ = p.toxics().latency("latency_down", ToxicDirection.DOWNSTREAM, durationMs)
    val _ = p.toxics().latency("latency_up", ToxicDirection.UPSTREAM, durationMs)

  def clearToxics(): Unit = containerProxy.foreach: p =>
    p.toxics().getAll.asScala.foreach(_.remove())
