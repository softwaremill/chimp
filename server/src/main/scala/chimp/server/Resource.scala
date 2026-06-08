
package chimp.server

import chimp.protocol.{Resource, ResourceContents, ResourceTemplate}
import sttp.shared.Identity

import java.util.regex.Pattern
import scala.util.matching.Regex

/** A failure to read a resource. Surfaced to the client as a JSON-RPC `-32602` error; the optional `uri` is included in the error `data`.
  */
case class ResourceError(message: String, uri: Option[String] = None)

/** Creates a new static MCP resource description for the given URI. */
def resource(uri: String): PartialResource = PartialResource(uri)

/** Creates a new MCP resource template description for the given RFC-6570 level-1 URI template (e.g. `test://item/{id}`). */
def resourceTemplate(uriTemplate: String): PartialResourceTemplate = PartialResourceTemplate(uriTemplate)

/** Describes a static resource before its read logic is specified. */
case class PartialResource(
    uri: String,
    name: Option[String] = None,
    title: Option[String] = None,
    description: Option[String] = None,
    mimeType: Option[String] = None,
    size: Option[Long] = None
):
  def name(value: String): PartialResource = copy(name = Some(value))
  def title(value: String): PartialResource = copy(title = Some(value))
  def description(value: String): PartialResource = copy(description = Some(value))
  def mimeType(value: String): PartialResource = copy(mimeType = Some(value))
  def size(value: Long): PartialResource = copy(size = Some(value))

  /** Combine the resource description with logic that reads its contents in the F-effect. */
  def read[F[_]](logic: () => F[Either[ResourceError, List[ResourceContents]]]): ServerResource[F] =
    ServerResource(definition, logic)

  /** Same as [[read]], but with synchronous logic. */
  def handle(logic: () => Either[ResourceError, List[ResourceContents]]): ServerResource[Identity] =
    ServerResource(definition, logic)

  private def definition: Resource = Resource(uri, name.getOrElse(uri), title, description, mimeType, size)

/** A static resource that can be read from the MCP server. */
case class ServerResource[F[_]](definition: Resource, read: () => F[Either[ResourceError, List[ResourceContents]]])

/** Describes a resource template before its read logic is specified. */
case class PartialResourceTemplate(
    uriTemplate: String,
    name: Option[String] = None,
    title: Option[String] = None,
    description: Option[String] = None,
    mimeType: Option[String] = None
):
  def name(value: String): PartialResourceTemplate = copy(name = Some(value))
  def title(value: String): PartialResourceTemplate = copy(title = Some(value))
  def description(value: String): PartialResourceTemplate = copy(description = Some(value))
  def mimeType(value: String): PartialResourceTemplate = copy(mimeType = Some(value))

  /** Combine the template with logic that reads contents for a concrete URI, given the extracted template variables and the matched URI. */
  def read[F[_]](
      logic: (Map[String, String], String) => F[Either[ResourceError, List[ResourceContents]]]
  ): ServerResourceTemplate[F] =
    ServerResourceTemplate(definition, UriTemplate.compile(uriTemplate), logic)

  /** Same as [[read]], but with synchronous logic. */
  def handle(
      logic: (Map[String, String], String) => Either[ResourceError, List[ResourceContents]]
  ): ServerResourceTemplate[Identity] =
    ServerResourceTemplate(definition, UriTemplate.compile(uriTemplate), logic)

  private def definition: ResourceTemplate = ResourceTemplate(uriTemplate, name.getOrElse(uriTemplate), title, description, mimeType)

/** A resource template that can be read from the MCP server. */
case class ServerResourceTemplate[F[_]](
    definition: ResourceTemplate,
    matcher: UriTemplate,
    read: (Map[String, String], String) => F[Either[ResourceError, List[ResourceContents]]]
)

/** A compiled RFC-6570 level-1 URI template. `{var}` segments match a single path segment and are extracted by name. */
final class UriTemplate private (regex: Regex, names: List[String]):
  def matchUri(uri: String): Option[Map[String, String]] =
    regex.findFirstMatchIn(uri).map(m => names.zipWithIndex.map((n, i) => n -> m.group(i + 1)).toMap)

object UriTemplate:
  private val VarPattern: Regex = "\\{([^}]+)\\}".r

  def compile(template: String): UriTemplate =
    val names = scala.collection.mutable.ListBuffer.empty[String]
    val regex = new StringBuilder("^")
    var last = 0
    for m <- VarPattern.findAllMatchIn(template) do
      regex.append(Pattern.quote(template.substring(last, m.start)))
      regex.append("([^/]+)")
      names += m.group(1)
      last = m.end
    regex.append(Pattern.quote(template.substring(last)))
    regex.append("$")
    new UriTemplate(regex.toString.r, names.toList)
