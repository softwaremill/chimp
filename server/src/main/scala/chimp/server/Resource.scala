package chimp.server

import chimp.protocol.{Resource, ResourceContents, ResourceTemplate}
import sttp.model.Header
import sttp.shared.Identity

import java.util.regex.Pattern
import scala.util.matching.Regex

/** An error returned when reading a resource fails, optionally naming the offending URI. */
case class ResourceError(message: String, uri: Option[String] = None)

/** Starts defining a resource served at the given fixed URI. */
def resource(uri: String): PartialResource = PartialResource(uri)

/** Starts defining a resource template whose URI carries `{variable}` placeholders. */
def resourceTemplate(uriTemplate: String): PartialResourceTemplate = PartialResourceTemplate(uriTemplate)

/** A resource being defined, before its read logic is attached. */
case class PartialResource(
    uri: String,
    name: Option[String] = None,
    title: Option[String] = None,
    description: Option[String] = None,
    mimeType: Option[String] = None,
    size: Option[Long] = None
):
  def name(value: String): PartialResource =
    copy(name = Some(value))

  def title(value: String): PartialResource =
    copy(title = Some(value))

  def description(value: String): PartialResource =
    copy(description = Some(value))

  def mimeType(value: String): PartialResource =
    copy(mimeType = Some(value))

  def size(value: Long): PartialResource =
    copy(size = Some(value))

  /** Attaches effectful logic, with access to the request headers, producing the resource's contents (or an error). */
  def serverLogic[F[_]](logic: Seq[Header] => F[Either[ResourceError, List[ResourceContents]]]): ServerResource[F] =
    ServerResource(definition, logic)

  /** Attaches synchronous logic that also receives the request headers. */
  def handleWithHeaders(logic: Seq[Header] => Either[ResourceError, List[ResourceContents]]): ServerResource[Identity] =
    ServerResource(definition, logic)

  /** Attaches synchronous logic producing the resource's contents (or an error). */
  def handle(logic: () => Either[ResourceError, List[ResourceContents]]): ServerResource[Identity] =
    handleWithHeaders(_ => logic())

  private def definition: Resource = Resource(uri, name.getOrElse(uri), title, description, mimeType, size)

end PartialResource

/** A fully-defined resource: its metadata plus the logic reading its contents. */
case class ServerResource[F[_]](definition: Resource, read: Seq[Header] => F[Either[ResourceError, List[ResourceContents]]])

/** A resource template being defined, before its read logic is attached. */
case class PartialResourceTemplate(
    uriTemplate: String,
    name: Option[String] = None,
    title: Option[String] = None,
    description: Option[String] = None,
    mimeType: Option[String] = None
):
  def name(value: String): PartialResourceTemplate =
    copy(name = Some(value))

  def title(value: String): PartialResourceTemplate =
    copy(title = Some(value))

  def description(value: String): PartialResourceTemplate =
    copy(description = Some(value))

  def mimeType(value: String): PartialResourceTemplate =
    copy(mimeType = Some(value))

  /** Attaches effectful logic reading a matched URI; receives the extracted variables, the full URI, and the request headers. */
  def serverLogic[F[_]](
      logic: (Map[String, String], String, Seq[Header]) => F[Either[ResourceError, List[ResourceContents]]]
  ): ServerResourceTemplate[F] =
    ServerResourceTemplate(definition, UriTemplate.compile(uriTemplate), logic)

  /** Attaches synchronous logic that also receives the request headers. */
  def handleWithHeaders(
      logic: (Map[String, String], String, Seq[Header]) => Either[ResourceError, List[ResourceContents]]
  ): ServerResourceTemplate[Identity] =
    ServerResourceTemplate(definition, UriTemplate.compile(uriTemplate), logic)

  /** Attaches synchronous logic receiving the extracted variables and the full URI. */
  def handle(logic: (Map[String, String], String) => Either[ResourceError, List[ResourceContents]]): ServerResourceTemplate[Identity] =
    handleWithHeaders((vars, uri, _) => logic(vars, uri))

  private def definition: ResourceTemplate =
    ResourceTemplate(uriTemplate, name.getOrElse(uriTemplate), title, description, mimeType)

end PartialResourceTemplate

/** A fully-defined resource template: its metadata, a compiled URI matcher, and the logic reading matched URIs. */
case class ServerResourceTemplate[F[_]](
    definition: ResourceTemplate,
    matcher: UriTemplate,
    read: (Map[String, String], String, Seq[Header]) => F[Either[ResourceError, List[ResourceContents]]]
)

/** A compiled URI template that matches concrete URIs and extracts their `{variable}` values. */
final class UriTemplate private (regex: Regex, names: List[String]):
  /** Returns the extracted variables if `uri` matches, or `None` otherwise. */
  def matchUri(uri: String): Option[Map[String, String]] =
    regex.findFirstMatchIn(uri).map(m => names.zipWithIndex.map((n, i) => n -> m.group(i + 1)).toMap)

object UriTemplate:
  private val VarPattern: Regex = "\\{([^}]+)\\}".r

  /** Compiles a `{variable}` URI template into a matcher; each variable matches one path segment. */
  def compile(template: String): UriTemplate =
    val names = scala.collection.mutable.ListBuffer.empty[String]
    val regex = new StringBuilder("^")
    var last = 0
    for `match` <- VarPattern.findAllMatchIn(template) do
      regex.append(Pattern.quote(template.substring(last, `match`.start)))
      regex.append("([^/]+)")
      names += `match`.group(1)
      last = `match`.end
    regex.append(Pattern.quote(template.substring(last)))
    regex.append("$")
    new UriTemplate(regex.toString.r, names.toList)
