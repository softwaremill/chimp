package chimp.server

import chimp.protocol.{Resource, ResourceContents, ResourceTemplate}
import sttp.shared.Identity

import java.util.regex.Pattern
import scala.util.matching.Regex

case class ResourceError(message: String, uri: Option[String] = None)

def resource(uri: String): PartialResource = PartialResource(uri)

def resourceTemplate(uriTemplate: String): PartialResourceTemplate = PartialResourceTemplate(uriTemplate)

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

  def read[F[_]](logic: () => F[Either[ResourceError, List[ResourceContents]]]): ServerResource[F] =
    ServerResource(definition, logic)

  def handle(logic: () => Either[ResourceError, List[ResourceContents]]): ServerResource[Identity] =
    ServerResource(definition, logic)

  private def definition: Resource = Resource(uri, name.getOrElse(uri), title, description, mimeType, size)

end PartialResource

case class ServerResource[F[_]](definition: Resource, read: () => F[Either[ResourceError, List[ResourceContents]]])

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

  def serverLogic[F[_]](
      logic: (Map[String, String], String) => F[Either[ResourceError, List[ResourceContents]]]
  ): ServerResourceTemplate[F] =
    ServerResourceTemplate(definition, UriTemplate.compile(uriTemplate), logic)

  private def definition: ResourceTemplate =
    ResourceTemplate(uriTemplate, name.getOrElse(uriTemplate), title, description, mimeType)

end PartialResourceTemplate

case class ServerResourceTemplate[F[_]](
    definition: ResourceTemplate,
    matcher: UriTemplate,
    read: (Map[String, String], String) => F[Either[ResourceError, List[ResourceContents]]]
)

final class UriTemplate private (regex: Regex, names: List[String]):
  def matchUri(uri: String): Option[Map[String, String]] =
    regex.findFirstMatchIn(uri).map(m => names.zipWithIndex.map((n, i) => n -> m.group(i + 1)).toMap)

object UriTemplate:
  private val VarPattern: Regex = "\\{([^}]+)\\}".r

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
