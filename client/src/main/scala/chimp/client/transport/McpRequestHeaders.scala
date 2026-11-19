package chimp.client.transport

import chimp.protocol.{JSONRPCMessage, ProtocolMeta, ProtocolVersion}
import io.circe.Json
import org.slf4j.LoggerFactory
import sttp.model.Header

import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicReference
import java.util.{Base64, Locale}

/** The request headers of the stateless wire (2026-07-28): `Mcp-Method`, `Mcp-Name` and the `Mcp-Param-*` headers that mirror tool
  * arguments annotated with `x-mcp-header`.
  */
private[client] object McpRequestHeaders:
  private val Key = "x-mcp-header"
  private val ParamPrefix = "Mcp-Param-"
  private val PrimitiveTypes = Set("string", "integer", "boolean")
  private val TokenSymbols = "!#$%&'*+-.^_`|~".toSet
  private val SentinelPrefix = "=?base64?"
  private val SentinelSuffix = "?="
  private val SchemaMaps = Set("properties", "patternProperties", "$defs", "definitions", "dependentSchemas")
  private val NonSchemaValues = Set("default", "examples", "example", "const", "enum", Key)

  // the body field that the `Mcp-Name` header mirrors, per method
  private val nameFields: Map[String, String] = Map("tools/call" -> "name", "prompts/get" -> "name", "resources/read" -> "uri")

  /** The version that a request names in `_meta`. */
  def metaVersion(msg: JSONRPCMessage): Option[String] = msg match
    case JSONRPCMessage.Request(_, _, Some(params), _) =>
      params.hcursor.downField("_meta").get[String](ProtocolMeta.ProtocolVersionKey).toOption
    case _ => None

  def isStateless(msg: JSONRPCMessage): Boolean = metaVersion(msg).flatMap(ProtocolVersion.from).exists(_.isModern)

  /** `Mcp-Method` on every request, and `Mcp-Name` on a request with a name-shaped body field. Notifications and responses carry none. */
  def standard(msg: JSONRPCMessage): List[Header] = msg match
    case JSONRPCMessage.Request(_, method, params, _) =>
      val name = nameFields.get(method).flatMap(field => params.flatMap(_.hcursor.get[String](field).toOption))
      Header("Mcp-Method", method) :: name.map(value => Header("Mcp-Name", encode(value))).toList
    case _ => Nil

  /** A value goes as it is when it is printable ASCII without leading or trailing spaces and does not look like the sentinel, else in the
    * Base64 sentinel form.
    */
  def encode(value: String): String =
    val sentinelLike = value.startsWith(SentinelPrefix) && value.endsWith(SentinelSuffix)
    if value.forall(c => c >= ' ' && c <= '~') && value.trim == value && !sentinelLike then value
    else SentinelPrefix + Base64.getEncoder.encodeToString(value.getBytes(StandardCharsets.UTF_8)) + SentinelSuffix

  /** The header value of a tool argument; `null`, arrays and objects have none. */
  def argumentValue(argument: Json): Option[String] =
    argument.fold(
      jsonNull = None,
      jsonBoolean = value => Some(value.toString),
      jsonNumber = number => Some(Json.fromJsonNumber(number).noSpaces),
      jsonString = value => Some(encode(value)),
      jsonArray = _ => None,
      jsonObject = _ => None
    )

  /** A tool input property mirrored into the `Mcp-Param-{name}` header. */
  final case class Param(path: List[String], name: String):
    def header: String = ParamPrefix + name

  /** The params that a tool's input schema declares, or why its annotations are invalid. */
  def params(inputSchema: Json): Either[List[String], List[Param]] =
    val found = annotations(inputSchema, Nil)
    val names = found.flatMap((_, node) => node.hcursor.get[String](Key).toOption)
    val duplicates = names
      .groupBy(_.toLowerCase(Locale.ROOT))
      .values
      .toList
      .collect:
        case same if same.size > 1 => s"x-mcp-header '${same.head}' is used more than once"
    val checked = found.map((path, node) => problem(path, node))
    val problems = checked.collect { case Left(problem) => problem } ++ duplicates
    if problems.isEmpty then Right(checked.collect { case Right(param) => param }) else Left(problems)

  private def problem(path: List[String], node: Json): Either[String, Param] =
    def invalid(reason: String) = Left(s"x-mcp-header at ${path.mkString("#/", "/", "")} $reason")
    (node.hcursor.downField(Key).focus.flatMap(_.asString), propertyPath(path)) match
      case (None, _)                                    => invalid("is not a string")
      case (Some(name), _) if name.isEmpty              => invalid("is empty")
      case (Some(name), _) if !name.forall(isTokenChar) => invalid(s"is not an HTTP token: '$name'")
      case (Some(_), None)                              => invalid("is not on a property reachable through properties keys only")
      case (Some(_), _) if !node.hcursor.downField("type").focus.exists(isPrimitiveType) =>
        invalid("is not on a string, integer or boolean property")
      case (Some(name), Some(properties)) => Right(Param(properties, name))

  private def propertyPath(path: List[String]): Option[List[String]] =
    val steps = path.grouped(2).toList
    Option.when(path.nonEmpty && path.length % 2 == 0 && steps.forall(_.head == "properties"))(steps.map(_(1)))

  /** Every schema node that carries `x-mcp-header`, with its path of JSON keys from the root. */
  private def annotations(json: Json, path: List[String]): List[(List[String], Json)] =
    json.asObject.toList.flatMap: node =>
      val own = node(Key).map(_ => (path, json)).toList
      val nested = node.toList.flatMap:
        case (key, _) if NonSchemaValues.contains(key) => Nil
        case (key, value) if SchemaMaps.contains(key)  =>
          value.asObject.toList.flatMap(_.toList.flatMap((name, sub) => annotations(sub, path ++ List(key, name))))
        case (key, value) =>
          value.asArray match
            case Some(items) => items.toList.zipWithIndex.flatMap((item, i) => annotations(item, path ++ List(key, i.toString)))
            case None        => annotations(value, path :+ key)
      own ++ nested

  private def isTokenChar(c: Char): Boolean = (c < 128 && c.isLetterOrDigit) || TokenSymbols.contains(c)

  private def isPrimitiveType(json: Json): Boolean =
    json.asString.exists(PrimitiveTypes.contains) ||
      json.asArray
        .map(_.flatMap(_.asString).filterNot(_ == "null").toList)
        .exists:
          case single :: Nil => PrimitiveTypes.contains(single)
          case _             => false

/** The `x-mcp-header` params of the tools that an HTTP transport has listed on the stateless wire. */
private[client] final class ToolHeaders:
  import McpRequestHeaders.*

  private val log = LoggerFactory.getLogger(classOf[ToolHeaders])
  private val byTool = AtomicReference(Map.empty[String, List[Param]])

  /** The `Mcp-Param-*` headers of a stateless `tools/call`, for the arguments that are present and not `null`. */
  def paramHeaders(msg: JSONRPCMessage): List[Header] = msg match
    case JSONRPCMessage.Request(_, "tools/call", Some(params), _) if isStateless(msg) =>
      val arguments = params.hcursor.downField("arguments").focus
      val toolParams = params.hcursor.get[String]("name").toOption.flatMap(byTool.get().get).getOrElse(Nil)
      toolParams.flatMap: param =>
        val argument = param.path.foldLeft(arguments)((json, key) => json.flatMap(_.asObject).flatMap(_(key)))
        argument.flatMap(argumentValue).map(Header(param.header, _))
    case _ => Nil

  /** Drops the tools with invalid `x-mcp-header` annotations from a stateless `tools/list` result, and keeps the params of the others. */
  def filterToolsList(request: JSONRPCMessage, response: Option[JSONRPCMessage]): Option[JSONRPCMessage] = (request, response) match
    case (JSONRPCMessage.Request(_, "tools/list", _, _), Some(JSONRPCMessage.Response(jsonrpc, id, result))) if isStateless(request) =>
      result.hcursor.downField("tools").focus.flatMap(_.asArray) match
        case None        => response
        case Some(tools) =>
          val kept = tools.filter: tool =>
            val name = tool.hcursor.get[String]("name").getOrElse("")
            params(tool.hcursor.downField("inputSchema").focus.getOrElse(Json.obj())) match
              case Right(found) =>
                val _ = byTool.updateAndGet(_.updated(name, found))
                true
              case Left(problems) =>
                log.warn(s"Ignoring tool '$name' with an invalid x-mcp-header: ${problems.mkString("; ")}")
                false
          Some(JSONRPCMessage.Response(jsonrpc, id, result.mapObject(_.add("tools", Json.fromValues(kept)))))
    case _ => response
