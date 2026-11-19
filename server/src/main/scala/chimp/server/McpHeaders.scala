package chimp.server

import io.circe.syntax.*
import io.circe.{Json, JsonObject}
import sttp.apispec.circe.*
import sttp.tapir.docs.apispec.schema.TapirSchemaToJsonSchema

import java.nio.ByteBuffer
import java.nio.charset.{CodingErrorAction, StandardCharsets}
import java.util.{Base64, Locale}
import scala.util.Try

/** A tool input property mirrored into the `Mcp-Param-{name}` HTTP header.
  *
  * @param path
  *   The chain of `properties` keys from the input schema root to the property.
  * @param name
  *   The `x-mcp-header` value: the name part of the header.
  */
case class McpHeader(path: List[String], name: String)

/** The `x-mcp-header` schema extension and the Base64 sentinel form of header values. */
private[server] object McpHeaders:
  val Key = "x-mcp-header"
  val ParamPrefix = "Mcp-Param-"

  private val SentinelPrefix = "=?base64?"
  private val SentinelSuffix = "?="
  private val Base64Alphabet = "^[A-Za-z0-9+/]*={0,2}$".r
  private val PrimitiveTypes = Set("string", "integer", "boolean")
  private val TokenSymbols = "!#$%&'*+-.^_`|~".toSet
  private val SchemaMaps = Set("properties", "patternProperties", "$defs", "definitions", "dependentSchemas")
  private val NonSchemaValues = Set("default", "examples", "example", "const", "enum", Key)

  def render(schema: ToolSchema, showMetadata: Boolean): Json = schema match
    case ToolSchema.Derived(schema) =>
      val base = TapirSchemaToJsonSchema(schema, markOptionsAsNullable = false)
      (if showMetadata then base else base.copy($schema = None)).asJson
    case ToolSchema.Raw(json) => json

  /** The input schema with every header in `headers` added as an `x-mcp-header` annotation. */
  def annotated(schema: Json, headers: List[McpHeader]): Either[String, Json] =
    headers.foldLeft[Either[String, Json]](Right(schema))((acc, header) => acc.flatMap(annotate(_, header)))

  private def annotate(schema: Json, header: McpHeader): Either[String, Json] =
    def at(json: Json, path: List[String]): Option[Json] = path match
      case Nil         => json.asObject.map(node => Json.fromJsonObject(node.add(Key, header.name.asJson)))
      case key :: rest =>
        for
          node <- json.asObject
          properties <- node("properties").flatMap(_.asObject)
          child <- properties(key)
          updated <- at(child, rest)
        yield Json.fromJsonObject(node.add("properties", Json.fromJsonObject(properties.add(key, updated))))
    Option
      .when(header.path.nonEmpty)(at(schema, header.path))
      .flatten
      .toRight(s"no input property at ${header.path.mkString(".")} reachable through properties keys only")

  /** The violations of the `x-mcp-header` constraints in the input schema. */
  def problems(schema: Json): List[String] =
    val found = annotations(schema)
    val names = found.flatMap((_, node) => node(Key).flatMap(_.asString))
    val duplicates = names
      .groupBy(_.toLowerCase(Locale.ROOT))
      .values
      .collect { case same if same.size > 1 => s"x-mcp-header '${same.head}' is used more than once (names are case-insensitive)" }
    found.flatMap(problem) ++ duplicates

  /** The headers which the input schema declares, from annotations on properties reachable through `properties` keys only. */
  def params(schema: Json): List[McpHeader] =
    for
      (path, node) <- annotations(schema)
      name <- node(Key).flatMap(_.asString)
      propertyPath <- propertyPath(path)
    yield McpHeader(propertyPath, name)

  private def problem(path: List[String], node: JsonObject): Option[String] =
    val location = path.mkString("#/", "/", "")
    def invalid(reason: String) = Some(s"x-mcp-header at $location $reason")
    node(Key).flatMap(_.asString) match
      case None                                              => invalid("must be a string")
      case Some(name) if name.isEmpty                        => invalid("must not be empty")
      case Some(name) if name.exists(Character.isISOControl) => invalid("must not contain control characters")
      case Some(name) if !name.forall(isTokenChar)           => invalid(s"must be an HTTP token, got '$name'")
      case Some(_) if propertyPath(path).isEmpty             => invalid("must be on a property reachable through properties keys only")
      case Some(_) if !node("type").exists(isPrimitiveType)  => invalid("must be on a string, integer or boolean property")
      case Some(_)                                           => None

  private def isTokenChar(c: Char): Boolean = (c < 128 && c.isLetterOrDigit) || TokenSymbols.contains(c)

  private def isPrimitiveType(json: Json): Boolean =
    json.asString.exists(PrimitiveTypes.contains) ||
      json.asArray
        .flatMap(_.flatMap(_.asString).filterNot(_ == "null").toList match
          case single :: Nil => Some(PrimitiveTypes.contains(single))
          case _             => None)
        .contains(true)

  private def propertyPath(path: List[String]): Option[List[String]] =
    val steps = path.grouped(2).toList
    Option.when(path.nonEmpty && path.length % 2 == 0 && steps.forall(_.head == "properties"))(steps.map(_(1)))

  /** Every schema node carrying `x-mcp-header`, with its path from the root as JSON keys (array indices as strings). */
  private def annotations(schema: Json): List[(List[String], JsonObject)] =
    def walk(json: Json, path: List[String]): List[(List[String], JsonObject)] =
      json.asObject.toList.flatMap: node =>
        val own = node(Key).map(_ => (path, node)).toList
        val nested = node.toList.flatMap:
          case (key, _) if NonSchemaValues.contains(key) => Nil
          case (key, value) if SchemaMaps.contains(key)  =>
            value.asObject.toList.flatMap(_.toList.flatMap((name, sub) => walk(sub, path ++ List(key, name))))
          case (key, value) =>
            value.asArray match
              case Some(items) => items.toList.zipWithIndex.flatMap((item, i) => walk(item, path ++ List(key, i.toString)))
              case None        => walk(value, path :+ key)
        own ++ nested
    walk(schema, Nil)

  /** Decodes a header value which may use the Base64 sentinel form `=?base64?...?=`; a value without the full wrapper is literal. */
  def decode(value: String): Either[String, String] =
    val wrapped =
      value.length >= SentinelPrefix.length + SentinelSuffix.length && value.startsWith(SentinelPrefix) && value.endsWith(SentinelSuffix)
    if value.exists(c => (c < ' ' && c != '\t') || c > '~') then Left("contains invalid characters")
    else if !wrapped then Right(value)
    else
      val encoded = value.substring(SentinelPrefix.length, value.length - SentinelSuffix.length)
      val bytes = Option
        .when(encoded.length % 4 == 0 && Base64Alphabet.matches(encoded))(Try(Base64.getDecoder.decode(encoded)).toOption)
        .flatten
      bytes.toRight("is not valid Base64").flatMap(utf8)

  private def utf8(bytes: Array[Byte]): Either[String, String] =
    val decoder =
      StandardCharsets.UTF_8.newDecoder().onMalformedInput(CodingErrorAction.REPORT).onUnmappableCharacter(CodingErrorAction.REPORT)
    Try(decoder.decode(ByteBuffer.wrap(bytes)).toString).toOption.toRight("is not valid UTF-8 after Base64 decoding")

  /** Whether a decoded header value equals the argument: numbers compare numerically, booleans as `true`/`false`, strings literally. */
  def matches(header: String, argument: Json): Boolean =
    argument.fold(
      jsonNull = false,
      jsonBoolean = _.toString == header,
      jsonNumber = number => Try(BigDecimal(header)).toOption.exists(value => number.toBigDecimal.exists(_.compare(value) == 0)),
      jsonString = _ == header,
      jsonArray = _ => false,
      jsonObject = _ => false
    )

  /** The `-32020` message for the first `Mcp-Param-*` header of `params` which does not mirror its argument. A null or absent argument
    * expects no header.
    */
  def mismatch(params: List[McpHeader], arguments: Option[Json], header: String => Option[String]): Option[String] =
    params.iterator
      .map: param =>
        val name = ParamPrefix + param.name
        val argument = param.path.foldLeft(arguments)((json, key) => json.flatMap(_.asObject).flatMap(_(key))).filterNot(_.isNull)
        (argument, header(name).map(decode)) match
          case (None, None)                  => None
          case (Some(_), None)               => Some(s"$name header missing for an argument present in the body")
          case (_, Some(Left(problem)))      => Some(s"$name header $problem")
          case (None, Some(Right(_)))        => Some(s"$name header present, but the argument is absent from the body")
          case (Some(value), Some(Right(h))) => Option.unless(matches(h, value))(s"$name header does not match the argument")
      .collectFirst { case Some(message) => message }

  /** Throws [[IllegalArgumentException]] when the tool's input schema breaks the `x-mcp-header` constraints. */
  def validate(tool: String, schema: ToolSchema, headers: List[McpHeader]): Unit =
    val annotated = McpHeaders.annotated(render(schema, showMetadata = false), headers)
    val found = annotated.fold(List(_), problems)
    require(found.isEmpty, s"Tool $tool has an invalid x-mcp-header: ${found.mkString("; ")}")
