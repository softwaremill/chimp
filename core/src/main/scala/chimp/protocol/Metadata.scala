package chimp.protocol

import io.circe.{Codec, Decoder, Encoder, Json}

/** The visual theme an [[Icon]] is designed for. Mirrors the closed `["dark", "light"]` string enum declared in both the 2025-11-25 and
  * 2026-07-28 MCP schemas, so an invalid value cannot be put on the wire.
  */
enum IconTheme:
  case Dark, Light

object IconTheme:
  given Encoder[IconTheme] = Encoder.instance:
    case IconTheme.Dark  => Json.fromString("dark")
    case IconTheme.Light => Json.fromString("light")
  given Decoder[IconTheme] = Decoder.decodeString.emap:
    case "dark"  => Right(IconTheme.Dark)
    case "light" => Right(IconTheme.Light)
    case other   => Left(s"Unknown icon theme: $other")

/** A relative-importance hint bounded to the schema-mandated 0.0..1.0 range. Constructing or decoding a value outside that range fails,
  * which a plain `Double` would silently allow onto the wire.
  */
opaque type Priority = Double
object Priority:
  /** Builds a [[Priority]] when `value` is within 0.0..1.0, otherwise returns an error message. */
  def from(value: Double): Either[String, Priority] =
    if value >= 0.0 && value <= 1.0 then Right(value)
    else Left(s"priority must be within 0.0..1.0, got $value")

  /** Unchecked constructor for in-range literals; throws [[IllegalArgumentException]] for an out-of-range value. */
  def apply(value: Double): Priority = from(value).fold(msg => throw IllegalArgumentException(msg), identity)

  def unapply(priority: Priority): Option[Double] = Some(priority)

  extension (priority: Priority) def value: Double = priority

  given Encoder[Priority] = Encoder.encodeDouble
  given Decoder[Priority] = Decoder.decodeDouble.emap(from)

/** An icon for a resource, tool, prompt, or implementation. `src` is a URI (http(s) or data:); the other fields are optional hints. Present
  * on `icons` fields across the shared types.
  */
final case class Icon(
    src: String,
    mimeType: Option[String] = None,
    sizes: Option[List[String]] = None,
    theme: Option[IconTheme] = None
) derives Codec

/** Optional annotations describing how an object or content should be used: its intended `audience`, a `priority` (0.0..1.0) and a
  * `lastModified` ISO-8601 timestamp.
  */
final case class Annotations(
    audience: Option[List[Role]] = None,
    lastModified: Option[String] = None,
    priority: Option[Priority] = None
) derives Codec
