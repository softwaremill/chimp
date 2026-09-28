package chimp.protocol

import io.circe.Codec

/** An icon for a resource, tool, prompt, or implementation (2026-07-28). `src` is a URI (http(s) or data:); the other fields are optional
  * hints. Present on `icons` fields across the shared types.
  */
final case class Icon(
    src: String,
    mimeType: Option[String] = None,
    sizes: Option[List[String]] = None,
    theme: Option[String] = None
) derives Codec

/** Optional annotations describing how an object or content should be used (2026-07-28): its intended `audience`, a `priority` (0.0-1.0),
  * and a `lastModified` ISO-8601 timestamp.
  */
final case class Annotations(
    audience: Option[List[Role]] = None,
    lastModified: Option[String] = None,
    priority: Option[Double] = None
) derives Codec
