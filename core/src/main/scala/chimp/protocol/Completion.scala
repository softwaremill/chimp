package chimp.protocol

import io.circe.syntax.*
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, Json}

enum CompleteRef:
  case Prompt(prompt: PromptReference)
  case Resource(resource: ResourceReference)

object CompleteRef:
  given Encoder[CompleteRef] = Encoder.instance:
    case Prompt(p)   => p.asJson
    case Resource(r) => r.asJson
  given Decoder[CompleteRef] = Decoder.instance: c =>
    c.downField("type")
      .as[String]
      .flatMap:
        case "ref/prompt"   => c.as[PromptReference].map(Prompt(_))
        case "ref/resource" => c.as[ResourceReference].map(Resource(_))
        case other          => Left(DecodingFailure(s"Unknown CompleteRef type: $other", c.history))

final case class CompleteArgument(name: String, value: String) derives Codec

final case class CompleteContext(arguments: Option[Map[String, String]] = None) derives Codec

final case class CompleteParams(
    ref: CompleteRef,
    argument: CompleteArgument,
    context: Option[CompleteContext] = None,
    _meta: Option[Map[String, Json]] = None
) derives Codec

final case class CompleteRequest(method: String = "completion/complete", params: CompleteParams) derives Codec

final case class Completion(values: List[String], total: Option[Int] = None, hasMore: Option[Boolean] = None) derives Codec

final case class CompleteResult(completion: Completion, _meta: Option[Map[String, Json]] = None) derives Codec
