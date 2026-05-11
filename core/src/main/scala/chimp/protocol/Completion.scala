package chimp.protocol

import io.circe.{Codec, Json}

enum CompleteRef derives Codec:
  case Prompt(prompt: PromptReference)
  case Resource(resource: ResourceReference)

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
