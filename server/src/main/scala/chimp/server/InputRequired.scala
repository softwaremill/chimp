package chimp.server

import chimp.protocol.*
import io.circe.Json

/** The outcome of a tool, prompt or resource handler which needs input from the client before it can finish (multi round-trip requests,
  * 2026-07-28). The client answers each request under its key, echoes the request state, and retries the same request; the handler reads
  * the answers from [[RequestInfo.inputResponses]] and the state from [[RequestInfo.requestState]]. It always carries at least one input
  * request or a request state.
  */
final case class InputRequired private (requests: Map[String, InputRequest], requestState: Option[String]):
  /** Adds an input request under `key`; a request already under `key` is replaced. */
  def and(key: String, request: InputRequest): InputRequired = copy(requests = requests + (key -> request))

  /** Sets the opaque state which the client echoes on its retry. */
  def withRequestState(state: String): InputRequired = copy(requestState = Some(state))

  private[server] def mapRequestState(f: String => String): InputRequired = copy(requestState = requestState.map(f))

object InputRequired:
  def apply(first: (String, InputRequest), rest: (String, InputRequest)*): InputRequired =
    new InputRequired((first +: rest).toMap, None)

  /** An outcome without input requests: the client retries at once, with `state`. */
  def withRequestState(state: String): InputRequired = new InputRequired(Map.empty, Some(state))

  /** Asks the user to fill in a form described by a JSON Schema; the client must declare the `elicitation` (form) capability. */
  def elicitForm(message: String, requestedSchema: Json): InputRequest =
    InputRequest.Elicit(ElicitRequest(params = ElicitParams.Form(message, requestedSchema)))

  /** Asks the user to complete an interaction at `url`, out of band; the client must declare the `elicitation.url` capability. */
  def elicitUrl(message: String, url: String): InputRequest =
    InputRequest.Elicit(ElicitRequest(params = ElicitParams.Url(message, url)))

  /** Asks the client to sample its LLM; the client must declare the `sampling` capability. */
  def sample(params: CreateMessageParams): InputRequest = InputRequest.Sampling(CreateMessageRequest(params = params))

  /** Asks the client for its roots; the client must declare the `roots` capability. */
  val listRoots: InputRequest = InputRequest.Roots(ListRootsRequest())

  /** The client capabilities which `request` needs. */
  private def capabilitiesFor(request: InputRequest): ClientCapabilities = request match
    case InputRequest.Sampling(_)                                    => ClientCapabilities(sampling = Some(Json.obj()))
    case InputRequest.Roots(_)                                       => ClientCapabilities(roots = Some(ClientRootsCapability()))
    case InputRequest.Elicit(ElicitRequest(_, _: ElicitParams.Form)) =>
      ClientCapabilities(elicitation = Some(Json.obj("form" -> Json.obj())))
    case InputRequest.Elicit(ElicitRequest(_, _: ElicitParams.Url)) => ClientCapabilities(elicitation = Some(Json.obj("url" -> Json.obj())))

  /** Whether `declared` covers what `request` needs. An empty `elicitation` object means form mode only, as in 2025-11-25. */
  private[server] def isDeclared(request: InputRequest, declared: ClientCapabilities): Boolean =
    def elicitationHas(mode: String, emptyMeans: Boolean): Boolean =
      declared.elicitation.flatMap(_.asObject).exists(modes => modes.contains(mode) || (modes.isEmpty && emptyMeans))
    request match
      case InputRequest.Sampling(_)                                    => declared.sampling.isDefined
      case InputRequest.Roots(_)                                       => declared.roots.isDefined
      case InputRequest.Elicit(ElicitRequest(_, _: ElicitParams.Form)) => elicitationHas("form", emptyMeans = true)
      case InputRequest.Elicit(ElicitRequest(_, _: ElicitParams.Url))  => elicitationHas("url", emptyMeans = false)

  /** The client capabilities which `requests` need together, as `requiredCapabilities` of a `-32021` error names them. */
  private[server] def requiredCapabilities(requests: Iterable[InputRequest]): ClientCapabilities =
    requests
      .map(capabilitiesFor)
      .foldLeft(ClientCapabilities()): (all, one) =>
        ClientCapabilities(
          roots = all.roots.orElse(one.roots),
          sampling = all.sampling.orElse(one.sampling),
          elicitation = (all.elicitation ++ one.elicitation).reduceOption(_.deepMerge(_))
        )
