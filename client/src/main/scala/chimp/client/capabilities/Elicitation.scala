package chimp.client.capabilities

import chimp.protocol.{ElicitRequest, ElicitResult}

/** Host-side handler for `elicitation/create` requests from the server. Required when the client advertises the `elicitation` capability. */
trait Elicitation[F[_]]:
  def elicit(req: ElicitRequest): F[ElicitResult]
