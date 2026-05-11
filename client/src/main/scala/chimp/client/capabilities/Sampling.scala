package chimp.client.capabilities

import chimp.protocol.{CreateMessageRequest, CreateMessageResult}

/** Host-side handler for `sampling/createMessage` requests from the server. Required when the client advertises the `sampling` capability. */
trait Sampling[F[_]]:
  def createMessage(req: CreateMessageRequest): F[CreateMessageResult]
