package chimp.client.capabilities

import chimp.protocol.ListRootsResult

/** Host-side handler for `roots/list` requests from the server. Required when the client advertises the `roots` capability. */
trait Roots[F[_]]:
  def list(): F[ListRootsResult]
