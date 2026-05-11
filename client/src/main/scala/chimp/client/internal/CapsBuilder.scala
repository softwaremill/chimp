package chimp.client.internal

import chimp.protocol.ClientCapabilities

object CapsBuilder:
  inline def wire[F[_], Caps]: ClientCapabilities = ClientCapabilities()
