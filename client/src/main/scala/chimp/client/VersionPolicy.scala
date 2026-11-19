package chimp.client

import chimp.protocol.ProtocolVersion

/** Makes a client detect the era of the server: it sends `server/discover` first, and sends `initialize` when the server is legacy. */
case object DetectVersion

/** How a client chooses its protocol version: a fixed [[chimp.protocol.ProtocolVersion]], with no detection, or [[DetectVersion]]. */
type VersionPolicy = ProtocolVersion | DetectVersion.type
