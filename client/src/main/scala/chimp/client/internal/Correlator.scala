package chimp.client.internal

import chimp.protocol.RequestId

import java.util.UUID

trait Correlator:
  def nextId(): RequestId

private[client] final class UUIDCorrelator extends Correlator:
  def nextId(): RequestId = RequestId(UUID.randomUUID().toString)
