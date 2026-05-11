package chimp.client.internal

import chimp.protocol.RequestId

import java.util.concurrent.atomic.AtomicLong

final class Correlator:
  private val counter = AtomicLong(0L)
  def nextId(): RequestId = RequestId(counter.incrementAndGet().toInt)
