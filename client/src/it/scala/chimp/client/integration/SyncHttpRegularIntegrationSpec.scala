package chimp.client.integration

import chimp.client.transport.{HttpTransport, Transport}
import sttp.client4.{Backend, DefaultSyncBackend}
import sttp.model.Uri
import sttp.shared.Identity

class SyncHttpRegularIntegrationSpec
    extends HttpIntegrationSpec[Identity, Backend[Identity]]
    with SyncFutureFixtures:

  override def usingBackend[A](use: Backend[Identity] => Identity[A]): Identity[A] =
    val b = DefaultSyncBackend()
    try use(b)
    finally b.close()

  override def usingTransport[A](backend: Backend[Identity], uri: Uri)(use: Transport[Identity] => Identity[A]): Identity[A] =
    val t = HttpTransport[Identity](backend, uri)
    try use(t)
    finally t.close()
