package chimp.client.integration

import chimp.client.transport.{HttpTransport, Transport}
import sttp.client4.{Backend, DefaultSyncBackend}
import sttp.model.Uri
import sttp.shared.Identity

class SyncHttpIntegrationSpec extends HttpIntegrationSpec[Identity, Backend[Identity]] with SyncToFuture:

  override def usingBackend[A](use: Backend[Identity] => Identity[A]): Identity[A] =
    val backend = DefaultSyncBackend()
    try use(backend)
    finally backend.close()

  override def usingTransport[A](backend: Backend[Identity], uri: Uri)(use: Transport[Identity] => Identity[A]): Identity[A] =
    val transport = HttpTransport[Identity](backend, uri)
    try use(transport)
    finally transport.close()
