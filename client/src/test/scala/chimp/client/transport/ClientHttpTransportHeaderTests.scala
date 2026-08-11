package chimp.client.transport

import chimp.client.integration.ToFuture
import chimp.protocol.{JSONRPCMessage, RequestId}
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.client4.GenericRequest
import sttp.model.Uri.UriContext
import sttp.model.{Header, Method, Uri}

import scala.concurrent.Future

trait ClientHttpTransportHeaderTests[F[_]] extends AsyncFlatSpec with Matchers:
  this: ToFuture[F] =>

  protected def withRecordedRequests(headers: Seq[Header])(use: ClientTransport[F] => F[Unit]): Future[List[GenericRequest[?, ?]]]

  protected def expectedMethods: Seq[Method] = List(Method.POST, Method.DELETE)

  protected val mcpUri: Uri = uri"http://localhost/mcp"

  protected val responseBody: String =
    (JSONRPCMessage.Response(id = RequestId(1), result = Json.obj()): JSONRPCMessage).asJson.noSpaces

  private val request: JSONRPCMessage = JSONRPCMessage.Request(method = "tools/list", params = None, id = RequestId(1))

  private def sendOne(transport: ClientTransport[F]): F[Unit] = monad.map(transport.send(request))(_ => ())

  "an HTTP transport" should "send the given headers with every request" in:
    withRecordedRequests(List(Header.authorization("Bearer", "t-1")))(sendOne).map: sent =>
      sent.map(_.method).distinct should contain allElementsOf expectedMethods
      all(sent.map(_.header("Authorization"))) shouldBe Some("Bearer t-1")

  it should "let a given header replace one that the transport sets itself" in:
    withRecordedRequests(List(Header("Accept", "application/json")))(sendOne).map: sent =>
      sent should not be empty
      all(sent.map(_.headers.filter(_.is("Accept")))) shouldBe List(Header("Accept", "application/json"))
