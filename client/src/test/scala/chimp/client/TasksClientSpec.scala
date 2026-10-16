package chimp.client

import chimp.client.transport.ClientHttpTransport
import chimp.protocol.*
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.client4.testing.SyncBackendStub
import sttp.client4.{GenericRequest, StringBody}
import sttp.model.StatusCode
import sttp.shared.Identity

class TasksClientSpec extends AnyFlatSpec with Matchers:

  private val mcpUri = sttp.model.Uri.parse("http://localhost/mcp").toOption.get
  private val clientInfo = Implementation(name = "chimp-test", version = "0.0.1")

  private def envelopeFor(method: String, request: GenericRequest[?, ?]): Boolean =
    request.body match
      case StringBody(s, _, _) => s.contains(s"\"$method\"")
      case _                   => false

  private val initEnvelope: String =
    val initResult = InitializeResult(
      protocolVersion = ProtocolVersion.Latest.name,
      capabilities = ServerCapabilities(),
      serverInfo = Implementation(name = "test-server", version = "1.0")
    )
    (JSONRPCMessage.Response(id = RequestId(1), result = initResult.asJson): JSONRPCMessage).asJson.noSpaces

  private def client(backend: sttp.client4.testing.SyncBackendStub): McpClient[Identity] =
    McpClient[Identity](ClientHttpTransport[Identity](backend, mcpUri), clientInfo, ProtocolVersion.Latest)

  it should "poll a task with tasks/get and expose the underlying result" in:
    val task = GetTaskResult(
      taskId = TaskId("t1"),
      outcome = TaskOutcome.Completed(CallToolResult(content = List(ToolContent.Text(text = "done"))).asJson),
      resultType = Some("complete")
    )
    val taskEnvelope = (JSONRPCMessage.Response(id = RequestId(1), result = task.asJson): JSONRPCMessage).asJson.noSpaces
    val backend = SyncBackendStub
      .whenRequestMatches(envelopeFor("initialize", _))
      .thenRespondAdjust(initEnvelope)
      .whenRequestMatches(envelopeFor("tasks/get", _))
      .thenRespondAdjust(taskEnvelope)
      .whenAnyRequest
      .thenRespondAdjust("", StatusCode.Accepted)

    val res = client(backend).getTask(TaskId("t1"))
    res.status shouldBe TaskStatus.Completed
    res.outcome match
      case TaskOutcome.Completed(result) =>
        result.as[CallToolResult].toOption.map(_.content.head) shouldBe Some(ToolContent.Text("text", "done"))
      case other => fail(s"expected Completed, got $other")

  it should "cancel a task with tasks/cancel" in:
    val ack = TaskAck(taskId = Some(TaskId("t1")), status = Some(TaskStatus.Cancelled))
    val ackEnvelope = (JSONRPCMessage.Response(id = RequestId(1), result = ack.asJson): JSONRPCMessage).asJson.noSpaces
    val backend = SyncBackendStub
      .whenRequestMatches(envelopeFor("initialize", _))
      .thenRespondAdjust(initEnvelope)
      .whenRequestMatches(envelopeFor("tasks/cancel", _))
      .thenRespondAdjust(ackEnvelope)
      .whenAnyRequest
      .thenRespondAdjust("", StatusCode.Accepted)

    noException should be thrownBy client(backend).cancelTask(TaskId("t1"))

  it should "declare task support and parse a task handle from callToolWithTasks" in:
    val created = CreateTaskResult(taskId = TaskId("t9"), status = TaskStatus.Working)
    val createdEnvelope = (JSONRPCMessage.Response(id = RequestId(1), result = created.asJson): JSONRPCMessage).asJson.noSpaces
    val initResult = InitializeResult(
      protocolVersion = ProtocolVersion.Latest.name,
      capabilities = ServerCapabilities(tools = Some(ServerToolsCapability())),
      serverInfo = Implementation(name = "s", version = "1")
    )
    val toolsInitEnvelope = (JSONRPCMessage.Response(id = RequestId(1), result = initResult.asJson): JSONRPCMessage).asJson.noSpaces
    val backend = SyncBackendStub
      .whenRequestMatches(envelopeFor("initialize", _))
      .thenRespondAdjust(toolsInitEnvelope)
      .whenRequestMatches(req => envelopeFor("tools/call", req) && envelopeFor(TasksExtension.Id, req))
      .thenRespondAdjust(createdEnvelope)
      .whenAnyRequest
      .thenRespondAdjust("", StatusCode.Accepted)

    client(backend).callToolWithTasks("slow", io.circe.Json.obj()) shouldBe ToolCallResponse.Deferred(created)
