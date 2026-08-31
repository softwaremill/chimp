package chimp.server

import chimp.protocol.*
import chimp.protocol.JSONRPCMessage.given
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.monad.{IdentityMonad, MonadError}
import sttp.shared.Identity
import sttp.tapir.Schema

class TaskServerSpec extends AnyFlatSpec with Matchers:
  import JSONRPCMessage.*

  private given MonadError[Identity] = IdentityMonad

  case class TIn(message: String) derives Schema, io.circe.Codec

  private val instant = tool("instant").input[TIn].handle(in => ToolResult.text(s"echo:${in.message}"))
  private val slow = tool("slow")
    .input[TIn]
    .handle: in =>
      Thread.sleep(150)
      ToolResult.text(s"slow:${in.message}")
  private val boom = tool("boom").input[TIn].handle(_ => throw new RuntimeException("boom"))
  private val forever = tool("forever")
    .input[TIn]
    .handle: _ =>
      Thread.sleep(10000)
      ToolResult.text("done")

  private val askThenEcho = tool("askThenEcho")
    .input[TIn]
    .taskLogic[Identity]: (in, ctx, _) =>
      val answer = ctx.requestInput("q1", Json.fromString(s"need input for ${in.message}"))
      ToolResult.text(s"got:${answer.noSpaces}")

  private def handlerWith(requireTask: String => Boolean = _ => false): McpHandler[Identity, ServerContext[Identity]] =
    val support = TaskSupport[Identity](
      store = TaskStore.inMemory[Identity],
      executor = TaskExecutor.threadPool(),
      requireTask = requireTask
    )
    McpHandler(McpServer(tools = List(instant, slow, boom, forever)).withTasks(support).addTaskTool(askThenEcho))

  private def resultJson(response: McpResponse): Json =
    val json = response match
      case McpResponse.JsonResponse(j)     => j
      case McpResponse.EmptyAcceptResponse => fail("expected JsonResponse")
    json.as[JSONRPCMessage].getOrElse(fail("decode JSONRPCMessage")) match
      case Response(_, _, result) => result
      case other                  => fail(s"expected Response, got $other")

  private def errorObj(response: McpResponse): JSONRPCErrorObject =
    val json = response match
      case McpResponse.JsonResponse(j)     => j
      case McpResponse.EmptyAcceptResponse => fail("expected JsonResponse")
    json.as[JSONRPCMessage].getOrElse(fail("decode JSONRPCMessage")) match
      case Error(_, _, err) => err
      case other            => fail(s"expected Error, got $other")

  private def callToolReq(name: String, withTasks: Boolean): Json =
    val meta = Option.when(withTasks)(TasksExtension.clientCapabilityMeta)
    val params = CallToolParams(name = name, arguments = TIn("hi").asJson, _meta = meta).asJson
    (Request(method = "tools/call", params = Some(params), id = RequestId("call")): JSONRPCMessage).asJson

  private def pollUntil(handler: McpHandler[Identity, ServerContext[Identity]], taskId: TaskId)(
      predicate: GetTaskResult => Boolean
  ): GetTaskResult =
    var last = GetTaskResult(taskId = taskId, outcome = TaskOutcome.Working)
    var done = false
    var i = 0
    while !done && i < 200 do
      val req = (Request(method = "tasks/get", params = Some(GetTaskParams(taskId).asJson), id = RequestId("get")): JSONRPCMessage).asJson
      last = resultJson(handler.handleJsonRpc(req, Seq.empty)).as[GetTaskResult].getOrElse(fail("decode GetTaskResult"))
      if predicate(last) then done = true
      else
        Thread.sleep(20)
        i += 1
    last

  private def pollTask(handler: McpHandler[Identity, ServerContext[Identity]], taskId: TaskId): GetTaskResult =
    pollUntil(handler, taskId)(task => TaskStatus.isTerminal(task.status))

  "a task-enabled server" should "run tools/call synchronously when the client does not declare task support" in:
    val handler = handlerWith()
    val result = resultJson(handler.handleJsonRpc(callToolReq("instant", withTasks = false), Seq.empty))
    val call = result.as[CallToolResult].getOrElse(fail("decode CallToolResult"))
    call.content.head shouldBe ToolContent.Text("text", "echo:hi")

  it should "answer with a task and complete it when the client declares support" in:
    val handler = handlerWith()
    val created = resultJson(handler.handleJsonRpc(callToolReq("slow", withTasks = true), Seq.empty))
      .as[CreateTaskResult]
      .getOrElse(fail("decode CreateTaskResult"))
    created.status shouldBe TaskStatus.Working
    created.taskId.value should not be empty

    val finished = pollTask(handler, created.taskId)
    finished.outcome match
      case TaskOutcome.Completed(result) =>
        result.as[CallToolResult].toOption.map(_.content.head) shouldBe Some(ToolContent.Text("text", "slow:hi"))
      case other => fail(s"expected Completed, got $other")

  it should "report a failed task when the tool throws" in:
    val handler = handlerWith()
    val created = resultJson(handler.handleJsonRpc(callToolReq("boom", withTasks = true), Seq.empty))
      .as[CreateTaskResult]
      .getOrElse(fail("decode CreateTaskResult"))

    val finished = pollTask(handler, created.taskId)
    finished.outcome match
      case TaskOutcome.Failed(_) => succeed
      case other                 => fail(s"expected Failed, got $other")

  it should "cancel a running task" in:
    val handler = handlerWith()
    val created = resultJson(handler.handleJsonRpc(callToolReq("forever", withTasks = true), Seq.empty))
      .as[CreateTaskResult]
      .getOrElse(fail("decode CreateTaskResult"))

    val cancelReq =
      (Request(method = "tasks/cancel", params = Some(CancelTaskParams(created.taskId).asJson), id = RequestId("c")): JSONRPCMessage).asJson
    val ack = resultJson(handler.handleJsonRpc(cancelReq, Seq.empty)).as[TaskAck].getOrElse(fail("decode TaskAck"))
    ack.status shouldBe Some(TaskStatus.Cancelled)

    pollTask(handler, created.taskId).status shouldBe TaskStatus.Cancelled

  it should "reject a required-task call that lacks the client capability with -32003" in:
    val handler = handlerWith(requireTask = _ == "slow")
    val err = errorObj(handler.handleJsonRpc(callToolReq("slow", withTasks = false), Seq.empty))
    err.code shouldBe JSONRPCErrorCodes.MissingRequiredClientCapability.code

  it should "advertise the tasks extension capability on initialize" in:
    val handler = handlerWith()
    val req = (Request(method = "initialize", id = RequestId("i")): JSONRPCMessage).asJson
    val result = resultJson(handler.handleJsonRpc(req, Seq.empty)).as[InitializeResult].getOrElse(fail("decode InitializeResult"))
    result.capabilities.extensions.map(_.keySet) shouldBe Some(Set(TasksExtension.Id))

  it should "run an input_required task tool: request input, answer via tasks/update, then complete" in:
    val handler = handlerWith()
    val created = resultJson(handler.handleJsonRpc(callToolReq("askThenEcho", withTasks = true), Seq.empty))
      .as[CreateTaskResult]
      .getOrElse(fail("decode CreateTaskResult"))
    created.status shouldBe TaskStatus.Working

    // the tool has requested input and parked, surfacing the request via tasks/get
    val waiting = pollUntil(handler, created.taskId)(_.status == TaskStatus.InputRequired)
    waiting.outcome match
      case TaskOutcome.InputRequired(requests) => requests.keySet shouldBe Set("q1")
      case other                               => fail(s"expected InputRequired, got $other")

    // answer it
    val updateReq = (Request(
      method = "tasks/update",
      params = Some(UpdateTaskParams(created.taskId, Map("q1" -> Json.fromString("42"))).asJson),
      id = RequestId("u")
    ): JSONRPCMessage).asJson
    val ack = resultJson(handler.handleJsonRpc(updateReq, Seq.empty)).as[TaskAck].getOrElse(fail("decode TaskAck"))
    ack.taskId shouldBe Some(created.taskId)

    // the tool resumes and completes with the answer folded in
    pollTask(handler, created.taskId).outcome match
      case TaskOutcome.Completed(result) =>
        result.as[CallToolResult].toOption.map(_.content.head) shouldBe Some(ToolContent.Text("text", "got:\"42\""))
      case other => fail(s"expected Completed, got $other")

  it should "reject the input_required tool when the client does not declare task support" in:
    val handler = handlerWith()
    val err = errorObj(handler.handleJsonRpc(callToolReq("askThenEcho", withTasks = false), Seq.empty))
    err.code shouldBe JSONRPCErrorCodes.MissingRequiredClientCapability.code
