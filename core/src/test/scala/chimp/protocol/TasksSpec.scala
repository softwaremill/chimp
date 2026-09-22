package chimp.protocol

import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import scala.concurrent.duration.*

class TasksSpec extends AnyFlatSpec with Matchers:

  it should "decode a CreateTaskResult from the spec example" in:
    val json =
      """
        {
          "resultType": "task",
          "taskId": "786512e2-9e0d-44bd-8f29-789f320fe840",
          "status": "working",
          "createdAt": "2025-11-25T10:30:00Z",
          "lastUpdatedAt": "2025-11-25T10:50:00Z",
          "ttlMs": 3600000,
          "pollIntervalMs": 5000
        }
      """
    val res = decode[CreateTaskResult](json)
    res.map(_.taskId) shouldBe Right(TaskId("786512e2-9e0d-44bd-8f29-789f320fe840"))
    res.map(_.status) shouldBe Right(TaskStatus.Working)
    res.map(_.createdAt) shouldBe Right(Some(Instant.parse("2025-11-25T10:30:00Z")))
    res.map(_.ttlMs) shouldBe Right(Some(1.hour))
    res.map(_.pollIntervalMs) shouldBe Right(Some(5.seconds))

  it should "encode durations as integer milliseconds on the wire" in:
    val created =
      CreateTaskResult(
        taskId = TaskId("t"),
        status = TaskStatus.Working,
        ttlMs = Some(1.hour),
        pollIntervalMs = Some(5.seconds)
      )
    val json = created.asJson
    json.hcursor.downField("ttlMs").as[Long] shouldBe Right(3600000L)
    json.hcursor.downField("pollIntervalMs").as[Long] shouldBe Right(5000L)

  it should "encode and decode task status with the spec wire strings" in:
    (TaskStatus.InputRequired: TaskStatus).asJson shouldBe Json.fromString("input_required")
    (TaskStatus.Cancelled: TaskStatus).asJson shouldBe Json.fromString("cancelled")
    decode[TaskStatus](""" "completed" """.trim) shouldBe Right(TaskStatus.Completed)

  it should "reject an unknown task status" in:
    decode[TaskStatus](""" "bogus" """.trim).isLeft shouldBe true

  it should "round-trip a completed GetTaskResult carrying the tool result" in:
    val toolResult = CallToolResult(content = List(ToolContent.Text(text = "Hello, Luca!"))).asJson
    val task = GetTaskResult(
      taskId = TaskId("t1"),
      outcome = TaskOutcome.Completed(toolResult),
      resultType = Some("complete")
    )
    decode[GetTaskResult](task.asJson.noSpaces) shouldBe Right(task)

  it should "flatten the outcome onto the wire and reject a completed task without a result" in:
    val completed = GetTaskResult(taskId = TaskId("t1"), outcome = TaskOutcome.Completed(Json.obj("k" -> Json.fromInt(1))))
    completed.asJson.hcursor.downField("status").as[String] shouldBe Right("completed")
    completed.asJson.hcursor.downField("result").as[Json] shouldBe Right(Json.obj("k" -> Json.fromInt(1)))
    decode[GetTaskResult]("""{ "taskId": "t1", "status": "completed" }""").isLeft shouldBe true

  it should "mark only completed, failed and cancelled as terminal" in:
    TaskStatus.values.filter(TaskStatus.isTerminal).toSet shouldBe
      Set(TaskStatus.Completed, TaskStatus.Failed, TaskStatus.Cancelled)
