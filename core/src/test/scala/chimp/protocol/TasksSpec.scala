package chimp.protocol

import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{Duration, Instant}

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
    res.map(_.ttl) shouldBe Right(Some(Duration.ofHours(1)))
    res.map(_.pollInterval) shouldBe Right(Some(Duration.ofSeconds(5)))

  it should "encode durations as ISO-8601 strings on the wire" in:
    val created =
      CreateTaskResult(
        taskId = TaskId("t"),
        status = TaskStatus.Working,
        ttl = Some(Duration.ofHours(1)),
        pollInterval = Some(Duration.ofSeconds(5))
      )
    val json = created.asJson
    json.hcursor.downField("ttlMs").as[String] shouldBe Right("PT1H")
    json.hcursor.downField("pollIntervalMs").as[String] shouldBe Right("PT5S")

  it should "decode durations from an ISO-8601 string" in:
    val json = """{ "resultType": "task", "taskId": "t", "status": "working", "ttlMs": "PT2H", "pollIntervalMs": "PT10S" }"""
    val res = decode[CreateTaskResult](json)
    res.map(_.ttl) shouldBe Right(Some(Duration.ofHours(2)))
    res.map(_.pollInterval) shouldBe Right(Some(Duration.ofSeconds(10)))

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
      status = TaskStatus.Completed,
      result = Some(toolResult),
      resultType = Some("complete")
    )
    decode[GetTaskResult](task.asJson.noSpaces) shouldBe Right(task)

  it should "mark only completed, failed and cancelled as terminal" in:
    TaskStatus.values.filter(TaskStatus.isTerminal).toSet shouldBe
      Set(TaskStatus.Completed, TaskStatus.Failed, TaskStatus.Cancelled)
