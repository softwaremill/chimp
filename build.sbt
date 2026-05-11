import com.softwaremill.SbtSoftwareMillCommon.commonSmlBuildSettings
import com.softwaremill.Publish.{ossPublishSettings, updateDocs}
import com.softwaremill.UpdateVersionInDocs

// Version constants
val scalaTestV = "3.2.20"
val circeV = "0.14.15"
val tapirV = "1.13.19"
val sttpClient4V = "4.0.23"

lazy val verifyExamplesCompileUsingScalaCli = taskKey[Unit]("Verify that each example compiles using Scala CLI")

lazy val commonSettings = commonSmlBuildSettings ++ ossPublishSettings ++ Seq(
  organization := "com.softwaremill.chimp",
  scalaVersion := "3.3.7",
  updateDocs := Def.taskDyn {
    val files = UpdateVersionInDocs(sLog.value, organization.value, version.value)
    Def.task {
      files
    }
  }.value,
  Test / scalacOptions += "-Wconf:msg=unused value of type org.scalatest.Assertion:s",
  Test / scalacOptions += "-Wconf:msg=unused value of type org.scalatest.compatible.Assertion:s"
)

val scalaTest = "org.scalatest" %% "scalatest" % scalaTestV % Test

lazy val rootProject = (project in file("."))
  .settings(commonSettings: _*)
  .settings(publishArtifact := false, name := "chimp")
  .aggregate(core, server, client, examples)

lazy val core: Project = (project in file("core"))
  .settings(commonSettings: _*)
  .settings(
    name := "chimp-core",
    libraryDependencies ++= Seq(
      scalaTest,
      "io.circe" %% "circe-core" % circeV,
      "io.circe" %% "circe-generic" % circeV,
      "io.circe" %% "circe-parser" % circeV,
      "org.slf4j" % "slf4j-api" % "2.0.17"
    )
  )

lazy val server: Project = (project in file("server"))
  .settings(commonSettings: _*)
  .settings(
    name := "chimp-server",
    libraryDependencies ++= Seq(
      scalaTest,
      "com.softwaremill.sttp.tapir" %% "tapir-core" % tapirV,
      "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % tapirV,
      "com.softwaremill.sttp.tapir" %% "tapir-apispec-docs" % tapirV,
      "com.softwaremill.sttp.apispec" %% "jsonschema-circe" % "0.11.10"
    )
  )
  .dependsOn(core)

lazy val client: Project = (project in file("client"))
  .settings(commonSettings: _*)
  .settings(
    name := "chimp-client",
    libraryDependencies ++= Seq(
      scalaTest,
      "com.softwaremill.sttp.client4" %% "core" % sttpClient4V
    )
  )
  .dependsOn(core)

lazy val examples = (project in file("examples"))
  .settings(commonSettings: _*)
  .settings(
    publishArtifact := false,
    name := "examples",
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.client4" %% "core" % "4.0.23",
      "com.softwaremill.sttp.tapir" %% "tapir-netty-server-sync" % tapirV,
      "com.softwaremill.sttp.tapir" %% "tapir-zio-http-server" % tapirV,
      "ch.qos.logback" % "logback-classic" % "1.5.32"
    ),
    verifyExamplesCompileUsingScalaCli := VerifyExamplesCompileUsingScalaCli(sLog.value, sourceDirectory.value)
  )
  .dependsOn(server)
