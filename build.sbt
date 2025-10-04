import com.softwaremill.SbtSoftwareMillCommon.commonSmlBuildSettings
import com.softwaremill.Publish.{ossPublishSettings, updateDocs}
import com.softwaremill.UpdateVersionInDocs

// Version constants
val scalaTestV = "3.2.19"
val circeV = "0.14.15"
val tapirV = "1.11.47"

lazy val commonSettings = commonSmlBuildSettings ++ ossPublishSettings ++ Seq(
  organization := "com.softwaremill.chimp",
  scalaVersion := "3.3.6",
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
  .aggregate(core, examples)

lazy val core: Project = (project in file("core"))
  .settings(commonSettings: _*)
  .settings(
    name := "core",
    libraryDependencies ++= Seq(
      scalaTest,
      "io.circe" %% "circe-core" % circeV,
      "io.circe" %% "circe-generic" % circeV,
      "io.circe" %% "circe-parser" % circeV,
      "com.softwaremill.sttp.tapir" %% "tapir-core" % tapirV,
      "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % tapirV,
      "com.softwaremill.sttp.tapir" %% "tapir-apispec-docs" % tapirV,
      "com.softwaremill.sttp.tapir" %% "tapir-zio-http-server" % tapirV,
      "com.softwaremill.sttp.apispec" %% "jsonschema-circe" % "0.11.10",
      "org.slf4j" % "slf4j-api" % "2.0.17"
    )
  )

lazy val examples = (project in file("examples"))
  .settings(commonSettings: _*)
  .settings(
    publishArtifact := false,
    name := "examples",
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.client4" %% "core" % "4.0.12",
      "com.softwaremill.sttp.tapir" %% "tapir-netty-server-sync" % tapirV,
      "ch.qos.logback" % "logback-classic" % "1.5.19"
    )
  )
  .dependsOn(core)
