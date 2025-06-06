import com.softwaremill.SbtSoftwareMillCommon.commonSmlBuildSettings

// Version constants
val scalaTestV = "3.2.18"
val circeV = "0.14.6"
val tapirV = "1.11.33"

lazy val commonSettings = commonSmlBuildSettings ++ Seq(
  organization := "com.softwaremill",
  scalaVersion := "3.3.6"
)

val scalaTest = "org.scalatest" %% "scalatest" % scalaTestV % Test

lazy val rootProject = (project in file("."))
  .settings(commonSettings: _*)
  .settings(publishArtifact := false, name := "root")
  .aggregate(core)

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
      "com.softwaremill.sttp.tapir" %% "tapir-netty-server-sync" % tapirV,
      "com.softwaremill.sttp.tapir" %% "tapir-apispec-docs" % tapirV,
      "com.softwaremill.sttp.apispec" %% "jsonschema-circe" % "0.11.9",
      "ch.qos.logback" % "logback-classic" % "1.4.14",
      "org.slf4j" % "slf4j-api" % "2.0.13"
    )
  )
