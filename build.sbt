import com.softwaremill.SbtSoftwareMillCommon.commonSmlBuildSettings

lazy val commonSettings = commonSmlBuildSettings ++ Seq(
  organization := "com.chimp",
  scalaVersion := "3.3.1"
)

val scalaTest = "org.scalatest" %% "scalatest" % "3.2.18" % Test

lazy val rootProject = (project in file("."))
  .settings(commonSettings: _*)
  .settings(publishArtifact := false, name := "root")
  .aggregate(core)

lazy val core: Project = (project in file("core"))
  .settings(commonSettings: _*)
  .settings(
    name := "core",
    libraryDependencies ++= Seq(
      scalaTest
    )
  )
