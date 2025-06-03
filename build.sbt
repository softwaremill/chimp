ThisBuild / scalaVersion := "3.3.1"
ThisBuild / organization := "com.chimp"

lazy val root = (project in file("."))
  .settings(
    name := "chimp",
    version := "0.1.0-SNAPSHOT",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )
