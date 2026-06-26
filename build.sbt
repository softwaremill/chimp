import com.softwaremill.Publish.{ossPublishSettings, updateDocs}
import com.softwaremill.SbtSoftwareMillCommon.commonSmlBuildSettings
import com.softwaremill.UpdateVersionInDocs

val scalaTestV = "3.2.20"
val circeV = "0.14.15"
val slf4jV = "2.0.18"
val logbackV = "1.5.34"
val tapirV = "1.13.19"
val sttpClientV = "4.0.25"
val zioV = "2.1.26"
val zioProcessV = "0.8.0"
val zioHttpV = "3.8.0"
val oxV = "1.0.5"
val testcontainersScalaV = "0.41.8"

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
  Test / scalacOptions += "-Wconf:msg=unused value of type org.scalatest.compatible.Assertion:s",
  Test / test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-l", "Integration"),
  Test / parallelExecution := false,
  scalacOptions ++= Seq("-Wunused:all", "-Werror")
)

val scalaTest = "org.scalatest" %% "scalatest" % scalaTestV % Test

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(publishArtifact := false, name := "chimp")
  .aggregate(core, server, serverZio, serverOx, client, clientZio, clientOx, examples, serverConformance, clientConformance)

val conformance = inputKey[Unit]("Run the MCP conformance harness via npx, extra args are passed through")

lazy val core: Project = (project in file("core"))
  .settings(commonSettings: _*)
  .settings(
    name := "chimp-core",
    libraryDependencies ++= Seq(
      scalaTest,
      "io.circe" %% "circe-core" % circeV,
      "io.circe" %% "circe-generic" % circeV,
      "io.circe" %% "circe-parser" % circeV,
      "org.slf4j" % "slf4j-api" % slf4jV,
      "com.networknt" % "json-schema-validator" % "3.0.3" % Test
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
      "com.softwaremill.sttp.apispec" %% "jsonschema-circe" % "0.11.10",
      "com.softwaremill.sttp.tapir" %% "tapir-netty-server-sync" % tapirV % Test,
      "com.softwaremill.sttp.client4" %% "core" % sttpClientV % Test
    )
  )
  .dependsOn(core, client % "test->compile")

lazy val serverZio: Project = (project in file("server-streaming/server-zio"))
  .settings(commonSettings: _*)
  .settings(
    name := "chimp-server-zio",
    libraryDependencies ++= Seq(
      scalaTest,
      "dev.zio" %% "zio" % zioV,
      "dev.zio" %% "zio-streams" % zioV,
      "com.softwaremill.sttp.tapir" %% "tapir-zio" % tapirV,
      "com.softwaremill.sttp.tapir" %% "tapir-zio-http-server" % tapirV,
      "dev.zio" %% "zio-http" % zioHttpV
    )
  )
  .dependsOn(server % "compile->compile;test->test", clientZio % "test->compile")

lazy val serverOx: Project = (project in file("server-streaming/server-ox"))
  .settings(commonSettings: _*)
  .settings(
    name := "chimp-server-ox",
    libraryDependencies ++= Seq(
      scalaTest,
      "com.softwaremill.sttp.tapir" %% "tapir-netty-server-sync" % tapirV,
      "com.softwaremill.ox" %% "core" % oxV
    )
  )
  .dependsOn(server % "compile->compile;test->test", clientOx % "test->compile")

lazy val client: Project = (project in file("client"))
  .settings(commonSettings: _*)
  .settings(
    name := "chimp-client",
    libraryDependencies ++= Seq(
      scalaTest,
      "com.softwaremill.sttp.client4" %% "core" % sttpClientV,
      "com.dimafeng" %% "testcontainers-scala-scalatest" % testcontainersScalaV % Test,
      "ch.qos.logback" % "logback-classic" % logbackV % Test,
      "com.dimafeng" %% "testcontainers-scala-toxiproxy" % testcontainersScalaV % Test
    )
  )
  .dependsOn(core)

lazy val clientZio: Project = (project in file("client-streaming/client-zio"))
  .settings(commonSettings: _*)
  .settings(
    name := "chimp-client-zio",
    libraryDependencies ++= Seq(
      scalaTest,
      "dev.zio" %% "zio" % zioV,
      "dev.zio" %% "zio-streams" % zioV,
      "dev.zio" %% "zio-process" % zioProcessV,
      "com.softwaremill.sttp.client4" %% "zio" % sttpClientV
    )
  )
  .dependsOn(client % "compile->compile;test->test")

lazy val clientOx: Project = (project in file("client-streaming/client-ox"))
  .settings(commonSettings: _*)
  .settings(
    name := "chimp-client-ox",
    libraryDependencies ++= Seq(
      scalaTest,
      "com.softwaremill.sttp.client4" %% "core" % sttpClientV,
      "com.softwaremill.ox" %% "core" % oxV
    )
  )
  .dependsOn(client % "compile->compile;test->test")

lazy val examples = (project in file("examples"))
  .settings(commonSettings: _*)
  .settings(
    publishArtifact := false,
    name := "examples",
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.client4" %% "core" % sttpClientV,
      "com.softwaremill.sttp.tapir" %% "tapir-netty-server-sync" % tapirV,
      "com.softwaremill.sttp.tapir" %% "tapir-zio-http-server" % tapirV,
      "ch.qos.logback" % "logback-classic" % logbackV
    ),
    verifyExamplesCompileUsingScalaCli := VerifyExamplesCompileUsingScalaCli(sLog.value, sourceDirectory.value)
  )
  .dependsOn(server, serverOx, client, clientOx)

import sbtassembly.AssemblyPlugin.autoImport.*

lazy val assemblySettings = Seq(
  assembly / assemblyMergeStrategy := {
    case PathList("META-INF", "MANIFEST.MF")     => MergeStrategy.discard
    case PathList("META-INF", "INDEX.LIST")      => MergeStrategy.discard
    case PathList("META-INF", "DEPENDENCIES")    => MergeStrategy.discard
    case PathList("META-INF", "services", _ @_*) => MergeStrategy.concat
    case PathList("META-INF", xs @ _*) if xs.lastOption.exists(s => s.endsWith(".SF") || s.endsWith(".DSA") || s.endsWith(".RSA")) =>
      MergeStrategy.discard
    case PathList("META-INF", _ @_*)   => MergeStrategy.first
    case PathList("module-info.class") => MergeStrategy.discard
    case _                             => MergeStrategy.first
  }
)

lazy val serverConformance = (project in file("server-conformance"))
  .enablePlugins(AssemblyPlugin)
  .settings(commonSettings: _*)
  .settings(assemblySettings: _*)
  .settings(
    publishArtifact := false,
    name := "server-conformance",
    Compile / mainClass := Some("chimp.conformance.server.Main"),
    assembly / assemblyJarName := "chimp-server-conformance.jar",
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.tapir" %% "tapir-netty-server-sync" % tapirV,
      "ch.qos.logback" % "logback-classic" % logbackV
    ),
    conformance := {
      import complete.DefaultParsers.*

      import scala.sys.process.*
      val args = spaceDelimited("<args>").parsed.toList
      val jar = assembly.value
      val rootDir = (LocalRootProject / baseDirectory).value
      val baseline = (rootDir / "conformance-baseline.yml").getAbsolutePath
      val log = streams.value.log

      val urlPromise = scala.concurrent.Promise[String]()
      val pb = new java.lang.ProcessBuilder("java", "-jar", jar.getAbsolutePath).redirectErrorStream(false)
      val proc = pb.start()
      val readerThread = new Thread(() => {
        val reader = new java.io.BufferedReader(new java.io.InputStreamReader(proc.getInputStream, "UTF-8"))
        try {
          val line = reader.readLine()
          if (line != null && line.startsWith("http")) urlPromise.trySuccess(line.trim)
          else urlPromise.tryFailure(new RuntimeException(s"Server did not print a URL; first line was: $line"))
          var more: String = reader.readLine()
          while (more != null) {
            log.info(s"[server] $more")
            more = reader.readLine()
          }
        } catch {
          case t: Throwable => urlPromise.tryFailure(t)
        }
      })
      readerThread.setDaemon(true)
      readerThread.start()

      val errReaderThread = new Thread(() => {
        val reader = new java.io.BufferedReader(new java.io.InputStreamReader(proc.getErrorStream, "UTF-8"))
        try {
          var line: String = reader.readLine()
          while (line != null) {
            log.warn(s"[server-stderr] $line")
            line = reader.readLine()
          }
        } catch {
          case _: Throwable => ()
        }
      })
      errReaderThread.setDaemon(true)
      errReaderThread.start()

      try {
        val url = scala.concurrent.Await.result(urlPromise.future, scala.concurrent.duration.Duration("15s"))
        log.info(s"Server started at $url")
        val cmd = List("npx", "@modelcontextprotocol/conformance") ++ args ++
          List("--url", url, "--expected-failures", baseline)
        val rc = Process(cmd, rootDir).!
        if (rc != 0) sys.error(s"conformance harness exited with code $rc")
      } finally {
        proc.destroy()
        if (!proc.waitFor(2, java.util.concurrent.TimeUnit.SECONDS)) proc.destroyForcibly()
      }
    }
  )
  .dependsOn(server)

lazy val clientConformance = (project in file("client-conformance"))
  .enablePlugins(AssemblyPlugin)
  .settings(commonSettings: _*)
  .settings(assemblySettings: _*)
  .settings(
    publishArtifact := false,
    name := "client-conformance",
    Compile / mainClass := Some("chimp.conformance.client.Main"),
    assembly / assemblyJarName := "chimp-client-conformance.jar",
    libraryDependencies ++= Seq(
      "ch.qos.logback" % "logback-classic" % "1.5.34"
    ),
    conformance := {
      import complete.DefaultParsers.*

      import scala.sys.process.*
      val args = spaceDelimited("<args>").parsed.toList
      val _ = assembly.value
      val baseDir = baseDirectory.value
      val rootDir = (LocalRootProject / baseDirectory).value
      val wrapper = (baseDir / "bin" / "chimp-conformance-client").getAbsolutePath
      val cmd = List("npx", "@modelcontextprotocol/conformance") ++ args ++
        List("--command", wrapper, "--expected-failures", (rootDir / "conformance-baseline.yml").getAbsolutePath)
      val rc = Process(cmd, rootDir).!
      if (rc != 0) sys.error(s"conformance harness exited with code $rc")
    }
  )
  .dependsOn(client)

val compileDocs: TaskKey[Unit] = taskKey[Unit]("Compiles docs module throwing away its output")
compileDocs :=
  (docs / mdoc).toTask(" --out target/chimp-docs").value

lazy val docs: Project = (project in file("generated-docs"))
  .enablePlugins(MdocPlugin)
  .settings(commonSettings)
  .settings(
    mdocIn := file("docs"),
    moduleName := "chimp-docs",
    mdocVariables := Map(
      "VERSION" -> version.value
    ),
    mdocOut := file("generated-docs/out"),
    mdocExtraArguments := Seq("--clean-target", "--exclude", ".venv", "--exclude", "_build"),
    libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-netty-server-sync" % tapirV,
    publishArtifact := false,
    name := "docs"
  )
  .dependsOn(core, server, serverZio, serverOx, client, clientZio, clientOx)
