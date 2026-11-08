package histos.cli

import histos.{LawDir, Platform, ProjectName, ScalaVersion}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.*
import zio.test.*

object TestLogsLawSpec extends ZIOSpecDefault:
  def spec = suite("test log laws")(
    test("a second append keeps both lines under the cell path"):
      for
        root <- ZIO.service[Path]
        repo <- LawDir.child(root, "logs")
        _    <- TestLogs.append(repo, ProjectName("api"), ScalaVersion("3.3.4"), Platform.Jvm, "one")
        _    <- TestLogs.append(repo, ProjectName("api"), ScalaVersion("3.3.4"), Platform.Jvm, "two")
        path = TestLogs.file(repo, ProjectName("api"), ScalaVersion("3.3.4"), Platform.Jvm)
        text <- ZIO.attemptBlocking(Files.readString(path, StandardCharsets.UTF_8)).orDie
      yield assertTrue(
        path == repo.resolve(".histos").resolve("tests").resolve("api").resolve("3.3.4").resolve("jvm.log"),
        text == "one\ntwo\n",
      )
  ).provideSomeLayerShared[TestEnvironment & Scope](LawDir.layer)
