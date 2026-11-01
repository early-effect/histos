package histos.cli

import histos.clean.Products
import histos.{Jvm, ProjectName, ScalaVersion}

import java.nio.file.Files

import zio.ZIO
import zio.test.*

object PaintSpec extends ZIOSpecDefault:
  private val passed  = "api 3.10.0 jvm passed"
  private val failed  = "api 3.10.0 jvm failed 1"
  private val skipped = "api 3.10.0 jvm skipped unchanged"
  private val error   = "api 3.10.0 jvm Core.scala:1:1: error: missing"
  private val warn    = "api 3.10.0 jvm Core.scala:1:1: warn: unused"
  private val total   = "tests passed: 1 failed: 0 cached: 0 modules: 1"

  def spec = suite("console paint and test logs")(
    test("status words are colored on a terminal and plain when color is off"):
      assertTrue(
        Paint.paint(passed, tty = true, noColor = None).contains("\u001b[32mpassed"),
        Paint.paint(failed, tty = true, noColor = None).contains("\u001b[31mfailed"),
        Paint.paint(skipped, tty = true, noColor = None).contains("\u001b[2mskipped unchanged"),
        Paint.paint(error, tty = true, noColor = None).contains("\u001b[31merror"),
        Paint.paint(warn, tty = true, noColor = None).contains("\u001b[33mwarn"),
        Paint.paint(total, tty = true, noColor = None).startsWith("\u001b[1m"),
        Paint.paint(passed, tty = true, noColor = Some("")) == passed,
        Paint.paint(passed, tty = false, noColor = None) == passed,
        Lines.suites(1, 0, 2, 1).contains("tests passed: 1 failed: 0 cached: 2 modules: 1"),
        Lines.suites(0, 0, 0, 0).isEmpty,
      )
    ,
    test("a test log keeps the plain line and clean removes it"):
      for
        root  <- temp
        _     <- TestLogs.append(root, ProjectName("api"), ScalaVersion("3.10.0"), Jvm, passed)
        text  <- ZIO.attempt(Files.readString(TestLogs.file(root, ProjectName("api"), ScalaVersion("3.10.0"), Jvm)))
        _     <- TestLogs.append(root, ProjectName("api"), ScalaVersion("3.10.0"), Jvm, "api 3.10.0 jvm")
        both  <- ZIO.attempt(Files.readString(TestLogs.file(root, ProjectName("api"), ScalaVersion("3.10.0"), Jvm)))
        wiped <- Products.wipe(root).either
        gone  <- ZIO.attempt(!Files.exists(root.resolve(".histos").resolve("tests")))
      yield assertTrue(
        text == passed + "\n",
        both == passed + "\n" + "api 3.10.0 jvm\n",
        wiped.isRight,
        gone,
      ),
  )

  private def temp =
    histos.workspace.Workspace
      .work("histos-paint")
      .mapError(error => new java.io.IOException(error.message))
end PaintSpec
