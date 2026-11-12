package histos.cli

import histos.central.ReleaseFeed
import histos.resolve.Resolve
import histos.workspace.Workspace
import histos.zinc.{CompileError, GraphCompileError}
import histos.{LawGit, LawMachine, Platform, ProjectName, ScalaVersion}
import slayer.stubbed

import zio.Scope
import zio.test.{TestEnvironment, TestSystem, ZIOSpecDefault, assertTrue, suite, test}

/** A failed diff is the whole suite. An empty diff on HEAD is nothing. A cell count is not the compiler's own text. */
object CliGapLawSpec extends ZIOSpecDefault:
  private val build =
    """import histos.*
      |
      |object build extends HistosBuild:
      |  def graph =
      |    val gaplaw = module("gaplaw", Jvm)(scala"3.10.0")
      |    Projects(gaplaw)
      |""".stripMargin

  def spec = suite("cli gap laws")(
    test("one failed cell, many failed cells, and any other compile error keep their own text"):
      val name   = ProjectName.trusted("gaplaw")
      val one    = GraphCompileError.Failed(name, ScalaVersion("3.10.0"), Platform.Jvm, CompileError.NoSources)
      val root   = GraphCompileError.NoRoot(name)
      val single = GraphCompileError.Cells(zio.NonEmptyChunk(root))
      val many   =
        GraphCompileError.Cells(zio.NonEmptyChunk(root, GraphCompileError.NoRoot(ProjectName.trusted("other"))))
      assertTrue(
        Cli.compileFailure(one) == "'gaplaw' 3.10.0 failed to compile",
        Cli.compileFailure(single) == "1 cell failed to compile",
        Cli.compileFailure(many) == "2 cells failed to compile",
        Cli.compileFailure(root) == root.message,
      )
    ,
    test("a repo that is not a git checkout treats the diff as the whole suite"):
      for
        dir  <- stand("histos-cli-gap-absent")
        done <- Cli.launch(List("affected", "--base", "HEAD"), dir)
      yield assertTrue(done.exit == CliExit.Success, done.err == "", done.out == "[\"all\"]\n")
    ,
    test("an empty diff against HEAD selects nothing"):
      for
        dir  <- stand("histos-cli-gap-empty")
        _    <- LawGit.commit(dir, "main", "histos.scala")
        done <- Cli.launch(List("affected", "--base", "HEAD"), dir)
      yield assertTrue(done.exit == CliExit.Success, done.err == "", done.out == "[]\n"),
  ).provideSomeLayer[TestEnvironment & Scope](stubbed[ReleaseFeed] ++ stubbed[Resolve] ++ LawMachine.redirected)

  private def stand(prefix: String) =
    for
      dir <- Workspace.work(prefix)
      _   <- LawMachine.system(dir)
      _   <- TestSystem.putEnv("XDG_CACHE_HOME", "")
      _   <- Workspace.writeString(dir.resolve("histos.scala"), build)
    yield dir
end CliGapLawSpec
