package histos.cli

import histos.workspace.Workspace
import histos.zinc.{CompileError, GraphCompileError}
import histos.{Platform, ProjectName, ScalaVersion}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.ZIO
import zio.test.{TestSystem, ZIOSpecDefault, assertTrue, suite, test}

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
      val name = ProjectName.trusted("gaplaw")
      val one  = GraphCompileError.Failed(name, ScalaVersion("3.10.0"), Platform.Jvm, CompileError.NoSources)
      val root = GraphCompileError.NoRoot(name)
      val single = GraphCompileError.Cells(zio.NonEmptyChunk(root))
      val many   = GraphCompileError.Cells(zio.NonEmptyChunk(root, GraphCompileError.NoRoot(ProjectName.trusted("other"))))
      assertTrue(
        Cli.compileFailure(one) == "'gaplaw' 3.10.0 failed to compile",
        Cli.compileFailure(single) == "1 cell failed to compile",
        Cli.compileFailure(many) == "2 cells failed to compile",
        Cli.compileFailure(root) == root.message,
      )
    ,
    test("a repo that is not a git checkout treats the diff as the whole suite"):
      stand("histos-cli-gap-absent").flatMap: dir =>
        Cli.launch(List("affected", "--base", "HEAD"), dir).map: done =>
          assertTrue(done.exit == CliExit.Success, done.err == "", done.out == "[\"all\"]\n")
    ,
    test("an empty diff against HEAD selects nothing"):
      stand("histos-cli-gap-empty").flatMap: dir =>
        for
          _    <- git(dir, "init")
          _    <- ZIO.attempt(Files.writeString(dir.resolve("histos.scala"), build)).orDie
          _    <- git(dir, "add", "histos.scala")
          _    <- git(dir, "-c", "user.email=law@example.test", "-c", "user.name=law", "commit", "-m", "law")
          done <- Cli.launch(List("affected", "--base", "HEAD"), dir)
        yield assertTrue(done.exit == CliExit.Success, done.err == "", done.out == "[]\n")
  )

  private def stand(prefix: String) =
    val jdk  = java.lang.System.getProperty("java.home")
    val os   = java.lang.System.getProperty("os.name")
    val arch = java.lang.System.getProperty("os.arch")
    for
      dir <- Workspace.work(prefix)
      _   <- TestSystem.putProperty("java.home", jdk)
      _   <- TestSystem.putProperty("os.name", os)
      _   <- TestSystem.putProperty("os.arch", arch)
      _   <- TestSystem.putProperty("user.home", dir.toString)
      _   <- TestSystem.putEnv("XDG_CACHE_HOME", "")
      _   <- Workspace.writeString(dir.resolve("histos.scala"), build)
    yield dir

  private def git(dir: Path, args: String*): ZIO[Any, Nothing, Unit] =
    ZIO
      .attemptBlocking:
        val process = ProcessBuilder(("git" :: args.toList)*).directory(dir.toFile).redirectErrorStream(true).start()
        val text    = String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
        process.waitFor() -> text
      .orDie
      .flatMap: (code, text) =>
        ZIO.when(code != 0)(ZIO.dieMessage(text)).unit
end CliGapLawSpec
