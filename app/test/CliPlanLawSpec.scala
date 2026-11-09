package histos.cli

import histos.workspace.Workspace

import java.nio.file.Files

import zio.ZIO
import zio.test.{TestSystem, ZIOSpecDefault, assertTrue, suite, test}

/** `new --bare` writes a project and does not write CI. `preview compile` names sources that have never been compiled.
  */
object CliPlanLawSpec extends ZIOSpecDefault:
  private val version = "9.9.9-test"

  private val build =
    """import histos.*
      |
      |object build extends HistosBuild:
      |  def graph =
      |    val previewlaw = module("previewlaw", Jvm)(scala"3.10.0")
      |    Projects(previewlaw)
      |""".stripMargin

  def spec = suite("cli plan laws")(
    test("bare new prints the project and does not write a launcher or a lock"):
      Workspace
        .work("histos-cli-new")
        .flatMap: dir =>
          val created = dir.resolve("barelaw").toAbsolutePath.normalize
          for
            _         <- standIn(dir)
            made      <- Cli.launch(List("new", "--bare", "barelaw"), dir)
            props     <- ZIO.attempt(Files.readString(created.resolve("build.properties"))).orDie
            buildFile <- ZIO.attempt(Files.isRegularFile(created.resolve("histos.scala"))).orDie
            script    <- ZIO.attempt(Files.exists(created.resolve("histos"))).orDie
            lock      <- ZIO.attempt(Files.exists(created.resolve("dependencies.lock"))).orDie
          yield assertTrue(
            made.exit == CliExit.Success,
            made.err == "",
            made.out == s"$created\nhistos test",
            props == s"histos.version=$version\n",
            buildFile,
            !script,
            !lock,
          )
    ,
    test("preview compile names an uncompiled source"):
      Workspace
        .work("histos-cli-preview")
        .flatMap: dir =>
          val source = dir.resolve("previewlaw").resolve("src").resolve("main").resolve("scala").resolve("Ready.scala")
          for
            _ <- standIn(dir)
            _ <- Workspace.createDirectories(parentOf(source)).catchAll(error => ZIO.dieMessage(error.message))
            _ <- Workspace
              .writeString(dir.resolve("histos.scala"), build)
              .catchAll(error => ZIO.dieMessage(error.message))
            _ <- Workspace
              .writeString(source, "object Ready:\n  def n: Int = 1\n")
              .catchAll(error => ZIO.dieMessage(error.message))
            preview <- Cli.launch(List("preview", "compile"), dir)
          yield assertTrue(
            preview.exit == CliExit.Success,
            preview.err == "",
            preview.out ==
              """previewlaw 3.10.0 jvm selected
              |upstreams
              |sources previewlaw/src/main/scala/Ready.scala
              |no previous compile
              |stale previewlaw/src/main/scala/Ready.scala""".stripMargin,
          ),
  )

  private def parentOf(path: java.nio.file.Path): java.nio.file.Path =
    Option(path.getParent).getOrElse(path)

  private def standIn(dir: java.nio.file.Path) =
    for
      _ <- TestSystem.putProperty("java.home", java.lang.System.getProperty("java.home"))
      _ <- TestSystem.putProperty("os.name", java.lang.System.getProperty("os.name"))
      _ <- TestSystem.putProperty("os.arch", java.lang.System.getProperty("os.arch"))
      _ <- TestSystem.putProperty("user.home", dir.toString)
      _ <- TestSystem.putEnv("XDG_CACHE_HOME", "")
      _ <- TestSystem.putEnv("HISTOS_VERSION", version)
    yield ()
end CliPlanLawSpec
