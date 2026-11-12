package histos.cli

import histos.LawMachine
import histos.central.ReleaseFeed
import histos.resolve.Resolve
import histos.workspace.Workspace
import slayer.stubbed

import java.nio.file.{Files, Path}

import zio.{Scope, ZIO}
import zio.test.{TestEnvironment, TestSystem, ZIOSpecDefault, assertTrue, suite, test}

/** `new --bare` writes a project and does not write CI. `preview compile` names sources that have never been compiled.
  * The release feed and `Resolve` are stubs: no law launches against Maven Central or a Coursier cache.
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
    test("bare new writes the build and the sources, and does not write a launcher, a lock, or CI"):
      Workspace
        .work("histos-cli-new")
        .flatMap: dir =>
          val created = dir.resolve("barelaw").toAbsolutePath.normalize
          val main    =
            created
              .resolve("app")
              .resolve("src")
              .resolve("main")
              .resolve("scala")
              .resolve("barelaw")
              .resolve("Main.scala")
          for
            _         <- standIn(dir)
            made      <- Cli.launch(List("new", "--bare", "barelaw"), dir)
            props     <- textAt(created.resolve("build.properties"))
            buildText <- textAt(created.resolve("histos.scala"))
            mainText  <- textAt(main)
            script    <- ZIO.attempt(Files.exists(created.resolve("histos"))).orDie
            lock      <- ZIO.attempt(Files.exists(created.resolve("dependencies.lock"))).orDie
            ci        <- ZIO.attempt(Files.exists(created.resolve(".github"))).orDie
          yield assertTrue(
            made.exit == CliExit.Success,
            made.err == "",
            made.out == s"$created\nhistos test",
            props == s"histos.version=$version\n",
            buildText.contains("module(\"app\", Jvm)"),
            mainText.contains("object Main"),
            !script,
            !lock,
            !ci,
          )
    ,
    test("preview compile names an uncompiled source"):
      Workspace
        .work("histos-cli-preview")
        .flatMap: dir =>
          val source = dir.resolve("previewlaw").resolve("src").resolve("main").resolve("scala").resolve("Ready.scala")
          for
            _       <- standIn(dir)
            _       <- Workspace.writeString(dir.resolve("histos.scala"), build)
            _       <- Workspace.writeString(source, "object Ready:\n  def n: Int = 1\n")
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
  ).provideSomeLayer[TestEnvironment & Scope](stubbed[ReleaseFeed] ++ stubbed[Resolve] ++ LawMachine.redirected)

  private def textAt(path: Path): ZIO[Any, Nothing, String] =
    ZIO.attempt(if Files.isRegularFile(path) then Files.readString(path) else "").orDie

  private def standIn(dir: Path) =
    LawMachine.system(dir) *> TestSystem.putEnv("XDG_CACHE_HOME", "") *> TestSystem.putEnv("HISTOS_VERSION", version)
end CliPlanLawSpec
