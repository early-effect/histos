package histos.cli

import histos.workspace.Workspace

import java.nio.file.{Files, Path}

import zio.ZIO
import zio.test.{TestSystem, ZIOSpecDefault, assertTrue, suite, test}

/** `coverage` on a Scala 3 module writes the report and does not fetch a coverage plugin. */
object CliCoverLawSpec extends ZIOSpecDefault:
  private val version = "9.9.9-test"

  private val build =
    """import histos.*
      |
      |object build extends HistosBuild:
      |  def graph =
      |    val coverlaw = module("coverlaw", Jvm)(scala"3.10.0").test(TestMain("Probe"))
      |    Projects.locked(coverlaw)
      |""".stripMargin

  private val hello =
    """object Hello:
      |  def ready: Int = 1
      |  def unused: Int = 2
      |""".stripMargin

  private val probe =
    """object Probe:
      |  def main(args: Array[String]): Unit = ()
      |""".stripMargin

  def spec = suite("cli coverage laws")(
    test("a passing test main meets an empty floor and writes both reports"):
      Workspace
        .work("histos-cli-cover")
        .flatMap: dir =>
          val source = dir.resolve("coverlaw").resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
          val test   = dir.resolve("coverlaw").resolve("src").resolve("test").resolve("scala").resolve("Probe.scala")
          val report = dir.resolve(".histos").resolve("coverage")
          for
            _    <- standIn(dir)
            lock <- ZIO.attempt(Files.readString(checkout.resolve("dependencies.lock"))).orDie
            _    <- Workspace.createDirectories(parentOf(source)).catchAll(error => ZIO.dieMessage(error.message))
            _    <- Workspace.createDirectories(parentOf(test)).catchAll(error => ZIO.dieMessage(error.message))
            _    <- Workspace
              .writeString(dir.resolve("histos.scala"), build)
              .catchAll(error => ZIO.dieMessage(error.message))
            _ <- Workspace
              .writeString(dir.resolve("dependencies.lock"), s"cell coverlaw 3.10.0 jvm\n$lock")
              .catchAll(error => ZIO.dieMessage(error.message))
            _    <- Workspace.writeString(source, hello).catchAll(error => ZIO.dieMessage(error.message))
            _    <- Workspace.writeString(test, probe).catchAll(error => ZIO.dieMessage(error.message))
            ran  <- Cli.launch(List("coverage"), dir)
            xml  <- ZIO.attempt(Files.isRegularFile(report.resolve("scoverage.xml"))).orDie
            html <- ZIO.attempt(Files.isRegularFile(report.resolve("index.html"))).orDie
            text <- ZIO
              .attempt(
                if Files.isRegularFile(report.resolve("scoverage.xml")) then
                  Files.readString(report.resolve("scoverage.xml"))
                else ""
              )
              .orDie
          yield assertTrue(
            ran.exit == CliExit.Success,
            ran.err == "",
            ran.out.contains("statements"),
            xml,
            html,
            text.contains("coverlaw"),
          )
  )

  private def parentOf(path: Path): Path =
    Option(path.getParent).getOrElse(path)

  private def standIn(dir: Path) =
    for
      _ <- TestSystem.putProperty("java.home", java.lang.System.getProperty("java.home"))
      _ <- TestSystem.putProperty("os.name", java.lang.System.getProperty("os.name"))
      _ <- TestSystem.putProperty("os.arch", java.lang.System.getProperty("os.arch"))
      _ <- TestSystem.putProperty("user.home", dir.toString)
      _ <- TestSystem.putEnv("XDG_CACHE_HOME", "")
      _ <- TestSystem.putEnv("HISTOS_VERSION", version)
    yield ()

  private def checkout: Path =
    def walk(path: Path, left: Int): Option[Path] =
      if left == 0 then None
      else if Files.isRegularFile(path.resolve("dependencies.lock")) && Files.isRegularFile(
          path.resolve("histos.scala")
        )
      then Some(path)
      else
        Option(path.getParent) match
          case Some(parent) => walk(parent, left - 1)
          case None         => None
    walk(Path.of("").toAbsolutePath.normalize, 8).getOrElse(Path.of("").toAbsolutePath.normalize)
end CliCoverLawSpec
