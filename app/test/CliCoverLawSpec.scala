package histos.cli

import histos.cache.ContentStore
import histos.load.{BuildClasspath, Loader}
import histos.publish.IvyRoot
import histos.resolve.{DependencyLock, Resolve}
import histos.workspace.Workspace
import histos.{CompilerClosure, HistosBuild, LawMachine, ProjectName}

import java.nio.file.{Files, Path}

import zio.{Scope, ZIO, ZLayer}
import zio.test.{TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}

/** `coverage` on a Scala 3 module writes the report and does not fetch a coverage plugin. Resolve reads the Coursier
  * cache offline, so a cold cache is the closure's precondition and never a download.
  */
object CliCoverLawSpec extends ZIOSpecDefault:
  private val coverlaw = ProjectName.trusted("coverlaw")

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
    test("a test main that calls nothing meets an empty floor, names its statements, and writes both reports"):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        Workspace
          .work("histos-cli-cover")
          .flatMap: dir =>
            val source = dir.resolve("coverlaw").resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
            val test   = dir.resolve("coverlaw").resolve("src").resolve("test").resolve("scala").resolve("Probe.scala")
            val report = dir.resolve(".histos").resolve("coverage")
            val build  =
              s"""import histos.*
                 |
                 |object build extends HistosBuild:
                 |  def graph =
                 |    val coverlaw = module("coverlaw", Jvm)(scala"${closure.scalaVersion.value}").test(TestMain("Probe"))
                 |    Projects.locked(coverlaw)
                 |""".stripMargin
            for
              _ <- Workspace.writeString(dir.resolve("histos.scala"), build)
              _ <- Workspace
                .writeString(dir.resolve("dependencies.lock"), DependencyLock.render(closure.pinning(coverlaw)))
              _    <- Workspace.writeString(source, hello)
              _    <- Workspace.writeString(test, probe)
              api  <- apiJar(dir.resolve("cache"))
              ran  <- Cli.execute(List("coverage"), dir, api).provideSomeLayer[Resolve & Scope](services(dir))
              xml  <- ZIO.attempt(Files.readString(report.resolve("scoverage.xml"))).orDie
              html <- ZIO.attempt(Files.isRegularFile(report.resolve("index.html"))).orDie
            yield assertTrue(
              ran.exit == CliExit.Success,
              ran.err == "",
              ran.out.linesIterator.toList.take(2) == List("statements 0/4 0.00", "branches 0/0 0.00"),
              html,
              xml.contains("coverlaw"),
            )
  ).provideSomeLayerShared[TestEnvironment & Scope](CompilerClosure.layer)

  /** The api jar the build file compiles against, packed from this test JVM's classes under `cache`. */
  private def apiJar(cache: Path) =
    ZIO
      .fromOption(
        Option(classOf[HistosBuild].getProtectionDomain)
          .flatMap(domain => Option(domain.getCodeSource))
          .flatMap(source => Option(source.getLocation))
      )
      .flatMap(url => ZIO.attempt(Path.of(url.toURI)))
      .flatMap(BuildClasspath(_, cache))
      .orDieWith(error => IllegalStateException(s"the histos api classes are not on this law's classpath: $error"))

  private def services(dir: Path) =
    Loader.live ++
      ZLayer.succeed(ContentStore.in(dir.resolve("store"))) ++
      ZLayer.fromZIO(LawMachine.at(dir)) ++
      ZLayer.succeed(IvyRoot.at(dir.resolve("ivy")))
end CliCoverLawSpec
