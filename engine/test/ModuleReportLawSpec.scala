package histos.testrun

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.resolve.{ClasspathScope, DependencyLock, LockedCell, Resolve}
import histos.toolchain.Runtimes
import histos.version.Catalog
import histos.workspace.Workspace
import histos.zinc.GraphCompile
import histos.{
  Module,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  Resolution,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  Splits,
  TestMain,
}

import java.nio.file.{Files, Path}

import zio.{Scope, ZIO, ZLayer}
import zio.test.{TestAspect, ZIOSpecDefault, assertTrue, suite, test}

/** `run` folds the console into one report per cell. The lines are the process lines. */
object ModuleReportLawSpec extends ZIOSpecDefault:
  private val scala3  = ScalaVersion("3.10.0")
  private val lawName = ProjectName.trusted("reportlaw")

  def spec = suite("module report laws")(
    test("a test main's stdout and stderr are the report, and a zero exit is a pass"):
      Workspace
        .work("histos-module-report")
        .flatMap: dir =>
          Machine
            .load(dir)
            .either
            .flatMap:
              case Left(error)    => ZIO.succeed(assertTrue(error.message == ""))
              case Right(machine) =>
                GraphCompile
                  .jdk(machine)
                  .either
                  .flatMap:
                    case Left(error) => ZIO.succeed(assertTrue(error.message == ""))
                    case Right(jdk)  =>
                      ZIO.serviceWithZIO[DependencyLock]: lock =>
                        val main  = dir.resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
                        val probe = dir.resolve("src").resolve("test").resolve("scala").resolve("Probe.scala")
                        val ran   =
                          for
                            runtimes <- Runtimes.read(machine, None)
                            _        <- Workspace.writeString(
                              dir.resolve("dependencies.lock"),
                              DependencyLock.render(pinned(lock)),
                            )
                            _ <- Workspace.writeString(main, "object Hello:\n  def ready: Int = 1\n")
                            _ <- Workspace.writeString(
                              probe,
                              """object Probe:
                                |  def main(args: Array[String]): Unit =
                                |    println("histos-report-line")
                                |    System.err.println("histos-report-err")
                                |""".stripMargin,
                            )
                            reports <- ModuleTest
                              .run(TestRun(dir, graph, List(lawName), jdk, runtimes, TestMode.Run))
                              .provideSomeLayer[Resolve](ZLayer.succeed(ContentStore.in(dir.resolve("store"))))
                              .either
                          yield assertTrue(shown(reports) == "passed|histos-report-line|histos-report-err")
                        ran
    @@ TestAspect.withLiveSystem
  ).provideSomeLayerShared[zio.test.TestEnvironment & Scope](opened)

  private def shown(reports: Either[ModuleTestError, List[TestReport]]): String =
    reports match
      case Left(error) => error.message
      case Right(rows) =>
        rows
          .map: row =>
            val out = row.transcript.out.mkString(",")
            val err = row.transcript.err.mkString(",")
            s"${row.cell.status.label}|$out|$err"
          .mkString(";")

  private def graph: ProjectGraph =
    val law = Module(
      lawName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
      test = Some(TestMain("Probe")),
    )
    ProjectGraph.trusted(
      List(law),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      resolution = Resolution.Lock,
    )

  private def pinned(lock: DependencyLock): DependencyLock =
    lock.copy(cells = LockedCell(lawName, scala3, Platform.Jvm, Nil, Nil, Nil, ClasspathScope.Compile) :: lock.cells)

  private def opened =
    Resolve.offline(_root_.coursier.cache.CacheDefaults.location.toPath) ++
      ZLayer.fromZIO(Resolve.readLock(repository))

  private def repository: Path =
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
end ModuleReportLawSpec
