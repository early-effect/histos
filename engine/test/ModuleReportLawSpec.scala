package histos.testrun

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.resolve.{DependencyLock, Resolve}
import histos.toolchain.Runtimes
import histos.version.Catalog
import histos.workspace.Workspace
import histos.zinc.GraphCompile
import histos.{
  CompilerClosure,
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

import zio.{Scope, ZIO, ZLayer}
import zio.test.{TestAspect, ZIOSpecDefault, assertTrue, suite, test}

/** `run` folds the console into one report per cell. The lines are the process lines. */
object ModuleReportLawSpec extends ZIOSpecDefault:
  private val lawName = ProjectName.trusted("reportlaw")

  def spec = suite("module report laws")(
    test("a test main's stdout and stderr are the report, and a zero exit is a pass"):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        Workspace
          .work("histos-module-report")
          .flatMap: dir =>
            val main  = dir.resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
            val probe = dir.resolve("src").resolve("test").resolve("scala").resolve("Probe.scala")
            for
              machine  <- Machine.load(dir)
              jdk      <- GraphCompile.jdk(machine)
              runtimes <- Runtimes.read(machine, None)
              _        <- Workspace
                .writeString(dir.resolve("dependencies.lock"), DependencyLock.render(closure.pinning(lawName)))
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
                .run(TestRun(dir, graph(closure.scalaVersion), List(lawName), jdk, runtimes, TestMode.Run))
                .provideSomeLayer[Resolve](ZLayer.succeed(ContentStore.in(dir.resolve("store"))))
            yield assertTrue(shown(reports) == "passed|histos-report-line|histos-report-err")
    @@ TestAspect.withLiveSystem
  ).provideSomeLayerShared[zio.test.TestEnvironment & Scope](CompilerClosure.layer)

  private def shown(reports: List[TestReport]): String =
    reports
      .map: row =>
        val out = row.transcript.out.mkString(",")
        val err = row.transcript.err.mkString(",")
        s"${row.cell.status.label}|$out|$err"
      .mkString(";")

  private def graph(scala3: ScalaVersion): ProjectGraph =
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
end ModuleReportLawSpec
