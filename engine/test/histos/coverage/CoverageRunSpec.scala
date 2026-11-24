package histos.coverage

import histos.*
import histos.testrun.TestExecutionFixtures
import histos.workspace.Workspace
import zio.{Ref, ZIO, durationInt}
import zio.test.*

object CoverageRunSpec extends ZIOSpecDefault:
  private val version = ScalaVersion("2.13.18")
  private val name    = ProjectName("app")
  private val tests   = suite("actual coverage execution")(
    test(
      "measured JVM intent retains explicitly selected verification lanes and excludes unsupported cells before DAG planning"
    ) {
      val measured    = ScalaVersion("2.13.18")
      val unsupported = ScalaVersion("2.12.20")
      val verified    = Module(
        name,
        Platforms(Jvm, List(Js, Native)),
        ScalaVersions(measured, List(unsupported)),
        Nil,
        Nil,
        SourcePos("CoverageRunSpec.scala", 1, 1),
        lane = TestLane.Verify,
      )
      val graph   = ProjectGraph.trusted(List(verified))
      val planned = histos.testrun.TestCells.plan(
        graph,
        TestSelection(TestTargets.Filtered(List(name)), PlatformSelection.only(Jvm)),
        histos.testrun.TestCellPolicy.MeasuredJvm,
      )
      val wanted = histos.Cell(name, measured, Jvm)
      assertTrue(
        planned.exists(plan =>
          plan.requestedCells == List(wanted) &&
            plan.compileStageRoots == List(CompilationStage(wanted, histos.plugin.SourceSet.Test)) &&
            plan.executionGroups == List(histos.testrun.TestExecutionGroup.Direct(wanted))
        )
      )
    },
    test("Scala 2.13 Main records measurements through the requested suite and enforces report floors") {
      ZIO
        .fromEither(Projects(module("app", Jvm)(version).test(TestMain("demo.Check"))))
        .flatMap: graph =>
          TestExecutionFixtures.withGraph(graph): fixture =>
            for
              _ <- Workspace.writeString(
                fixture.repo.resolve("app/src/demo/Main.scala"),
                "package demo\nobject Main { def value(hit: Boolean): Int = if (hit) 42 else 0 }\n",
              )
              _ <- Workspace.writeString(
                fixture.repo.resolve("app/test/demo/Check.scala"),
                "package demo\nobject Check { def main(args: Array[String]): Unit = println(\"measured=\" + Main.value(true)) }\n",
              )
              heard <- Ref.make(List.empty[String])
              request = CoverageRequest(
                fixture.repo,
                graph,
                List(name),
                fixture.session.jdk,
                fixture.runtimes,
                CoverageFloors(statements = Some(1)),
                Excludes(Nil, Nil),
                123L,
              )
              result   <- Run.execute(request, CoverageLines(line => heard.update(_ :+ line), _ => ZIO.unit))
              measured <- Gather.read(Flags.classesDir(fixture.repo, name, version, Jvm))
              report = Report.of(measured._1, measured._2)
              xml    <- Workspace.readString(fixture.repo.resolve(".histos/coverage/scoverage.xml"))
              html   <- Workspace.readString(fixture.repo.resolve(".histos/coverage/index.html"))
              lines  <- heard.get
              strict <- Run.execute(
                request.copy(floors = CoverageFloors(branches = Some(100))),
                CoverageLines(_ => ZIO.unit, _ => ZIO.unit),
              )
            yield assertTrue(
              result == CoverageResult.Met(Render.show(report, request.floors)),
              report.statements.hit > 0,
              report.branches.hit > 0,
              report.branches.hit < report.branches.total,
              measured._2.nonEmpty,
              lines.exists(_.contains("measured=42")),
              xml.contains("timestamp=\"123\""),
              xml.contains(s"statements-invoked=\"${report.statements.hit}\""),
              html.contains(s"statements ${report.statements.hit}/${report.statements.total}"),
              strict == CoverageResult.Missed(Render.show(report, CoverageFloors(branches = Some(100)))),
            )
    },
  )

  def spec = tests @@ TestAspect.withLiveClock @@ TestAspect.timeout(5.minutes)
end CoverageRunSpec
