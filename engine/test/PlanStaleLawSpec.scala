package histos.preview

import histos.testrun.TestCompile
import histos.workspace.Workspace
import histos.zinc.{AnalysisKind, SourceStale}
import histos.{Jvm, ProjectName, Projects, module, scala}

import java.nio.file.Path

import zio.ZIO
import zio.test.{ZIOSpecDefault, assertTrue, test}

/** An unreadable analysis is not a first compile, and a class with no record is not skippable. */
object PlanStaleLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"

  def spec = suite("plan staleness laws")(
    test("garbage analysis is unknown, and a class file with no record is not skippable"):
      val built = Projects(module("core", Jvm)(scala3))
      built match
        case Left(error)  => ZIO.dieMessage(error.message)
        case Right(graph) =>
          graph.projects.collect { case item: histos.Module => item } match
            case core :: Nil =>
              Workspace
                .work("histos-plan-stale")
                .flatMap: repo =>
                  val name   = ProjectName.trusted("core")
                  val source =
                    repo.resolve("core").resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
                  val mainZip = SourceStale.directory(repo, name, scala3, Jvm, AnalysisKind.Main).resolve("inc.zip")
                  val testZip = SourceStale.directory(repo, name, scala3, Jvm, AnalysisKind.Test).resolve("inc.zip")
                  val probe   = TestCompile.testClasses(repo, name, scala3, Jvm).resolve("Probe.class")
                  for
                    _      <- Workspace.createDirectories(parentOf(source))
                    _      <- Workspace.createDirectories(parentOf(mainZip))
                    _      <- Workspace.createDirectories(parentOf(testZip))
                    _      <- Workspace.createDirectories(parentOf(probe))
                    _      <- Workspace.writeString(source, "object Hello\n")
                    _      <- Workspace.writeString(mainZip, "not-zinc")
                    _      <- Workspace.writeString(testZip, "not-zinc")
                    _      <- Workspace.writeString(probe, "not-a-class")
                    shown  <- Plan.show(repo, graph, List(core.name))
                    tested <- Plan.test(repo, graph, List(core.name)).map(plan => Plan.renderTest(repo, plan))
                  yield assertTrue(
                    shown.contains("file-level staleness: unknown"),
                    !shown.contains("no previous compile"),
                    !shown.contains("\nfresh"),
                    tested.contains("file-level staleness: unknown"),
                    tested.contains("quick: not known to be skippable Probe"),
                    !tested.contains("quick: no suite would be skipped"),
                    core.name == name,
                  )
            case _ => ZIO.dieMessage("the graph did not keep core")
  )

  private def parentOf(path: Path): Path =
    Option(path.getParent).getOrElse(path)
end PlanStaleLawSpec
