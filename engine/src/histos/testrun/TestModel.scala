package histos.testrun

import histos.jdk.JdkRuntime
import histos.toolchain.Runtimes
import histos.zinc.{CompileFacts, CompileFeed}
import histos.{Platform, PlatformSelection, ProjectGraph, ProjectName, ScalaVersion, TestSelection}

import java.nio.file.Path

import zio.UIO

final case class TestReport(cell: TestedCell, transcript: Transcript)

enum SessionFeed:
  case Compile(feed: CompileFeed)
  case Test(feed: TestFeed)

/** Platform selection filters requested tests, never their compilation dependencies. */
final case class TestRun(
    repo: Path,
    graph: ProjectGraph,
    names: List[ProjectName],
    jdk: JdkRuntime,
    runtimes: Runtimes,
    mode: TestMode,
    patterns: List[SuitePattern] = Nil,
    filter: TestFilter = TestFilter.empty,
    onTestCompile: (ProjectName, ScalaVersion, Platform, CompileFacts) => UIO[Unit] = (_, _, _, _) => zio.ZIO.unit,
    coverage: Option[histos.coverage.CoverageCompile] = None,
    platforms: PlatformSelection = PlatformSelection.All,
    requested: Option[TestSelection] = None,
    cellPolicy: TestCellPolicy = TestCellPolicy.Declared,
):
  def selection: TestSelection = requested.getOrElse(TestSelection.named(names, platforms))
