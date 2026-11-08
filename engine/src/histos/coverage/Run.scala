package histos.coverage

import histos.jdk.JdkRuntime
import histos.resolve.{BuildResolvers, Resolve, ResolveError}
import histos.select.{SelectError, Selection}
import histos.testrun.{ModuleTest, ModuleTestError, SessionFeed, TestFeed, TestRun, TestStatus}
import histos.toolchain.Runtimes
import histos.workspace.{IoFailure, Workspace}
import histos.zinc.{CompileFeed, GraphCompileError, ScalaSeries}
import histos.{CoverageFloors, Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.Path

import zio.{UIO, ZIO}

/** Why `histos coverage` stopped before it could write the report. */
enum CoverageFailure:
  case Selected(error: SelectError)
  case Resolved(error: ResolveError)
  case Test(error: ModuleTestError)
  case Reported(error: CoverageError)
  case Instrument(error: CellFault)
  case Io(fault: IoFailure)
  case Compiled(error: GraphCompileError)

  def message: String = this match
    case Selected(error)   => error.message
    case Resolved(error)   => error.message
    case Test(error)       => error.message
    case Reported(error)   => error.message
    case Instrument(error) => error.message
    case Io(fault)         => fault.message
    case Compiled(error)   => error.message
end CoverageFailure

enum CoverageResult:
  case Met(text: String)
  case Missed(text: String)

/** One coverage command: the suite, the floors, the exclusions, and the report timestamp. */
final case class CoverageRequest(
    repo: Path,
    graph: ProjectGraph,
    names: List[ProjectName],
    jdk: JdkRuntime,
    runtimes: Runtimes,
    floors: CoverageFloors,
    excludes: Excludes,
    at: Long,
)

/** Live compiler and test lines. The report is the result, not these lines. */
final case class CoverageLines(out: String => UIO[Unit], err: String => UIO[Unit])

object Run:
  def execute(
      request: CoverageRequest,
      lines: CoverageLines,
  ): ZIO[Resolve & histos.cache.ContentStore, CoverageFailure, CoverageResult] =
    val CoverageRequest(repo, graph, names, jdk, runtimes, floors, excludes, at) = request
    val modules = graph.projects.collect { case module: Module => module }
    for
      selected <- ZIO.fromEither(Selection.select(graph, names)).mapError(CoverageFailure.Selected(_))
      closed = Module.closure(modules, selected.modules.map(_.name).toSet)
      plugins <- fetch(graph, closed, BuildResolvers(graph.resolvers))
      spec  = CoverageCompile(excludes, graph.tools.compilers.scoverage, plugins)
      asked = TestRun(
        repo,
        graph,
        names,
        jdk,
        runtimes,
        histos.testrun.TestMode.Run,
        coverage = Some(spec),
      )
      refused <- run(asked, lines)
      parts   <- read(repo, closed)
      summed  = Report.sum(parts.map((rows, invoked) => Report.of(rows, invoked)))
      skipped = Cell.skipped(selected.modules)
      text    = Render.show(summed, floors, skipped)
      xml     = Render.xmlParts(parts, at)
      html    = Render.page(summed, skipped)
      _ <- write(repo, xml, html)
      missed = refused || Floor.shortfalls(summed, floors).nonEmpty
    yield if missed then CoverageResult.Missed(text) else CoverageResult.Met(text)

  private def run(
      asked: TestRun,
      lines: CoverageLines,
  ): ZIO[Resolve & histos.cache.ContentStore, CoverageFailure, Boolean] =
    ModuleTest
      .session(asked)
      .tap(feed => speak(feed, lines))
      .runFold(Heard(false, Nil))(note)
      .mapError(CoverageFailure.Test(_))
      .flatMap: heard =>
        heard.failed match
          case head :: _ => ZIO.fail(CoverageFailure.Compiled(head))
          case Nil       => ZIO.succeed(heard.refused)

  private def speak(feed: SessionFeed, lines: CoverageLines): UIO[Unit] =
    feed match
      case SessionFeed.Test(TestFeed.Out(_, _, _, text)) => lines.out(text)
      case SessionFeed.Test(TestFeed.Err(_, _, _, text)) => lines.err(text)
      case _                                             => ZIO.unit

  private def fetch(
      graph: ProjectGraph,
      modules: List[Module],
      repositories: histos.resolve.Repositories,
  ): ZIO[Resolve, CoverageFailure, List[CoveragePlugin]] =
    val versions = modules
      .flatMap(_.versions.all)
      .filter(version => ScalaSeries.of(version).contains(ScalaSeries.Scala2(13)))
      .foldLeft(List.empty[ScalaVersion]): (seen, version) =>
        if seen.contains(version) then seen else seen :+ version
    ZIO.foreach(versions): version =>
      ZIO
        .serviceWithZIO[Resolve](
          _.resolveTool(Tools.request(version, graph.tools.compilers.scoverage, repositories))
        )
        .mapError(CoverageFailure.Resolved(_))
        .flatMap: classpath =>
          ZIO
            .fromEither(CoveragePlugin.from(version, classpath.artifacts))
            .mapError(CoverageFailure.Instrument(_))

  private def read(
      repo: Path,
      modules: List[Module],
  ): ZIO[Any, CoverageFailure, List[(List[Statement], Set[Int])]] =
    val cells = modules.flatMap: module =>
      module.versions.all.filter(version => Cell.measures(version, Platform.Jvm)).map(version => (module.name, version))
    ZIO.foreach(cells): (name, version) =>
      Gather.read(Flags.classesDir(repo, name, version, Platform.Jvm)).mapError(lift)

  private def write(repo: Path, xml: String, html: String): ZIO[Any, CoverageFailure, Unit] =
    val root = repo.resolve(".histos").resolve("coverage")
    (
      Workspace.createDirectories(root) *>
        Workspace.writeString(root.resolve("scoverage.xml"), xml) *>
        Workspace.writeString(root.resolve("index.html"), html)
    ).mapError(CoverageFailure.Io(_))

  private def lift(error: CoverageError | IoFailure): CoverageFailure =
    error match
      case found: CoverageError => CoverageFailure.Reported(found)
      case found: IoFailure     => CoverageFailure.Io(found)

  private def failedTest(status: TestStatus): Boolean =
    status match
      case TestStatus.Failed(_)                                          => true
      case TestStatus.NoTests | TestStatus.Unmatched | TestStatus.Passed => false

  private final case class Heard(refused: Boolean, failed: List[GraphCompileError])

  private def note(have: Heard, feed: SessionFeed): Heard =
    feed match
      case SessionFeed.Compile(CompileFeed.Failed(error)) =>
        have.copy(failed = have.failed :+ error)
      case SessionFeed.Test(TestFeed.Done(cell)) =>
        have.copy(refused = have.refused || failedTest(cell.status))
      case _ => have
end Run
