package histos.mcp

import heddle.LinePipe
import heddle.endpoint.Schema
import heddle.mcp.{Mcp, McpBuildError}
import histos.cache.ContentStore
import histos.cli.{GraphText, Lines}
import histos.testrun.{ModuleTest, ModuleTestError, SuitePattern, TestFeed}
import histos.zinc.{CompileFeed, GraphCompile, GraphCompileError}
import histos.jdk.Machine
import histos.load.{LoadError, Loader}
import histos.plugin.{PluginRunError, PluginTaskId}
import histos.resolve.Resolve
import histos.schedule.RemoteCache
import histos.task.{TaskError, TaskId, Tasks}
import histos.toolchain.Survey
import histos.version.VersionTask
import histos.ProjectGraph

import java.nio.file.Path

import zio.*
import zio.json.JsonCodec

/** MCP tools over a loaded build: `graph`, `run`, `cancel`, and `toolchain`. */
object HistosMcp:
  final case class NoArgs() derives Schema, JsonCodec
  final case class RunArgs(id: String) derives Schema, JsonCodec
  final case class CancelArgs(id: String) derives Schema, JsonCodec
  final case class CompileArgs(projects: List[String] = Nil)
      derives Schema,
        JsonCodec
  final case class TestArgs(
      projects: List[String] = Nil,
      patterns: List[String] = Nil,
      quick: Boolean = false
  ) derives Schema,
        JsonCodec
  final case class ToolFailure(message: String) derives Schema, JsonCodec

  enum ServeError:
    case Load(error: LoadError)
    case Build(errors: NonEmptyChunk[McpBuildError])
    case Stdio

    def message: String = this match
      case Load(error)   => error.message
      case Build(errors) => errors.map(_.message).mkString("\n")
      case Stdio         => "the mcp pipe failed"

  def serve(
      repo: Path,
      apiJar: Path,
      pipe: LinePipe,
      machine: Machine
  ): ZIO[Loader & Scope & Resolve & ContentStore, ServeError, Unit] =
    for
      loaded <- ZIO
        .serviceWithZIO[Loader](_.load(repo, apiJar))
        .mapError(ServeError.Load(_))
      store <- ZIO.service[ContentStore]
      bound <- loaded.plugins.bind
        .provide(ZLayer.succeed(ContentStore.actions(store)), RemoteCache.off)
      runners = Map(
        TaskId.Versions -> VersionTask.fromRepo(loaded.graph, repo, machine.env)
      ) ++ bound
      tasks <- Tasks.live(loaded.graph, runners).build.map(_.get[Tasks])
      mcp <- ZIO
        .fromEither(
          tools(loaded.graph, tasks, machine, repo, loaded.plugins.links)
        )
        .mapError(ServeError.Build(_))
      _ <- mcp.stdio(pipe).mapError(_ => ServeError.Stdio)
    yield ()

  def tools(
      graph: ProjectGraph,
      tasks: Tasks,
      machine: Machine,
      repo: Path,
      links: List[histos.load.PluginLink] = Nil
  ): Either[NonEmptyChunk[McpBuildError], Mcp[Scope & Resolve]] =
    for
      base <- Mcp.from()
      listed <- base.tool[NoArgs]("graph", "Show the project graph")(_ =>
        ZIO.succeed(GraphText.render(graph, links))
      )
      running <- listed.tool[RunArgs]("run", "Run a task and return its text")(
        args => run(tasks, args.id)
      )
      stopped <- running.tool[CancelArgs]("cancel", "Cancel a run")(args =>
        cancel(tasks, args.id)
      )
      compiling <- stopped.tool[CompileArgs](
        "compile",
        "Compile modules. An empty list compiles every module. An aggregate name compiles its modules, on every platform each module builds."
      )(args => compile(repo, graph, args.projects))
      testing <- compiling.tool[TestArgs](
        "test",
        "Test modules. An empty list tests every module. An aggregate name tests its modules. patterns are suite name globs, the same as histos testOnly. Each module runs once per Scala version and platform it builds."
      )(args => test(repo, graph, args.projects, args.patterns, args.quick))
      ready <- testing.tool[NoArgs](
        "toolchain",
        "Show the JDK decision, and the Node or clang decision when the build needs them"
      )(_ =>
        Survey
          .read(machine, graph)
          .mapBoth(
            error =>
              val shown = Survey.shown(error)
              if shown.isEmpty then error.message
              else s"$shown\n${error.message}"
            ,
            Survey.text
          )
      )
    yield ready

  private def compile(
      repo: Path,
      graph: ProjectGraph,
      projects: List[String]
  ): ZIO[Resolve, String, String] =
    for
      jdk <- GraphCompile.jdkHere.mapError(_.message)
      lines <- Ref.make(List.empty[String])
      failed <- Ref.make(List.empty[GraphCompileError])
      _ <- GraphCompile
        .console(repo, graph, projects, jdk)
        .mapError(_.message)
        .foreach { feed =>
          feed match
            case CompileFeed.Diagnostic(_, _, _, problem) =>
              lines.update(_ :+ problem.text)
            case CompileFeed.Done(cell) =>
              lines.update(_ :+ Lines.compiled(repo, cell))
            case CompileFeed.Failed(error) =>
              failed.update(_ :+ error) *> lines.update(_ :+ error.message)
        }
      text <- lines.get
      missed <- failed.get
      result <- missed match
        case Nil          => ZIO.succeed(text.mkString("\n"))
        case head :: rest =>
          ZIO.fail(GraphCompileError.Cells(head, rest).message)
    yield result

  private def test(
      repo: Path,
      graph: ProjectGraph,
      projects: List[String],
      patterns: List[String],
      quick: Boolean
  ): ZIO[Resolve, String, String] =
    for
      parsed <- ZIO.fromEither(
        patterns.foldLeft[Either[String, List[SuitePattern]]](Right(Nil)) {
          (acc, raw) =>
            acc.flatMap(have =>
              SuitePattern.from(raw).left.map(_.message).map(have :+ _)
            )
        }
      )
      jdk <- GraphCompile.jdkHere.mapError(_.message)
      lines <- Ref.make(List.empty[String])
      failedCompile <- Ref.make(List.empty[GraphCompileError])
      failedSuite <- Ref.make(false)
      cells <- GraphCompile
        .console(repo, graph, projects, jdk)
        .mapError(_.message)
        .mapZIO { feed =>
          val line = feed match
            case CompileFeed.Diagnostic(_, _, _, problem) => problem.text
            case CompileFeed.Done(cell)    => Lines.compiled(repo, cell)
            case CompileFeed.Failed(error) => error.message
          lines.update(_ :+ line) *>
            (feed match
              case CompileFeed.Done(cell)    => ZIO.succeed(Some(cell))
              case CompileFeed.Failed(error) =>
                failedCompile.update(_ :+ error).as(None)
              case CompileFeed.Diagnostic(_, _, _, _) => ZIO.succeed(None))
        }
        .collectSome
        .runCollect
      _ <- ModuleTest
        .suites(repo, graph, projects, jdk, cells.toList, parsed, quick)
        .mapError(_.message)
        .foreach {
          case TestFeed.Begin(module, version, platform) =>
            lines.update(_ :+ Lines.begin(module, version, platform))
          case TestFeed.Out(text) => lines.update(_ :+ text)
          case TestFeed.Err(text) => lines.update(_ :+ text)
          case TestFeed.Cached(module, version, platform, suite) =>
            lines.update(_ :+ Lines.cached(module, version, platform, suite))
          case TestFeed.Done(cell) =>
            val bad = cell.status match
              case histos.testrun.TestStatus.Failed(_, _, _) => true
              case _                                         => false
            ZIO.when(bad)(failedSuite.set(true)) *> lines.update(
              _ :+ Lines.tested(cell)
            )
        }
      text <- lines.get.map(_.mkString("\n"))
      missed <- failedCompile.get
      suiteBad <- failedSuite.get
      _ <- missed match
        case head :: rest =>
          ZIO.fail(GraphCompileError.Cells(head, rest).message)
        case Nil if suiteBad => ZIO.fail(text)
        case Nil             => ZIO.unit
    yield text

  private def run(tasks: Tasks, raw: String): ZIO[Scope, ToolFailure, String] =
    ZIO
      .fromEither(TaskId.from(raw))
      .mapError(failed)
      .flatMap: id =>
        tasks
          .run(id)
          .mapError(missed(raw, _))
          .flatMap(started =>
            started.await.mapError(missed(raw, _)).map(_.text)
          )

  private def missed(raw: String, err: TaskError): ToolFailure =
    err match
      case TaskError.NoRunner(_) =>
        PluginTaskId.parse(raw) match
          case Right(id) => ToolFailure(PluginRunError.Unknown(id).message)
          case Left(_)   => failed(err)
      case other => failed(other)

  private def cancel(tasks: Tasks, raw: String): ZIO[Any, ToolFailure, String] =
    ZIO
      .fromEither(histos.task.RunId.from(raw))
      .mapError(failed)
      .flatMap: id =>
        tasks.cancel(id).mapError(failed).as("cancelled")

  private def failed(err: TaskError): ToolFailure = ToolFailure(err.message)
end HistosMcp
