package histos.server

import histos.assembly.Assemble
import histos.cache.ContentStore
import histos.cli.{Cli, CliExit, GraphText, Lines}
import histos.testrun.{
  ModuleTest,
  ModuleTestError,
  SuitePattern,
  TestFeed,
  TestStatus
}
import histos.zinc.{CompileFeed, GraphCompile, GraphCompileError, ResolvedJdk}
import histos.jdk.{Machine, Os, Settled}
import histos.jdk.Sha256
import histos.load.{LoadError, PluginLink, PluginRuntime}
import histos.plugin.{PluginArgs, PluginRunError, PluginTaskId}
import histos.publish.PublishRun
import histos.resolve.{BuildResolvers, Repositories, Resolve, ResolveError}
import histos.schedule.RemoteCache
import histos.task.{RunId, TaskError, TaskId, TaskResult}
import histos.toolchain.Survey
import histos.version.VersionTask
import histos.workflow.{Workflow, WorkflowAction}
import histos.{Platform, ProjectGraph}

import zio.json.*
import zio.{Chunk, Fiber, IO, Ref, ZIO, ZLayer}

import java.io.OutputStream
import java.nio.file.{Files, Path}

/** One CLI message, sent as a frame or kept for a tool result. */
final class Say(private val send: Message => IO[ServerError, Unit]):
  def apply(message: Message): IO[ServerError, Unit] = send(message)

object Say:
  def frames(stream: OutputStream): Say =
    new Say(message =>
      histos.bsp.Frame
        .write(stream, message.toJson)
        .mapError(_ => ServerError.Io(ServerIo.Write))
    )

  def record(feeds: Ref[Chunk[Feed]]): Say =
    new Say({
      case Message.Out(text)       => feeds.update(_ :+ Feed.Out(text))
      case Message.Err(text)       => feeds.update(_ :+ Feed.Err(text))
      case Message.Exit(code, err) => feeds.update(_ :+ Feed.Exit(code, err))
      case _                       => ZIO.unit
    })

/** One graph command, written as the CLI would print it. */
object Calls:
  val quiet: PluginRuntime = Quiet

  def idleMachine: Machine =
    Machine(
      Map.empty,
      ".",
      Os.Other,
      Path.of(".histos"),
      Path.of("."),
      None,
      Settled.No
    )

  def graph(
      graph: ProjectGraph,
      plugins: PluginRuntime,
      say: Say
  ): IO[ServerError, Unit] =
    told(say, GraphText.render(graph, plugins.links)) *> done(
      say,
      CliExit.Success.code,
      ""
    )

  def publish(
      repo: Path,
      graph: ProjectGraph,
      machine: Machine,
      jdk: ResolvedJdk,
      repository: String,
      projects: List[String],
      say: Say
  ): ZIO[Resolve, ServerError, Unit] =
    PublishRun
      .run(repo, graph, projects, jdk, machine.env, repository)
      .foldZIO(
        failure => done(say, CliExit.PublishFailed.code, failure.message),
        report =>
          told(say, report.lines.mkString("\n")) *> done(say, CliExit.Success.code, "")
      )

  def run(
      repo: Path,
      graph: ProjectGraph,
      jdk: ResolvedJdk,
      machine: Machine,
      id: String,
      platform: Option[String],
      say: Say
  ): ZIO[Resolve, ServerError, Unit] =
    chosen(platform) match
      case Left(message) => done(say, CliExit.TaskFailed.code, message)
      case Right(choice) =>
        Ref.make(0).flatMap { status =>
          histos.program.Program
            .console(repo, graph, id, jdk, machine.env, choice)
            .foreach {
              case histos.program.ProgramFeed.Begin(module, version, target) =>
                told(
                  say,
                  s"${module.value} ${version.value} ${target.token}"
                )
              case histos.program.ProgramFeed.Out(text) => told(say, text)
              case histos.program.ProgramFeed.Err(text) =>
                emit(say, Message.Err(text))
              case histos.program.ProgramFeed.Exited(_, _, _, code) =>
                ZIO.when(code != 0)(status.set(code)).unit
            }
            .foldZIO(
              {
                case error: histos.program.ProgramError =>
                  done(say, programCode(error), error.message)
                case error: ServerError => ZIO.fail(error)
              },
              _ =>
                status.get.flatMap(code =>
                  done(
                    say,
                    if code == 0 then CliExit.Success.code else code,
                    ""
                  )
                )
            )
        }

  def task(
      repo: Path,
      graph: ProjectGraph,
      plugins: PluginRuntime,
      machine: Machine,
      id: String,
      say: Say
  ): ZIO[Resolve & histos.load.Loader & ContentStore, ServerError, Unit] =
    TaskId.from(id) match
      case Left(error) => done(say, CliExit.TaskFailed.code, error.message)
      case Right(found) if found == TaskId.Inspect =>
        told(say, graph.projects.map(_.name.value).mkString(",")) *> done(
          say,
          CliExit.Success.code,
          ""
        )
      case Right(found) if found == TaskId.Versions =>
        VersionTask
          .fromRepo(graph, repo, machine.env)
          .foldZIO(
            error => done(say, CliExit.TaskFailed.code, error.message),
            result =>
              told(say, result.text) *> done(
                say,
                CliExit.Success.code,
                ""
              )
          )
      case Right(_) =>
        PluginTaskId.parse(id) match
          case Left(error) =>
            done(say, CliExit.TaskFailed.code, error.message)
          case Right(taskId) => plugin(plugins, taskId, say)

  def toolchain(
      machine: Machine,
      graph: ProjectGraph,
      say: Say
  ): IO[ServerError, Unit] =
    Survey
      .read(machine, graph)
      .foldZIO(
        error =>
          told(say, Survey.shown(error)) *> done(
            say,
            CliExit.ToolchainFailed.code,
            error.message
          ),
        found =>
          told(say, Survey.text(found)) *> done(
            say,
            CliExit.Success.code,
            ""
          )
      )

  def resolve(
      repo: Path,
      graph: ProjectGraph,
      action: String,
      say: Say
  ): ZIO[Resolve & histos.load.Loader, ServerError, Unit] =
    resolveAction(action) match
      case None =>
        done(say, CliExit.Usage.code, s"'$action' is not a resolve action")
      case Some(kind) =>
        ZIO
          .serviceWithZIO[histos.load.Loader](_.scan(repo))
          .either
          .flatMap:
            case Left(error) =>
              done(say, CliExit.LoadFailed.code, error.message)
            case Right(scanned) =>
              val followed: ZIO[
                Resolve,
                LoadError | ResolveError,
                String
              ] =
                kind match
                  case histos.cli.ResolveAction.Verify =>
                    Resolve.requirePlugins(repo, scanned.requests) *>
                      Resolve.verify(
                        repo,
                        graph,
                        BuildResolvers(graph.resolvers)
                      )
                  case histos.cli.ResolveAction.Check =>
                    Resolve.requirePlugins(repo, scanned.requests) *>
                      Resolve.checkPlugins(
                        repo,
                        scanned.requests,
                        Repositories.central
                      ) *>
                      Resolve.check(
                        repo,
                        graph,
                        BuildResolvers(graph.resolvers)
                      )
                  case histos.cli.ResolveAction.Update =>
                    Resolve
                      .updatePlugins(
                        repo,
                        scanned.requests,
                        Repositories.central
                      )
                      .unit *>
                      Resolve.update(
                        repo,
                        graph,
                        BuildResolvers(graph.resolvers)
                      )
              followed.foldZIO(
                {
                  case error: LoadError =>
                    done(say, CliExit.LoadFailed.code, error.message)
                  case error: ResolveError =>
                    done(say, CliExit.ResolveFailed.code, error.message)
                },
                text => told(say, text) *> done(say, CliExit.Success.code, "")
              )

  def workflow(
      repo: Path,
      graph: ProjectGraph,
      action: String,
      say: Say
  ): IO[ServerError, Unit] =
    workflowAction(action) match
      case None =>
        done(say, CliExit.Usage.code, s"'$action' is not a workflow action")
      case Some(kind) =>
        val effect = kind match
          case WorkflowAction.Check  => Workflow.check(repo, graph)
          case WorkflowAction.Update => Workflow.update(repo, graph)
        effect.foldZIO(
          error => done(say, CliExit.WorkflowFailed.code, error.message),
          file =>
            told(say, file.toString) *> done(
              say,
              CliExit.Success.code,
              ""
            )
        )

  def assembly(
      repo: Path,
      graph: ProjectGraph,
      jdk: ResolvedJdk,
      destination: Option[String],
      say: Say
  ): ZIO[Resolve, ServerError, Unit] =
    placed(repo, destination) match
      case None =>
        done(say, CliExit.CompileFailed.code, "assembly path is not a path")
      case Some(path) =>
        Assemble
          .run(repo, graph, jdk, path)
          .foldZIO(
            error => done(say, CliExit.CompileFailed.code, error.message),
            assembled =>
              pin(repo, assembled.path).foldZIO(
                message => done(say, CliExit.CompileFailed.code, message),
                _ =>
                  told(say, relative(repo, assembled.path)) *> done(
                    say,
                    CliExit.Success.code,
                    ""
                  )
              )
          )

  def compile(
      repo: Path,
      graph: ProjectGraph,
      projects: List[String],
      jdk: ResolvedJdk,
      say: Say
  ): ZIO[Resolve, ServerError, Unit] =
    GraphCompile
      .console(repo, graph, projects, jdk)
      .mapZIO {
        case CompileFeed.Diagnostic(_, _, _, problem) =>
          say(Message.Err(problem.text))
        case CompileFeed.Done(cell) =>
          say(Message.Out(Lines.compiled(repo, cell)))
        case CompileFeed.Failed(error) =>
          say(Message.Err(error.message))
      }
      .runDrain
      .foldZIO(
        {
          case error: GraphCompileError =>
            done(say, CliExit.CompileFailed.code, Cli.compileFailure(error))
          case error: ServerError => ZIO.fail(error)
        },
        _ => done(say, CliExit.Success.code, "")
      )

  def test(
      repo: Path,
      graph: ProjectGraph,
      projects: List[String],
      patterns: List[String],
      quick: Boolean,
      jdk: ResolvedJdk,
      say: Say
  ): ZIO[Resolve, ServerError, Unit] =
    Ref.make(false).flatMap { failed =>
      Ref.make(List.empty[String]).flatMap { missed =>
        val ran =
          for
            cells <- GraphCompile
              .console(repo, graph, projects, jdk)
              .mapError(ModuleTestError.Compile(_))
              .mapZIO { feed =>
                val sent = feed match
                  case CompileFeed.Diagnostic(_, _, _, problem) =>
                    say(Message.Err(problem.text))
                  case CompileFeed.Done(cell) =>
                    say(Message.Out(Lines.compiled(repo, cell)))
                  case CompileFeed.Failed(error) =>
                    missed.update(_ :+ error.message) *> say(
                      Message.Err(error.message)
                    )
                sent.as(compiled(feed))
              }
              .collectSome
              .runCollect
            parsed <- ZIO
              .fromEither(
                patterns.foldLeft[Either[
                  histos.testrun.SuitePatternError,
                  List[SuitePattern]
                ]](Right(Nil)) { (acc, raw) =>
                  acc.flatMap(have => SuitePattern.from(raw).map(have :+ _))
                }
              )
              .mapError(error =>
                ModuleTestError.Compile(
                  GraphCompileError.Target(
                    histos.workflow.CiError.BadName(error.message)
                  )
                )
              )
            _ <- ModuleTest
              .suites(repo, graph, projects, jdk, cells.toList, parsed, quick)
              .foreach {
                case TestFeed.Begin(module, version, platform) =>
                  say(Message.Out(Lines.begin(module, version, platform)))
                case TestFeed.Out(text) => say(Message.Out(text))
                case TestFeed.Err(text) => say(Message.Err(text))
                case TestFeed.Cached(module, version, platform, suite) =>
                  say(
                    Message.Out(Lines.cached(module, version, platform, suite))
                  )
                case TestFeed.Done(cell) =>
                  ZIO.when(failedSuite(cell.status))(failed.set(true)) *>
                    say(Message.Out(Lines.tested(cell))) *>
                    say(Message.Out(""))
              }
            bad <- failed.get
            compileErrors <- missed.get
          yield
            if compileErrors.nonEmpty then
              (CliExit.TestFailed.code, compileErrors.mkString("\n"))
            else if bad then (CliExit.TestFailed.code, "")
            else (CliExit.Success.code, "")
        ran.foldZIO(
          {
            case error: ServerError     => ZIO.fail(error)
            case error: ModuleTestError =>
              missed.get.flatMap { compileErrors =>
                val (code, message) = error match
                  case ModuleTestError.NoneMatched(_)
                      if compileErrors.nonEmpty =>
                    (CliExit.TestFailed.code, compileErrors.mkString("\n"))
                  case other => Cli.testFailure(other)
                done(say, code, message)
              }
          },
          outcome => done(say, outcome._1, outcome._2)
        )
      }
    }

  def cancel(
      runs: Ref[Map[String, Fiber[ServerError, Unit]]],
      id: String,
      say: Say
  ): IO[ServerError, Unit] =
    RunId.from(id) match
      case Left(error)  => done(say, CliExit.TaskFailed.code, error.message)
      case Right(runId) =>
        runs.modify(map => (map.get(id), map - id)).flatMap {
          case Some(fiber) =>
            fiber.interrupt *> told(say, "cancelled") *> done(
              say,
              CliExit.Success.code,
              ""
            )
          case None =>
            done(
              say,
              CliExit.TaskFailed.code,
              TaskError.UnknownRun(runId).message
            )
        }

  def tracked[R](
      runs: Ref[Map[String, Fiber[ServerError, Unit]]],
      id: String,
      say: Say
  )(body: ZIO[R, ServerError, Unit]): ZIO[R, ServerError, Unit] =
    body.fork.flatMap { fiber =>
      runs.update(_ + (id -> fiber)) *>
        fiber.join.foldCauseZIO(
          cause =>
            runs.update(_ - id) *>
              (if cause.isInterruptedOnly then
                 done(say, CliExit.Interrupted.code, "")
               else
                 cause.failureOrCause match
                   case Left(error) => ZIO.fail(error)
                   case Right(_)    =>
                     done(say, CliExit.TaskFailed.code, "the run failed")),
          _ => runs.update(_ - id)
        )
    }

  private def plugin(
      plugins: PluginRuntime,
      id: PluginTaskId,
      say: Say
  ): ZIO[ContentStore, ServerError, Unit] =
    ZIO.serviceWithZIO[ContentStore] { store =>
      plugins
        .run(id, PluginArgs.empty)
        .provide(
          ZLayer.succeed(ContentStore.actions(store)),
          RemoteCache.off
        )
        .foldZIO(
          error => done(say, CliExit.TaskFailed.code, error.message),
          result =>
            told(say, result.text) *> done(say, CliExit.Success.code, "")
        )
    }

  private def chosen(
      platform: Option[String]
  ): Either[String, Option[Platform]] =
    platform match
      case None      => Right(None)
      case Some(raw) =>
        Platform.from(raw) match
          case Left(error)  => Left(error.message)
          case Right(found) => Right(Some(found))

  private def resolveAction(raw: String): Option[histos.cli.ResolveAction] =
    raw match
      case "verify" => Some(histos.cli.ResolveAction.Verify)
      case "update" => Some(histos.cli.ResolveAction.Update)
      case "check"  => Some(histos.cli.ResolveAction.Check)
      case _        => None

  private def workflowAction(raw: String): Option[WorkflowAction] =
    raw match
      case "check"  => Some(WorkflowAction.Check)
      case "update" => Some(WorkflowAction.Update)
      case _        => None

  private def programCode(error: histos.program.ProgramError): Int =
    error match
      case histos.program.ProgramError.Compile(
            GraphCompileError.Lock(_) | GraphCompileError.LockRead(_)
          ) =>
        CliExit.ResolveFailed.code
      case histos.program.ProgramError.Lock(_) |
          histos.program.ProgramError.LockRead(_) =>
        CliExit.ResolveFailed.code
      case histos.program.ProgramError.Compile(_) =>
        CliExit.CompileFailed.code
      case _ =>
        CliExit.TaskFailed.code

  private def placed(repo: Path, raw: Option[String]): Option[Path] =
    raw match
      case None       => Some(repo.resolve("dist").resolve("histos.jar"))
      case Some(text) =>
        try
          val path = Path.of(text)
          Some(if path.isAbsolute then path else repo.resolve(path))
        catch case _: java.nio.file.InvalidPathException => None

  private def placed(repo: Path, raw: String): Option[Path] =
    try
      val path = Path.of(raw)
      Some(if path.isAbsolute then path else repo.resolve(path))
    catch case _: java.nio.file.InvalidPathException => None

  private def relative(repo: Path, path: Path): String =
    val base = repo.toAbsolutePath.normalize
    val full = path.toAbsolutePath.normalize
    (if full.startsWith(base) then base.relativize(full) else full).toString
      .replace('\\', '/')

  /** The launcher verifies this checksum of `dist/histos.jar`. */
  private def pin(repo: Path, jar: Path): IO[String, Unit] =
    val pinned =
      repo.resolve("dist").resolve("histos.jar").toAbsolutePath.normalize
    if jar.toAbsolutePath.normalize != pinned then ZIO.unit
    else
      ZIO
        .attempt {
          val sha = Sha256.of(Chunk.fromArray(Files.readAllBytes(jar))).value
          val dir = repo.resolve(".histos")
          Files.createDirectories(dir)
          Files.writeString(dir.resolve("assembly.sha256"), sha + "\n")
          ()
        }
        .mapError(_ =>
          s"could not write ${repo.resolve(".histos").resolve("assembly.sha256")}"
        )

  private def compiled(feed: CompileFeed): Option[histos.zinc.CompiledCell] =
    feed match
      case CompileFeed.Done(cell)             => Some(cell)
      case CompileFeed.Diagnostic(_, _, _, _) => None
      case CompileFeed.Failed(_)              => None

  private def failedSuite(status: TestStatus): Boolean =
    status match
      case TestStatus.Failed(_, _, _) => true
      case TestStatus.NoTests | TestStatus.Unmatched |
          TestStatus.Passed(_, _) =>
        false

  private def told(say: Say, text: String): IO[ServerError, Unit] =
    if text.isEmpty then ZIO.unit else emit(say, Message.Out(text))

  private def done(
      say: Say,
      code: Int,
      err: String
  ): IO[ServerError, Unit] =
    emit(say, Message.Exit(code, err))

  private def emit(
      say: Say,
      message: Message
  ): IO[ServerError, Unit] =
    say(message)

  private object Quiet extends PluginRuntime:
    def run(
        id: PluginTaskId,
        args: PluginArgs
    ): ZIO[
      histos.schedule.ActionCache & RemoteCache,
      PluginRunError,
      histos.plugin.PluginOutput
    ] =
      ZIO.fail(PluginRunError.Unknown(id))

    def bind: ZIO[
      histos.schedule.ActionCache & RemoteCache,
      Nothing,
      Map[TaskId, IO[TaskError, TaskResult]]
    ] =
      ZIO.succeed(Map.empty)

    def links: List[PluginLink] = Nil
end Calls
