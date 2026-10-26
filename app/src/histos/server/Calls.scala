package histos.server

import histos.jdk.JdkRuntime
import histos.assembly.Assemble
import histos.cache.ContentStore
import histos.cli.{Cli, CliExit, CompileConsole, GraphText, Lines}
import histos.report.{Diagnostic, Observation, Stored}
import histos.testrun.{ModuleTest, ModuleTestError, SuitePattern, TestFeed, TestRun, TestStatus}
import histos.preview.Plan
import histos.zinc.{CompileFeed, GraphCompile, GraphCompileError}
import histos.jdk.{Machine, Os}
import histos.jdk.Sha256
import histos.load.{LoadError, Loader, PluginLink, PluginRuntime}
import histos.load.PluginFault
import histos.plugin.{PluginArgs, PluginRunError, PluginTaskId}
import histos.publish.{LauncherPin, LauncherScript, PublishRun, ScriptError}
import histos.resolve.{BuildResolvers, DependencyLock, PluginUpdate, Repositories, Resolve, ResolveError}
import histos.schedule.RemoteCache
import histos.task.{RunId, TaskError, TaskId, TaskResult}
import histos.native.NativeOrigin
import histos.cli.ResolveAction
import histos.toolchain.{Runtimes, Survey}
import histos.version.VersionTask
import histos.workflow.{Workflow, WorkflowAction}
import histos.{Platform, PlatformError, ProjectGraph}

import zio.json.*
import zio.json.ast.Json
import zio.{Chunk, Exit, Fiber, IO, Promise, Random, Ref, Scope, UIO, ZIO, ZLayer}

import java.io.OutputStream
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicReference

/** One CLI message, sent as a frame or kept for a tool result. */
final class Say private (
    private val send: Message => IO[ServerError, Unit],
    private val minted: AtomicReference[Option[String]],
    private val kept: AtomicReference[Option[Observation]],
    private val noted: AtomicReference[Chunk[Diagnostic]],
):
  def this(send: Message => IO[ServerError, Unit]) =
    this(
      send,
      new AtomicReference[Option[String]](None),
      new AtomicReference[Option[Observation]](None),
      new AtomicReference[Chunk[Diagnostic]](Chunk.empty),
    )

  def apply(message: Message): IO[ServerError, Unit] = send(message)

  /** The id [[Calls.tracked]] minted. Not read back from the transcript. */
  def mint(id: histos.task.RunId): UIO[Unit] =
    ZIO.succeed(minted.set(Some(id.value))).unit

  def keep(observation: Observation): UIO[Unit] =
    ZIO.succeed(kept.set(Some(observation))).unit

  def forget: UIO[Unit] =
    ZIO.succeed(kept.set(None)).unit

  def run: UIO[Option[String]] = ZIO.succeed(minted.get)

  def observation: UIO[Option[Observation]] = ZIO.succeed(kept.get)

  /** The chunk read once at the end of compile or test. Cells append on the console ref, not here. */
  def note(found: Chunk[Diagnostic]): UIO[Unit] =
    ZIO.succeed(noted.set(found)).unit

  def diagnostics: UIO[Chunk[Diagnostic]] = ZIO.succeed(noted.get)

  /** Inner watch iterations keep their text and do not end the session. */
  def dropExit: Say =
    new Say(
      {
        case Message.Exit(_, err) =>
          if err.isEmpty then ZIO.unit else send(Message.Err(err))
        case other => send(other)
      },
      minted,
      kept,
      noted,
    )
end Say

object Say:
  def frames(stream: OutputStream, socket: Path): Say =
    val minted = new AtomicReference[Option[String]](None)
    val kept   = new AtomicReference[Option[Observation]](None)
    val noted  = new AtomicReference[Chunk[Diagnostic]](Chunk.empty)
    new Say(
      message =>
        histos.bsp.Frame
          .write(stream, wire(message, minted.get, kept.get, noted.get))
          .mapError(_ => ServerError.Io(ServerIo.Write(socket))),
      minted,
      kept,
      noted,
    )

  def record(feeds: Ref[Chunk[Feed]]): Say =
    new Say({
      case Message.Out(text) =>
        feeds.update(_ :+ Feed.Out(text)) *> histos.mcp.Notice.out(text)
      case Message.Err(text) =>
        feeds.update(_ :+ Feed.Err(text)) *> histos.mcp.Notice.err(text)
      case Message.Exit(code, err) => feeds.update(_ :+ Feed.Exit(code, err))
      case _                       => ZIO.unit
    })

  /** Extra exit fields stay inside the `Exit` object so the old decoder still matches.
    */
  private[histos] def wire(
      message: Message,
      run: Option[String],
      observation: Option[Observation],
      diagnostics: Chunk[Diagnostic],
  ): String =
    message match
      case Message.Exit(code, err) if run.isDefined || observation.isDefined || diagnostics.nonEmpty =>
        val base = Chunk[(String, Json)](
          "code" -> Json.Num(code),
          "err"  -> Json.Str(err),
        )
        val withRun = run.fold(base)(id => base :+ ("run" -> Json.Str(id)))
        val withObs = observation.fold(withRun) { found =>
          found.toJsonAST match
            case Right(json) => withRun :+ ("observation" -> json)
            case Left(_)     => withRun
        }
        val fields = diagnostics.toJsonAST match
          case Right(json) => withObs :+ ("diagnostics" -> json)
          case Left(_)     => withObs
        Json.Obj(Chunk("Exit" -> Json.Obj(fields.toList*))).toJson
      case other => other.toJson
end Say

enum WorkflowFault:
  case Files(error: histos.workflow.CiError)
  case Scripts(error: ScriptError)

  def message: String = this match
    case Files(error)   => error.message
    case Scripts(error) => error.message
end WorkflowFault

/** One graph command, written as the CLI would print it. */
object Calls:
  val quiet: PluginRuntime = Quiet

  def idleMachine: Machine =
    Machine(
      Map.empty,
      ".",
      Os.Other,
      histos.toolchain.Architecture.Unsupported("fixture"),
      Path.of(".histos"),
      Path.of("."),
      None,
      NativeOrigin.Central,
    )

  def graph(
      graph: ProjectGraph,
      plugins: PluginRuntime,
      say: Say,
  ): IO[ServerError, Unit] =
    told(say, GraphText.render(graph, plugins.links)) *> done(
      say,
      CliExit.Success.code,
      "",
    )

  def publish(
      repo: Path,
      graph: ProjectGraph,
      machine: Machine,
      jdk: JdkRuntime,
      repository: String,
      projects: List[String],
      say: Say,
  ): ZIO[Resolve, ServerError, Unit] =
    CompileConsole.open(repo).flatMap { console =>
      PublishRun
        .run(
          repo,
          graph,
          projects,
          jdk,
          machine.env,
          repository,
          feed => speak(say, console, feed).ignore,
        )
        .foldZIO(
          failure =>
            console.announce(text => say(Message.Out(text))) *>
              finishNoted(
                repo,
                say,
                console,
                List(console.observation),
                CliExit.PublishFailed.code,
                failure.message,
              ),
          report =>
            console.announce(text => say(Message.Out(text))) *>
              told(say, report.lines.mkString("\n")) *> finishNoted(
                repo,
                say,
                console,
                List(console.observation),
                CliExit.Success.code,
                "",
              ),
        )
    }

  def run(
      repo: Path,
      graph: ProjectGraph,
      jdk: JdkRuntime,
      machine: Machine,
      id: String,
      platform: Option[String],
      say: Say,
  ): ZIO[Resolve, ServerError, Unit] =
    chosen(platform) match
      case Left(error)   => done(say, CliExit.TaskFailed.code, error.message)
      case Right(choice) =>
        CompileConsole
          .open(repo)
          .flatMap: console =>
            Ref
              .make(0)
              .zip(Runtimes.read(machine))
              .flatMap: (status, runtimes) =>
                histos.program.Program
                  .console(
                    repo,
                    graph,
                    id,
                    jdk,
                    runtimes,
                    choice,
                    feed => speak(say, console, feed).ignore,
                    console.announce(text => say(Message.Out(text)).ignore),
                  )
                  .foreach:
                    case histos.program.ProgramFeed
                          .Begin(module, version, target) =>
                      told(
                        say,
                        s"${module.value} ${version.value} ${target.token}",
                      )
                    case histos.program.ProgramFeed.Out(text) => told(say, text)
                    case histos.program.ProgramFeed.Err(text) =>
                      emit(say, Message.Err(text))
                    case histos.program.ProgramFeed.Exited(_, _, _, code) =>
                      ZIO.when(code != 0)(status.set(code)).unit
                  .foldZIO(
                    {
                      case error: histos.program.ProgramError =>
                        val announced = error match
                          case histos.program.ProgramError.Compile(_) =>
                            console.announce(text => say(Message.Out(text)))
                          case _ => ZIO.unit
                        announced *> finishNoted(
                          repo,
                          say,
                          console,
                          List(console.observation),
                          programCode(error),
                          error.message,
                        )
                      case error: ServerError => ZIO.fail(error)
                    },
                    _ =>
                      status.get.flatMap: code =>
                        finishNoted(
                          repo,
                          say,
                          console,
                          List(console.observation),
                          if code == 0 then CliExit.Success.code else code,
                          "",
                        ),
                  )

  def task(
      repo: Path,
      graph: ProjectGraph,
      plugins: PluginRuntime,
      machine: Machine,
      id: String,
      args: PluginArgs,
      say: Say,
  ): ZIO[Resolve & histos.load.Loader & ContentStore, ServerError, Unit] =
    TaskId.from(id) match
      case Left(error)                             => done(say, CliExit.TaskFailed.code, error.message)
      case Right(found) if found == TaskId.Inspect =>
        if args != PluginArgs.empty then
          done(
            say,
            CliExit.TaskFailed.code,
            s"run ${found.value} does not take arguments",
          )
        else
          told(say, graph.projects.map(_.name.value).mkString(",")) *> done(
            say,
            CliExit.Success.code,
            "",
          )
      case Right(found) if found == TaskId.Versions =>
        if args != PluginArgs.empty then
          done(
            say,
            CliExit.TaskFailed.code,
            s"run ${found.value} does not take arguments",
          )
        else
          VersionTask
            .fromRepo(graph, repo, machine.env)
            .foldZIO(
              error => done(say, CliExit.TaskFailed.code, error.message),
              result =>
                told(say, result.render) *> done(
                  say,
                  CliExit.Success.code,
                  "",
                ),
            )
      case Right(_) =>
        PluginTaskId.parse(id) match
          case Left(error) =>
            done(say, CliExit.TaskFailed.code, error.message)
          case Right(taskId) => plugin(plugins, taskId, args, say)

  def toolchain(
      machine: Machine,
      graph: ProjectGraph,
      say: Say,
  ): IO[ServerError, Unit] =
    Survey
      .read(machine, graph)
      .foldZIO(
        error =>
          told(say, Survey.shown(error)) *> done(
            say,
            CliExit.ToolchainFailed.code,
            error.message,
          ),
        found =>
          told(say, Survey.text(found)) *> done(
            say,
            CliExit.Success.code,
            "",
          ),
      )

  def resolve(
      repo: Path,
      apiJar: Path,
      action: String,
      say: Say,
      native: histos.native.NativeOrigin,
  ): ZIO[Resolve & Loader, ServerError, Unit] =
    resolveAction(action) match
      case None =>
        done(say, CliExit.Usage.code, s"'$action' is not a resolve action")
      case Some(kind) =>
        resolveWork(repo, apiJar, kind, native).foldZIO(
          {
            case error: LoadError =>
              done(say, CliExit.LoadFailed.code, error.message)
            case error: ResolveError =>
              done(say, CliExit.ResolveFailed.code, error.message)
          },
          text => told(say, text) *> done(say, CliExit.Success.code, ""),
        )

  /** Stage plugin cells, load that generation, then solve and commit once. Check and verify load the locked generation.
    * The adopted graph is not the update input.
    */
  def resolveWork(
      repo: Path,
      apiJar: Path,
      action: ResolveAction,
      native: histos.native.NativeOrigin,
  ): ZIO[Loader & Resolve, LoadError | ResolveError, String] =
    ZIO.serviceWithZIO[Loader](_.scan(repo)).flatMap { scanned =>
      val followed: ZIO[Loader & Resolve, LoadError | ResolveError, String] =
        action match
          case ResolveAction.Update =>
            for
              plugins <- Resolve.planPlugins(
                scanned.requests,
                Repositories.central,
              )
              prior <- Resolve.readLock(repo).catchSome { case ResolveError.NoLock(_) =>
                ZIO.succeed(DependencyLock(Nil))
              }
              built <- generation(
                _.loadLocked(repo, apiJar, prior.copy(plugins = plugins))
              )
              updated <- Resolve.update(
                repo,
                built.graph,
                BuildResolvers(built.graph.resolvers),
                native,
                PluginUpdate.Replace(plugins),
              )
            yield updated
          case ResolveAction.Check =>
            for
              _ <- Resolve.requirePlugins(repo, scanned.requests)
              _ <- Resolve.checkPlugins(
                repo,
                scanned.requests,
                Repositories.central,
              )
              built <- generation(_.load(repo, apiJar))
              text  <- Resolve.check(
                repo,
                built.graph,
                BuildResolvers(built.graph.resolvers),
                native,
              )
            yield text
          case ResolveAction.Verify =>
            for
              _     <- Resolve.requirePlugins(repo, scanned.requests)
              built <- generation(_.load(repo, apiJar))
              text  <- Resolve.verify(
                repo,
                built.graph,
                BuildResolvers(built.graph.resolvers),
                native,
              )
            yield text
      followed.flatMap(text =>
        Resolve
          .readLock(repo)
          .map(lock => DependencyLock.withEvictions(text, lock))
      )
    }

  private def generation[A](
      load: Loader => ZIO[Scope, LoadError, A]
  ): ZIO[Loader, LoadError, A] =
    ZIO.serviceWithZIO[Loader] { loader =>
      for
        scope  <- Scope.make
        result <- scope.extend(load(loader)).ensuring(scope.close(Exit.unit))
      yield result
    }

  def workflow(
      repo: Path,
      graph: ProjectGraph,
      action: String,
      say: Say,
  ): IO[ServerError, Unit] =
    workflowAction(action) match
      case None =>
        done(say, CliExit.Usage.code, s"'$action' is not a workflow action")
      case Some(kind) =>
        workflowWork(repo, graph, kind).foldZIO(
          fault => done(say, CliExit.WorkflowFailed.code, fault.message),
          file =>
            told(say, file.toString) *> done(
              say,
              CliExit.Success.code,
              "",
            ),
        )

  /** Workflow files and launcher scripts. The pin is not advanced. */
  def workflowWork(
      repo: Path,
      graph: ProjectGraph,
      action: WorkflowAction,
  ): IO[WorkflowFault, Path] =
    val rendered = action match
      case WorkflowAction.Check =>
        Workflow.check(repo, graph).mapError(WorkflowFault.Files(_))
      case WorkflowAction.Update =>
        Workflow.update(repo, graph).mapError(WorkflowFault.Files(_))
    val scripts = action match
      case WorkflowAction.Check =>
        LauncherScript.check(repo, graph).mapError(WorkflowFault.Scripts(_))
      case WorkflowAction.Update =>
        LauncherScript.update(repo, graph).mapError(WorkflowFault.Scripts(_))
    rendered *> scripts.as(Workflow.path(repo))

  def version(text: String, say: Say): IO[ServerError, Unit] =
    told(say, text) *> done(say, CliExit.Success.code, "")

  def cachePath(say: Say): ZIO[ContentStore, ServerError, Unit] =
    ZIO.serviceWithZIO[ContentStore] { store =>
      told(say, store.root.toString) *> done(say, CliExit.Success.code, "")
    }

  def cacheList(say: Say): ZIO[ContentStore, ServerError, Unit] =
    ZIO
      .serviceWithZIO[ContentStore](_.list)
      .foldZIO(
        error => done(say, CliExit.LoadFailed.code, error.message),
        entries =>
          val text =
            if entries.isEmpty then ""
            else
              entries
                .map(entry => s"${entry.digest.value} ${entry.bytes}")
                .mkString("\n")
          told(say, text) *> done(say, CliExit.Success.code, ""),
      )

  def cacheClean(
      dryRun: Boolean,
      say: Say,
  ): ZIO[ContentStore, ServerError, Unit] =
    ZIO
      .serviceWithZIO[ContentStore]: store =>
        zio.Clock.instant
          .flatMap(now => store.clean(now, ContentStore.Retention, dryRun))
      .foldZIO(
        error => done(say, CliExit.LoadFailed.code, error.message),
        entries =>
          val verb = if dryRun then "would remove" else "removed"
          val text =
            if entries.isEmpty then s"$verb nothing"
            else
              entries
                .map(entry => s"$verb ${entry.digest.value}")
                .mkString("\n")
          told(say, text) *> done(say, CliExit.Success.code, ""),
      )

  def launcherUpdate(repo: Path, say: Say): IO[ServerError, Unit] =
    LauncherPin
      .update(repo)
      .foldZIO(
        error => done(say, CliExit.ResolveFailed.code, error.message),
        pin =>
          told(say, s"${pin.revision} ${pin.sha256.value}") *> done(
            say,
            CliExit.Success.code,
            "",
          ),
      )

  def assembly(
      repo: Path,
      graph: ProjectGraph,
      jdk: JdkRuntime,
      machine: Machine,
      destination: Option[String],
      say: Say,
  ): ZIO[Resolve, ServerError, Unit] =
    placed(repo, destination) match
      case None =>
        done(say, CliExit.CompileFailed.code, "assembly path is not a path")
      case Some(path) =>
        CompileConsole.open(repo).flatMap { console =>
          Assemble
            .run(
              repo,
              graph,
              jdk,
              NativeOrigin.read(machine.env),
              path,
              feed => speak(say, console, feed).ignore,
            )
            .foldZIO(
              error =>
                console.announce(text => say(Message.Out(text))) *>
                  finishNoted(
                    repo,
                    say,
                    console,
                    List(console.observation),
                    CliExit.CompileFailed.code,
                    error.message,
                  ),
              assembled =>
                console.announce(text => say(Message.Out(text))) *>
                  pin(repo, assembled.path).foldZIO(
                    message =>
                      finishNoted(
                        repo,
                        say,
                        console,
                        List(console.observation),
                        CliExit.CompileFailed.code,
                        message,
                      ),
                    _ =>
                      told(say, relative(repo, assembled.path)) *>
                        finishNoted(
                          repo,
                          say,
                          console,
                          List(console.observation),
                          CliExit.Success.code,
                          "",
                        ),
                  ),
            )
        }

  def compile(
      repo: Path,
      graph: ProjectGraph,
      projects: List[String],
      jdk: JdkRuntime,
      machine: Machine,
      say: Say,
  ): ZIO[Resolve, ServerError, Unit] =
    CompileConsole.open(repo).flatMap { console =>
      GraphCompile
        .strict(
          GraphCompile
            .console(
              repo,
              graph,
              projects,
              jdk,
              NativeOrigin.read(machine.env),
              None,
            )
        )
        .mapZIO(feed => speak(say, console, feed))
        .runDrain
        .foldZIO(
          {
            case error: GraphCompileError =>
              console.announce(text => say(Message.Out(text))) *>
                finishNoted(
                  repo,
                  say,
                  console,
                  List(console.observation),
                  CliExit.CompileFailed.code,
                  Cli.compileFailure(error),
                )
            case error: ServerError => ZIO.fail(error)
          },
          _ =>
            console.announce(text => say(Message.Out(text))) *>
              finishNoted(
                repo,
                say,
                console,
                List(console.observation),
                CliExit.Success.code,
                "",
              ),
        )
    }

  def previewCompile(
      repo: Path,
      graph: ProjectGraph,
      projects: List[String],
      say: Say,
  ): IO[ServerError, Unit] =
    Plan
      .show(repo, graph, projects)
      .foldZIO(
        error => done(say, CliExit.CompileFailed.code, error.message),
        text => told(say, text) *> done(say, CliExit.Success.code, ""),
      )

  def previewTest(
      repo: Path,
      graph: ProjectGraph,
      projects: List[String],
      say: Say,
  ): IO[ServerError, Unit] =
    Plan
      .test(repo, graph, projects)
      .foldZIO(
        error => done(say, CliExit.TestFailed.code, error.message),
        plan =>
          told(say, Plan.renderTest(repo, plan)) *> done(
            say,
            CliExit.Success.code,
            "",
          ),
      )

  def previewPublish(
      repo: Path,
      graph: ProjectGraph,
      projects: List[String],
      env: Map[String, String],
      say: Say,
  ): IO[ServerError, Unit] =
    Plan
      .publish(repo, graph, projects, env)
      .foldZIO(
        error => done(say, CliExit.PublishFailed.code, error.message),
        plan =>
          told(say, Plan.renderPublish(plan)) *> done(
            say,
            CliExit.Success.code,
            "",
          ),
      )

  /** Empty names do not read `graph`. */
  def previewClean(
      repo: Path,
      graph: ProjectGraph,
      projects: List[String],
      say: Say,
  ): IO[ServerError, Unit] =
    Plan
      .clean(repo, graph, projects)
      .foldZIO(
        error => done(say, CliExit.TaskFailed.code, error.message),
        plan =>
          told(say, Plan.renderClean(repo, plan)) *> done(
            say,
            CliExit.Success.code,
            "",
          ),
      )

  def previewCleanAll(repo: Path, say: Say): IO[ServerError, Unit] =
    val plan = Plan.cleanAll(repo)
    told(say, Plan.renderClean(repo, plan)) *> done(
      say,
      CliExit.Success.code,
      "",
    )

  def previewCleanFull(repo: Path, say: Say): IO[ServerError, Unit] =
    val plan = Plan.cleanFull(repo)
    told(say, Plan.renderClean(repo, plan)) *> done(
      say,
      CliExit.Success.code,
      "",
    )

  def previewRun(
      graph: ProjectGraph,
      id: String,
      platform: Option[String],
      say: Say,
  ): IO[ServerError, Unit] =
    chosen(platform) match
      case Left(error)   => done(say, CliExit.TaskFailed.code, error.message)
      case Right(choice) =>
        Plan.run(graph, id, choice) match
          case Left(error) => done(say, CliExit.TaskFailed.code, error.message)
          case Right(plan) =>
            told(say, Plan.renderRun(plan)) *> done(
              say,
              CliExit.Success.code,
              "",
            )

  /** No names wipe every product root and do not read `graph`. */
  def clean(
      repo: Path,
      graph: ProjectGraph,
      projects: List[String],
      say: Say,
  ): IO[ServerError, Unit] =
    val removed =
      if projects.isEmpty then histos.clean.Products.wipe(repo).as(histos.clean.Products.line(Nil))
      else
        histos.clean.Products
          .named(repo, graph, projects)
          .map(histos.clean.Products.line)
    removed.foldZIO(
      error => done(say, CliExit.TaskFailed.code, histos.clean.Products.fault(error)),
      line => told(say, line) *> done(say, CliExit.Success.code, ""),
    )

  def wipe(repo: Path, say: Say): IO[ServerError, Unit] =
    histos.clean.Products
      .wipe(repo)
      .foldZIO(
        error => done(say, CliExit.TaskFailed.code, error.message),
        _ =>
          told(say, histos.clean.Products.line(Nil)) *> done(
            say,
            CliExit.Success.code,
            "",
          ),
      )

  /** Products go first. A store failure is exit 3 and does not claim success.
    */
  def cleanFull(repo: Path, say: Say): ZIO[ContentStore, ServerError, Unit] =
    histos.clean.Products
      .wipe(repo)
      .foldZIO(
        error => done(say, CliExit.TaskFailed.code, error.message),
        _ =>
          ZIO
            .serviceWithZIO[ContentStore]: store =>
              zio.Clock.instant.flatMap(now => store.forget(histos.cache.CacheProject.of(repo), now))
            .foldZIO(
              error => done(say, CliExit.LoadFailed.code, error.message),
              entries =>
                told(say, histos.clean.Products.forgotten(entries)) *> done(
                  say,
                  CliExit.Success.code,
                  "",
                ),
            ),
      )

  def test(
      repo: Path,
      graph: ProjectGraph,
      projects: List[String],
      patterns: List[String],
      quick: Boolean,
      jdk: JdkRuntime,
      machine: Machine,
      say: Say,
  ): ZIO[Resolve, ServerError, Unit] =
    CompileConsole.open(repo).flatMap { console =>
      CompileConsole.notes.flatMap { notes =>
        Ref.make(false).flatMap { failed =>
          Ref.make(List.empty[String]).flatMap { missed =>
            val ran =
              for
                runtimes <- Runtimes.read(machine)
                parsed   <- ZIO.fromEither(suitePatterns(patterns))
                _        <- ZIO
                  .fromEither(histos.select.Selection.select(graph, projects))
                  .mapError(ModuleTestError.Target(_))
                lock  <- Resolve.readLock(repo).mapError(ModuleTestError.Lock(_))
                cells <- GraphCompile
                  .console(
                    histos.zinc
                      .CompileSession(repo, graph, jdk, runtimes.native, lock),
                    projects,
                    None,
                  )
                  .mapError(ModuleTestError.Compile(_))
                  .mapZIO { feed =>
                    val sent = feed match
                      case CompileFeed.Failed(error) =>
                        missed.update(_ :+ error.message) *>
                          speak(say, console, feed)
                      case _ => speak(say, console, feed)
                    sent.as(compiled(feed))
                  }
                  .collectSome
                  .runCollect
                _ <- console.announce(text => say(Message.Out(text)))
                _ <- ModuleTest
                  .suitesLocked(
                    TestRun(
                      repo,
                      graph,
                      projects,
                      jdk,
                      runtimes,
                      parsed,
                      quick,
                      (module, version, platform, facts) =>
                        notes(module, version, platform, facts)
                          .flatMap(line => say(Message.Out(line)).ignore),
                    ),
                    cells.toList,
                    lock,
                  )
                  .foreach {
                    case TestFeed.Begin(module, version, platform) =>
                      say(Message.Out(Lines.begin(module, version, platform)))
                    case TestFeed.Out(text)                                => say(Message.Out(text))
                    case TestFeed.Err(text)                                => say(Message.Err(text))
                    case TestFeed.Cached(module, version, platform, suite) =>
                      say(
                        Message
                          .Out(Lines.cached(module, version, platform, suite))
                      )
                    case TestFeed.Done(cell) =>
                      ZIO.when(failedSuite(cell.status))(failed.set(true)) *>
                        say(Message.Out(Lines.tested(cell))) *>
                        say(Message.Out(""))
                  }
                bad           <- failed.get
                compileErrors <- missed.get
              yield
                if compileErrors.nonEmpty then (CliExit.TestFailed.code, compileErrors.mkString("\n"))
                else if bad then (CliExit.TestFailed.code, "")
                else (CliExit.Success.code, "")
            ran.foldZIO(
              {
                case error: ServerError     => ZIO.fail(error)
                case error: ModuleTestError =>
                  console.announce(text => say(Message.Out(text))) *>
                    notes.announce(text => say(Message.Out(text))) *>
                    missed.get.flatMap { compileErrors =>
                      val (code, message) = error match
                        case ModuleTestError.NoneMatched(_) if compileErrors.nonEmpty =>
                          (
                            CliExit.TestFailed.code,
                            compileErrors.mkString("\n"),
                          )
                        case other => Cli.testFailure(other)
                      finishNoted(
                        repo,
                        say,
                        console,
                        List(console.observation, notes.observation),
                        code,
                        message,
                      )
                    }
              },
              outcome =>
                notes.announce(text => say(Message.Out(text))) *>
                  finishNoted(
                    repo,
                    say,
                    console,
                    List(console.observation, notes.observation),
                    outcome._1,
                    outcome._2,
                  ),
            )
          }
        }
      }
    }

  def cancel(
      runs: Ref[Map[RunId, Fiber[ServerError, Unit]]],
      id: String,
      say: Say,
  ): IO[ServerError, Unit] =
    RunId.from(id) match
      case Left(error)  => done(say, CliExit.TaskFailed.code, error.message)
      case Right(runId) =>
        runs.modify(map => (map.get(runId), map - runId)).flatMap {
          case None =>
            done(
              say,
              CliExit.TaskFailed.code,
              TaskError.UnknownRun(runId).message,
            )
          case Some(fiber) =>
            fiber.interrupt.flatMap { exit =>
              if exit.isInterrupted then told(say, "cancelled") *> done(say, CliExit.Success.code, "")
              else
                done(
                  say,
                  CliExit.TaskFailed.code,
                  TaskError.UnknownRun(runId).message,
                )
            }
        }

  /** The fiber is in the map before `run <id>` is sent, and the body starts after that line. A second call gets its own
    * id.
    */
  def tracked[R](
      runs: Ref[Map[RunId, Fiber[ServerError, Unit]]],
      say: Say,
  )(body: ZIO[R, ServerError, Unit]): ZIO[R, ServerError, Unit] =
    for
      uuid <- Random.nextUUID
      runId = RunId.trusted(uuid.toString)
      go    <- Promise.make[Nothing, Unit]
      fiber <- (go.await *> body).fork
      _     <- runs.update(_ + (runId -> fiber))
      _     <- (say.mint(runId) *> told(say, s"run ${runId.value}")).tapError(_ =>
        runs.update(_ - runId) *> fiber.interrupt
      )
      _ <- go.succeed(())
      _ <- fiber.join.foldCauseZIO(
        cause =>
          runs.update(_ - runId) *>
            (if cause.isInterruptedOnly then done(say, CliExit.Interrupted.code, "")
             else
               cause.failureOrCause match
                 case Left(error) => ZIO.fail(error)
                 case Right(_)    =>
                   done(say, CliExit.TaskFailed.code, "the run failed")),
        _ => runs.update(_ - runId),
      )
    yield ()

  private def plugin(
      plugins: PluginRuntime,
      id: PluginTaskId,
      args: PluginArgs,
      say: Say,
  ): ZIO[ContentStore & Resolve, ServerError, Unit] =
    ZIO.serviceWithZIO[ContentStore] { store =>
      plugins
        .run(id, args)
        .provideSome[Resolve](
          ZLayer.succeed(ContentStore.actions(store)),
          RemoteCache.off,
        )
        .foldZIO(
          error => done(say, CliExit.TaskFailed.code, error.message),
          result => told(say, result.text) *> done(say, CliExit.Success.code, ""),
        )
    }

  private def chosen(
      platform: Option[String]
  ): Either[PlatformError, Option[Platform]] =
    platform match
      case None      => Right(None)
      case Some(raw) => Platform.from(raw).map(Some(_))

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
      case histos.program.ProgramError.Lock(_) | histos.program.ProgramError.LockRead(_) =>
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
        .mapError(_ => s"could not write ${repo.resolve(".histos").resolve("assembly.sha256")}")

  private def speak(
      say: Say,
      console: CompileConsole.Session,
      feed: CompileFeed,
  ): IO[ServerError, Unit] =
    console.hear(feed).flatMap { lines =>
      CompileConsole.show(
        lines,
        text => say(Message.Out(text)),
        text => say(Message.Err(text)),
      )
    }

  private def compiled(feed: CompileFeed): Option[histos.zinc.CompiledCell] =
    feed match
      case CompileFeed.Done(cell)             => Some(cell)
      case CompileFeed.Began(_, _, _)         => None
      case CompileFeed.Diagnostic(_, _, _, _) => None
      case CompileFeed.Failed(_)              => None

  private[histos] def suitePatterns(
      raw: List[String]
  ): Either[ModuleTestError, List[SuitePattern]] =
    raw.foldLeft[Either[ModuleTestError, List[SuitePattern]]](Right(Nil)): (acc, item) =>
      for
        have    <- acc
        pattern <- SuitePattern.from(item).left.map(ModuleTestError.Pattern(_))
      yield have :+ pattern

  private def failedSuite(status: TestStatus): Boolean =
    status match
      case TestStatus.Failed(_)                                          => true
      case TestStatus.NoTests | TestStatus.Unmatched | TestStatus.Passed =>
        false

  def tell(say: Say, text: String): IO[ServerError, Unit] =
    told(say, text)

  def finish(say: Say, code: Int, err: String): IO[ServerError, Unit] =
    done(say, code, err)

  private def told(say: Say, text: String): IO[ServerError, Unit] =
    if text.isEmpty then ZIO.unit else emit(say, Message.Out(text))

  private def done(
      say: Say,
      code: Int,
      err: String,
  ): IO[ServerError, Unit] =
    emit(say, Message.Exit(code, err))

  /** Compile and test read the console once. The exit frame then carries that chunk. */
  private[histos] def finishNoted(
      repo: Path,
      say: Say,
      console: CompileConsole.Session,
      parts: List[UIO[Option[Observation]]],
      code: Int,
      err: String,
  ): IO[ServerError, Unit] =
    console.diagnostics.flatMap(say.note) *>
      finishObserved(repo, say, parts, code, err)

  /** Compile, test, run, assembly, and publish replace the stored tally. */
  private def finishObserved(
      repo: Path,
      say: Say,
      parts: List[UIO[Option[Observation]]],
      code: Int,
      err: String,
  ): IO[ServerError, Unit] =
    ZIO.foreach(parts)(identity).flatMap { found =>
      val observation = Observation.combine(found)
      val remembered  = observation match
        case Some(value) => say.keep(value)
        case None        => say.forget
      remembered *> Stored
        .keep(repo, observation)
        .foldZIO(
          failure =>
            done(
              say,
              if code == 0 then CliExit.LoadFailed.code else code,
              joinErr(err, failure.message),
            ),
          _ => done(say, code, err),
        )
    }

  private def joinErr(err: String, more: String): String =
    if err.isEmpty then more else s"$err\n$more"

  private def emit(
      say: Say,
      message: Message,
  ): IO[ServerError, Unit] =
    say(message)

  private object Quiet extends PluginRuntime:
    def run(
        id: PluginTaskId,
        args: PluginArgs,
    ): ZIO[
      histos.schedule.ActionCache & RemoteCache & Resolve,
      PluginFault,
      histos.plugin.PluginOutput,
    ] =
      ZIO.fail(PluginFault.Task(PluginRunError.Unknown(id)))

    def bind: ZIO[
      histos.schedule.ActionCache & RemoteCache & Resolve,
      TaskError,
      Map[TaskId, IO[TaskError, TaskResult]],
    ] =
      ZIO.succeed(Map.empty)

    def links: List[PluginLink] = Nil
  end Quiet
end Calls
