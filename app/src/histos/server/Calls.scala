package histos.server

import histos.jdk.JdkRuntime
import histos.assembly.Assemble
import histos.cache.ContentStore
import histos.cli.{Cli, CompileConsole, GraphText, Lines}
import histos.report.{BuildFault, CallResult, Diagnostic, Observation, Rejection, Report, Stored, WorkflowFault}
import histos.testrun.{ModuleTest, ModuleTestError, SuitePattern, TestFeed, TestRun, TestStatus, TestedCell}
import histos.preview.Plan
import histos.zinc.{CompileFeed, GraphCompile, GraphCompileError}
import histos.jdk.{Machine, Os}
import histos.jdk.Sha256
import histos.load.{LoadError, Loader, PluginLink, PluginRuntime}
import histos.load.PluginFault
import histos.plugin.{PluginArgs, PluginRunError, PluginTaskId}
import histos.publish.{LauncherPin, LauncherScript, PublishRun}
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

  def apply(message: Message): IO[ServerError, Unit] = send(enrich(message))

  /** Buffers filled during the run belong on the report. A fault is already complete. */
  private def enrich(message: Message): Message =
    message match
      case Message.Done(CallResult.Reported(report)) =>
        Message.Done(
          CallResult.Reported(
            report.copy(
              run = report.run.orElse(minted.get),
              observation = report.observation.orElse(kept.get),
              diagnostics = if report.diagnostics.isEmpty then noted.get else report.diagnostics,
            )
          )
        )
      case other => other

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
        case Message.Done(CallResult.Fault(fault)) =>
          val err = fault.message
          if err.isEmpty then ZIO.unit else send(Message.Err(err))
        case Message.Done(CallResult.Reported(_)) => ZIO.unit
        case other                                => send(other)
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
          .write(stream, wire(message))
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
      case Message.Done(result) => feeds.update(_ :+ Feed.Done(result))
      case _                    => ZIO.unit
    })

  private[histos] def wire(message: Message): String = message.toJson
end Say

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
    told(say, GraphText.render(graph, plugins.links)) *> ok(say)

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
                CallResult.Fault(BuildFault.Publish(failure)),
              ),
          report =>
            console.announce(text => say(Message.Out(text))) *>
              told(say, report.lines.mkString("\n")) *> finishNoted(
                repo,
                say,
                console,
                List(console.observation),
                CallResult.reported(),
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
      case Left(error)   => fault(say, BuildFault.Platform(error))
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
                        val result = BuildFault.programStopped(error) match
                          case Some(why) => CallResult.Fault(why)
                          case None      => CallResult.reported(error.message)
                        announced *> finishNoted(
                          repo,
                          say,
                          console,
                          List(console.observation),
                          result,
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
                          CallResult.reported(program = Some(code)),
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
      case Left(error)                             => fault(say, BuildFault.Task(error))
      case Right(found) if found == TaskId.Inspect =>
        if args != PluginArgs.empty then reject(say, Rejection.TaskArguments(found.value))
        else told(say, graph.projects.map(_.name.value).mkString(",")) *> ok(say)
      case Right(found) if found == TaskId.Versions =>
        if args != PluginArgs.empty then reject(say, Rejection.TaskArguments(found.value))
        else
          VersionTask
            .fromRepo(graph, repo, machine.env)
            .foldZIO(
              error => fault(say, BuildFault.Task(error)),
              result => told(say, result.render) *> ok(say),
            )
      case Right(_) =>
        PluginTaskId.parse(id) match
          case Left(error)   => fault(say, BuildFault.PluginTask(error))
          case Right(taskId) => plugin(plugins, taskId, args, say)

  def toolchain(
      machine: Machine,
      graph: ProjectGraph,
      say: Say,
  ): IO[ServerError, Unit] =
    Survey
      .read(machine, graph)
      .foldZIO(
        error => told(say, Survey.shown(error)) *> fault(say, BuildFault.Survey(error)),
        found => told(say, Survey.text(found)) *> ok(say),
      )

  def resolve(
      repo: Path,
      apiJar: Path,
      action: String,
      say: Say,
      native: histos.native.NativeOrigin,
  ): ZIO[Resolve & Loader, ServerError, Unit] =
    resolveAction(action) match
      case None       => reject(say, Rejection.ResolveAction(action))
      case Some(kind) =>
        resolveWork(repo, apiJar, kind, native).foldZIO(
          {
            case error: LoadError    => fault(say, BuildFault.Load(error))
            case error: ResolveError => fault(say, BuildFault.Resolve(error))
          },
          text => told(say, text) *> ok(say),
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
      case None       => reject(say, Rejection.WorkflowAction(action))
      case Some(kind) =>
        workflowWork(repo, graph, kind).foldZIO(
          why => fault(say, BuildFault.Workflow(why)),
          file => told(say, file.toString) *> ok(say),
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
    told(say, text) *> ok(say)

  def cachePath(say: Say): ZIO[ContentStore, ServerError, Unit] =
    ZIO.serviceWithZIO[ContentStore] { store =>
      told(say, store.root.toString) *> ok(say)
    }

  def cacheList(say: Say): ZIO[ContentStore, ServerError, Unit] =
    ZIO
      .serviceWithZIO[ContentStore](_.list)
      .foldZIO(
        error => fault(say, BuildFault.Cache(error)),
        entries =>
          val text =
            if entries.isEmpty then ""
            else
              entries
                .map(entry => s"${entry.digest.value} ${entry.bytes}")
                .mkString("\n")
          told(say, text) *> ok(say),
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
        error => fault(say, BuildFault.Cache(error)),
        entries =>
          val verb = if dryRun then "would remove" else "removed"
          val text =
            if entries.isEmpty then s"$verb nothing"
            else
              entries
                .map(entry => s"$verb ${entry.digest.value}")
                .mkString("\n")
          told(say, text) *> ok(say),
      )

  def launcherUpdate(repo: Path, say: Say): IO[ServerError, Unit] =
    LauncherPin
      .update(repo)
      .foldZIO(
        error => fault(say, BuildFault.Launcher(error)),
        pin => told(say, s"${pin.revision} ${pin.sha256.value}") *> ok(say),
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
        reject(say, Rejection.AssemblyDestination(destination.getOrElse("")))
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
                val result = BuildFault.assembleStopped(error) match
                  case Some(why) => CallResult.Fault(why)
                  case None      => CallResult.reported(error.message)
                console.announce(text => say(Message.Out(text))) *>
                  finishNoted(repo, say, console, List(console.observation), result)
              ,
              assembled =>
                console.announce(text => say(Message.Out(text))) *>
                  pin(repo, assembled.path).foldZIO(
                    failure =>
                      finishNoted(
                        repo,
                        say,
                        console,
                        List(console.observation),
                        CallResult.Fault(BuildFault.Io(failure)),
                      ),
                    _ =>
                      told(say, relative(repo, assembled.path)) *>
                        finishNoted(
                          repo,
                          say,
                          console,
                          List(console.observation),
                          CallResult.reported(),
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
              val result = BuildFault.compileStopped(error) match
                case Some(why) => CallResult.Fault(why)
                case None      => CallResult.reported(Cli.compileFailure(error))
              console.announce(text => say(Message.Out(text))) *>
                finishNoted(repo, say, console, List(console.observation), result)
            case error: ServerError => ZIO.fail(error)
          },
          _ =>
            console.announce(text => say(Message.Out(text))) *>
              finishNoted(repo, say, console, List(console.observation), CallResult.reported()),
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
        error => fault(say, BuildFault.Preview(error)),
        text => told(say, text) *> ok(say),
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
        error => fault(say, BuildFault.Preview(error)),
        plan => told(say, Plan.renderTest(repo, plan)) *> ok(say),
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
        error => fault(say, BuildFault.Preview(error)),
        plan => told(say, Plan.renderPublish(plan)) *> ok(say),
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
        error => fault(say, BuildFault.Preview(error)),
        plan => told(say, Plan.renderClean(repo, plan)) *> ok(say),
      )

  def previewCleanAll(repo: Path, say: Say): IO[ServerError, Unit] =
    val plan = Plan.cleanAll(repo)
    told(say, Plan.renderClean(repo, plan)) *> ok(say)

  def previewCleanFull(repo: Path, say: Say): IO[ServerError, Unit] =
    val plan = Plan.cleanFull(repo)
    told(say, Plan.renderClean(repo, plan)) *> ok(say)

  def previewRun(
      graph: ProjectGraph,
      id: String,
      platform: Option[String],
      say: Say,
  ): IO[ServerError, Unit] =
    chosen(platform) match
      case Left(error)   => fault(say, BuildFault.Platform(error))
      case Right(choice) =>
        Plan.run(graph, id, choice) match
          case Left(error) => fault(say, BuildFault.Preview(error))
          case Right(plan) => told(say, Plan.renderRun(plan)) *> ok(say)

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
      error => fault(say, cleanFault(error)),
      line => told(say, line) *> ok(say),
    )

  def wipe(repo: Path, say: Say): IO[ServerError, Unit] =
    histos.clean.Products
      .wipe(repo)
      .foldZIO(
        error => fault(say, BuildFault.Io(error)),
        _ => told(say, histos.clean.Products.line(Nil)) *> ok(say),
      )

  /** Products go first. A store failure is exit 3 and does not claim success.
    */
  def cleanFull(repo: Path, say: Say): ZIO[ContentStore, ServerError, Unit] =
    histos.clean.Products
      .wipe(repo)
      .foldZIO(
        error => fault(say, BuildFault.Io(error)),
        _ =>
          ZIO
            .serviceWithZIO[ContentStore]: store =>
              zio.Clock.instant.flatMap(now => store.forget(histos.cache.CacheProject.of(repo), now))
            .foldZIO(
              error => fault(say, BuildFault.Cache(error)),
              entries => told(say, histos.clean.Products.forgotten(entries)) *> ok(say),
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
        Ref.make(Chunk.empty[TestedCell]).flatMap { ranCells =>
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
                      ranCells.update(_ :+ cell) *>
                        say(Message.Out(Lines.tested(cell))) *>
                        say(Message.Out(""))
                  }
                compileErrors <- missed.get
                cells         <- ranCells.get
              yield CallResult.reported(compileErrors.mkString("\n"), cells)
            ran.foldZIO(
              {
                case error: ServerError     => ZIO.fail(error)
                case error: ModuleTestError =>
                  console.announce(text => say(Message.Out(text))) *>
                    notes.announce(text => say(Message.Out(text))) *>
                    missed.get.flatMap { compileErrors =>
                      val summary = error match
                        case ModuleTestError.NoneMatched(_) if compileErrors.nonEmpty =>
                          compileErrors.mkString("\n")
                        case other => other.message
                      val result = BuildFault.testStopped(error) match
                        case Some(why) => CallResult.Fault(why)
                        case None      => CallResult.reported(summary)
                      finishNoted(
                        repo,
                        say,
                        console,
                        List(console.observation, notes.observation),
                        result,
                      )
                    }
              },
              result =>
                notes.announce(text => say(Message.Out(text))) *>
                  finishNoted(
                    repo,
                    say,
                    console,
                    List(console.observation, notes.observation),
                    result,
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
      case Left(error)  => fault(say, BuildFault.Task(error))
      case Right(runId) =>
        runs.modify(map => (map.get(runId), map - runId)).flatMap {
          case None        => fault(say, BuildFault.Task(TaskError.UnknownRun(runId)))
          case Some(fiber) =>
            fiber.interrupt.flatMap { exit =>
              if exit.isInterrupted then told(say, "cancelled") *> ok(say)
              else fault(say, BuildFault.Task(TaskError.UnknownRun(runId)))
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
            (if cause.isInterruptedOnly then fault(say, BuildFault.Interrupted)
             else
               cause.failureOrCause match
                 case Left(error) => ZIO.fail(error)
                 case Right(_)    => reject(say, Rejection.RunFailed)),
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
          error => fault(say, BuildFault.Plugin(error)),
          result => told(say, result.text) *> ok(say),
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

  private def cleanFault(
      error: histos.select.SelectError | histos.workspace.IoFailure
  ): BuildFault =
    error match
      case found: histos.select.SelectError  => BuildFault.Select(found)
      case found: histos.workspace.IoFailure => BuildFault.Io(found)

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
  private def pin(repo: Path, jar: Path): IO[histos.workspace.IoFailure, Unit] =
    val pinned = repo.resolve("dist").resolve("histos.jar").toAbsolutePath.normalize
    val dest   = repo.resolve(".histos").resolve("assembly.sha256")
    if jar.toAbsolutePath.normalize != pinned then ZIO.unit
    else
      ZIO
        .attemptBlocking(Files.readAllBytes(jar))
        .mapError(_ => histos.workspace.IoFailure(jar, histos.workspace.IoFault.Read))
        .flatMap: bytes =>
          histos.workspace.Workspace.writeString(dest, Sha256.of(Chunk.fromArray(bytes)).value + "\n")

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

  def finish(say: Say, result: CallResult): IO[ServerError, Unit] =
    done(say, result)

  private def told(say: Say, text: String): IO[ServerError, Unit] =
    if text.isEmpty then ZIO.unit else emit(say, Message.Out(text))

  /** A reported summary is a live line. A fault is only the terminal frame. */
  private def done(say: Say, result: CallResult): IO[ServerError, Unit] =
    val live = result match
      case CallResult.Reported(report) =>
        ZIO.foreachDiscard(report.err)(line => emit(say, Message.Err(line)))
      case CallResult.Fault(_) => ZIO.unit
    live *> emit(say, Message.Done(result))

  private def ok(say: Say): IO[ServerError, Unit] =
    done(say, CallResult.reported())

  private def fault(say: Say, why: BuildFault): IO[ServerError, Unit] =
    done(say, CallResult.Fault(why))

  private def reject(say: Say, why: Rejection): IO[ServerError, Unit] =
    fault(say, BuildFault.Rejected(why))

  /** Compile and test read the console once. The terminal frame then carries that chunk. */
  private[histos] def finishNoted(
      repo: Path,
      say: Say,
      console: CompileConsole.Session,
      parts: List[UIO[Option[Observation]]],
      result: CallResult,
  ): IO[ServerError, Unit] =
    console.diagnostics.flatMap(say.note) *>
      finishObserved(repo, say, parts, result)

  /** Compile, test, run, assembly, and publish replace the stored tally. A store failure on a result that would succeed
    * is [[BuildFault.Store]]. A result that already failed keeps its kind and gains the store line. A fault is left as
    * it is.
    */
  private def finishObserved(
      repo: Path,
      say: Say,
      parts: List[UIO[Option[Observation]]],
      result: CallResult,
  ): IO[ServerError, Unit] =
    ZIO.foreach(parts)(identity).flatMap { found =>
      val observation = Observation.combine(found)
      val remembered  = observation match
        case Some(value) => say.keep(value)
        case None        => say.forget
      remembered *> failing(say, result).flatMap { failed =>
        Stored
          .keep(repo, observation)
          .foldZIO(
            failure =>
              if failed then done(say, remember(result, failure.message))
              else fault(say, BuildFault.Store(failure)),
            _ => done(say, result),
          )
      }
    }

  /** Error diagnostics, a failed cell, a non-zero program, or an error line. */
  private def failing(say: Say, result: CallResult): UIO[Boolean] =
    result match
      case CallResult.Fault(_)         => ZIO.succeed(true)
      case CallResult.Reported(report) =>
        say.diagnostics.map: noted =>
          report.err.nonEmpty ||
            report.program.exists(_ != 0) ||
            report.tests.exists(cell => failedSuite(cell.status)) ||
            erred(report.diagnostics) ||
            erred(noted)

  private def erred(found: Chunk[Diagnostic]): Boolean =
    found.exists(_.severity == histos.report.Severity.Error)

  private def remember(result: CallResult, line: String): CallResult =
    result match
      case CallResult.Reported(report) => CallResult.Reported(report.copy(err = report.err :+ line))
      case fault                       => fault

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
