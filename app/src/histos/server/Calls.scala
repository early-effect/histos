package histos.server

import histos.jdk.JdkRuntime
import histos.assembly.Assemble
import histos.cache.{CacheCleanMode, ContentStore}
import histos.build.BuildEvent
import histos.cli.{Cli, CompileConsole, GraphText}
import histos.report.{BuildFault, CallResult, Diagnostic, Observation, Rejection, Report, Stored, WorkflowFault}
import histos.testrun.{ModuleTest, ModuleTestError, SuitePattern, TestFeed, TestMode, TestRun, TestStatus, TestedCell}
import histos.preview.Plan
import histos.zinc.{CompileFeed, GraphCompile, GraphCompileError}
import histos.jdk.{Machine, Os}
import histos.jdk.Sha256
import histos.load.{LoadError, LoadedBuild, Loader, PluginLink, PluginRuntime}
import histos.load.PluginFault
import histos.plugin.{PluginArgs, PluginRunError, PluginTaskId}
import histos.publish.{LauncherPin, LauncherScript, PublishRun, ReleaseError}
import histos.resolve.{BuildResolvers, DependencyLock, PluginUpdate, Repositories, Resolve, ResolveError}
import histos.schedule.{CacheNamespace, RemoteCache, RemoteError}
import histos.task.{RunId, TaskError, TaskId, TaskResult}
import histos.native.NativeOrigin
import histos.toolchain.{Runtimes, Survey}
import histos.version.VersionTask
import histos.workflow.{Workflow, WorkflowAction}
import histos.{Platform, ProjectGraph, ProjectName, Publishing, Sona}

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
          if err.isEmpty then ZIO.unit else send(Message.Event(BuildEvent.Err(err)))
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

  def record(repo: Path, feeds: Ref[Chunk[Feed]]): Say =
    new Say({
      case Message.Event(event) =>
        feeds.update(_ :+ Feed.Event(event)) *> histos.mcp.Notice.show(repo, event)
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

  def publishLocal(
      repo: Path,
      graph: ProjectGraph,
      build: histos.HistosBuild,
      machine: Machine,
      jdk: JdkRuntime,
      tokens: List[String],
      say: Say,
  ): ZIO[Resolve & ContentStore, ServerError, Unit] =
    CompileConsole.open(repo).flatMap { console =>
      reported(
        repo,
        say,
        console,
        PublishRun.local(
          repo,
          graph,
          build,
          tokens,
          jdk,
          machine.env,
          feed => speak(say, console, feed).ignore,
          hear(say),
        ),
      )
    }

  def publishSnapshots(
      repo: Path,
      graph: ProjectGraph,
      build: histos.HistosBuild,
      machine: Machine,
      jdk: JdkRuntime,
      tokens: List[String],
      say: Say,
  ): ZIO[Resolve & ContentStore, ServerError, Unit] =
    CompileConsole.open(repo).flatMap { console =>
      reported(
        repo,
        say,
        console,
        PublishRun.snapshots(
          repo,
          graph,
          build,
          tokens,
          jdk,
          machine.env,
          feed => speak(say, console, feed).ignore,
          hear(say),
        ),
      )
    }

  def publishRelease(
      repo: Path,
      graph: ProjectGraph,
      build: histos.HistosBuild,
      machine: Machine,
      jdk: JdkRuntime,
      tokens: List[String],
      automatic: Boolean,
      say: Say,
  ): ZIO[Resolve & ContentStore, ServerError, Unit] =
    CompileConsole.open(repo).flatMap { console =>
      released(
        repo,
        say,
        console,
        PublishRun.release(
          repo,
          graph,
          build,
          tokens,
          jdk,
          machine.env,
          automatic,
          feed => speak(say, console, feed).ignore,
          hear(say),
        ),
      )
    }

  def promote(repo: Path, machine: Machine, id: String, say: Say): IO[ServerError, Unit] =
    changed(say, PublishRun.promote(repo, machine.env, id, hear(say)))

  def drop(
      repo: Path,
      graph: ProjectGraph,
      machine: Machine,
      id: String,
      say: Say,
  ): IO[ServerError, Unit] =
    changed(say, PublishRun.drop(repo, graph, machine.env, id, hear(say)))

  def publishAssets(
      repo: Path,
      graph: ProjectGraph,
      machine: Machine,
      tokens: List[String],
      say: Say,
  ): IO[ServerError, Unit] =
    PublishRun
      .assets(repo, graph, tokens, machine.env)
      .foldZIO(
        failure => finish(say, CallResult.Fault(BuildFault.Release(failure.message))),
        report =>
          val shown  = if report.anyFailed then report.uploaded else report.lines
          val result =
            if report.anyFailed then CallResult.Fault(BuildFault.Release(report.failed.mkString("\n")))
            else CallResult.reported()
          told(say, shown.mkString("\n")) *> finish(say, result),
      )

  private def released[R](
      repo: Path,
      say: Say,
      console: histos.cli.CompileConsole.Session,
      ran: ZIO[R, ReleaseError, histos.publish.ReleaseReport],
  ): ZIO[R, ServerError, Unit] =
    ran.foldZIO(
      failure =>
        console.announce(text => told(say, text)) *>
          finishNoted(
            repo,
            say,
            console,
            List(console.observation),
            CallResult.Fault(BuildFault.Release(failure.message)),
          ),
      report =>
        val shown =
          if report.anyFailed then report.refused ++ report.held ++ report.deployed.map(_.line)
          else report.lines
        val result =
          if report.anyFailed then CallResult.Fault(BuildFault.Release(report.failed.mkString("\n")))
          else CallResult.reported()
        console.announce(text => told(say, text)) *>
          told(say, shown.mkString("\n")) *>
          finishNoted(repo, say, console, List(console.observation), result),
    )

  private def changed(say: Say, ran: IO[ReleaseError, Unit]): IO[ServerError, Unit] =
    ran.foldZIO(
      failure => finish(say, CallResult.Fault(BuildFault.Release(failure.message))),
      _ => ok(say),
    )

  private def reported[R](
      repo: Path,
      say: Say,
      console: histos.cli.CompileConsole.Session,
      ran: ZIO[R, histos.publish.LocalPublishError, histos.publish.PublishRun.Report],
  ): ZIO[R, ServerError, Unit] =
    ran.foldZIO(
      failure =>
        console.announce(text => told(say, text)) *>
          finishNoted(
            repo,
            say,
            console,
            List(console.observation),
            CallResult.Fault(BuildFault.Publish(failure)),
          ),
      report =>
        console.announce(text => told(say, text)) *>
          told(say, report.lines.mkString("\n")) *> finishNoted(
            repo,
            say,
            console,
            List(console.observation),
            CallResult.reported(),
          ),
    )

  def run(
      repo: Path,
      graph: ProjectGraph,
      jdk: JdkRuntime,
      machine: Machine,
      id: String,
      platform: Option[Platform],
      say: Say,
  ): ZIO[Resolve & ContentStore, ServerError, Unit] =
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
                platform,
                feed => speak(say, console, feed).ignore,
                console.announce(text => told(say, text).ignore),
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
                  errLine(say, text)
                case histos.program.ProgramFeed.Exited(_, _, _, code) =>
                  ZIO.when(code != 0)(status.set(code)).unit
              .foldZIO(
                {
                  case error: histos.program.ProgramError =>
                    val announced = error match
                      case histos.program.ProgramError.Compile(_) =>
                        console.announce(text => told(say, text))
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
      action: ResolveAction,
      say: Say,
      native: histos.native.NativeOrigin,
  ): ZIO[Resolve & Loader, ServerError, Unit] =
    resolveWork(repo, apiJar, action, native).foldZIO(
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
      loaded: LoadedBuild,
      action: WorkflowAction,
      say: Say,
  ): IO[ServerError, Unit] =
    sonaOf(loaded).flatMap(sona =>
      workflowWork(repo, loaded.graph, sona, action).foldZIO(
        why => fault(say, BuildFault.Workflow(why)),
        file => told(say, file.toString) *> ok(say),
      )
    )

  /** The build's `sona`, closed over the idle stock. Check and update read secrets and follow-up. They do not publish.
    */
  def sonaOf(loaded: LoadedBuild): UIO[Publishing] =
    ZIO.service[Publishing].provideLayer(ZLayer.succeed(Sona.idle) >>> loaded.build.sona)

  /** Workflow files and launcher scripts. The pin is not advanced. */
  def workflowWork(
      repo: Path,
      graph: ProjectGraph,
      sona: Publishing,
      action: WorkflowAction,
  ): IO[WorkflowFault, Path] =
    val rendered = action match
      case WorkflowAction.Check =>
        Workflow.check(repo, graph, sona).mapError(WorkflowFault.Files(_))
      case WorkflowAction.Update =>
        Workflow.update(repo, graph, sona).mapError(WorkflowFault.Files(_))
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
      mode: CacheCleanMode,
      say: Say,
  ): ZIO[ContentStore, ServerError, Unit] =
    ZIO
      .serviceWithZIO[ContentStore]: store =>
        zio.Clock.instant
          .flatMap(now => store.clean(now, ContentStore.Retention, mode))
      .foldZIO(
        error => fault(say, BuildFault.Cache(error)),
        entries =>
          val verb = mode match
            case CacheCleanMode.DryRun => "would remove"
            case CacheCleanMode.Remove => "removed"
          val text =
            if entries.isEmpty then s"$verb nothing"
            else
              entries
                .map(entry => s"$verb ${entry.digest.value}")
                .mkString("\n")
          told(say, text) *> ok(say),
      )

  /** The remote URI is configuration. The copy is [[histos.schedule.HttpCache.copy]]. This command does not open a
    * connection.
    */
  def cachePromote(from: CacheNamespace, to: CacheNamespace, say: Say): IO[ServerError, Unit] =
    fault(say, BuildFault.Remote(RemoteError.Off))

  def launcher(repo: Path, action: LauncherAction, say: Say): IO[ServerError, Unit] =
    val wrote = action match
      case LauncherAction.Update => LauncherPin.update(repo)
      case LauncherAction.Latest => LauncherPin.latest(repo)
    wrote.foldZIO(
      error => fault(say, BuildFault.Launcher(error)),
      pin => told(say, pin.shown) *> ok(say),
    )

  def assembly(
      repo: Path,
      graph: ProjectGraph,
      jdk: JdkRuntime,
      machine: Machine,
      destination: Option[String],
      say: Say,
  ): ZIO[Resolve & ContentStore, ServerError, Unit] =
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
                console.announce(text => told(say, text)) *>
                  finishNoted(repo, say, console, List(console.observation), result)
              ,
              assembled =>
                console.announce(text => told(say, text)) *>
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
      projects: List[ProjectName],
      jdk: JdkRuntime,
      machine: Machine,
      say: Say,
  ): ZIO[Resolve & ContentStore, ServerError, Unit] =
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
              console.announce(text => told(say, text)) *>
                finishNoted(repo, say, console, List(console.observation), result)
            case error: ServerError => ZIO.fail(error)
          },
          _ =>
            console.announce(text => told(say, text)) *>
              finishNoted(repo, say, console, List(console.observation), CallResult.reported()),
        )
    }

  def previewCompile(
      repo: Path,
      graph: ProjectGraph,
      projects: List[ProjectName],
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
      projects: List[ProjectName],
      say: Say,
  ): IO[ServerError, Unit] =
    Plan
      .test(repo, graph, projects)
      .foldZIO(
        error => fault(say, BuildFault.Preview(error)),
        plan => told(say, Plan.renderTest(repo, plan)) *> ok(say),
      )

  def previewPublishLocal(
      repo: Path,
      graph: ProjectGraph,
      tokens: List[String],
      env: Map[String, String],
      say: Say,
  ): IO[ServerError, Unit] =
    Plan
      .publishLocal(repo, graph, tokens, env)
      .foldZIO(
        error => fault(say, BuildFault.Preview(error)),
        plan => told(say, Plan.renderPublish(plan)) *> ok(say),
      )

  def previewPublishSnapshots(
      repo: Path,
      graph: ProjectGraph,
      tokens: List[String],
      env: Map[String, String],
      say: Say,
  ): IO[ServerError, Unit] =
    Plan
      .publishSnapshots(repo, graph, tokens, env)
      .foldZIO(
        error => fault(say, BuildFault.Preview(error)),
        plan => told(say, Plan.renderPublish(plan)) *> ok(say),
      )

  /** Empty names do not read `graph`. */
  def previewClean(
      repo: Path,
      graph: ProjectGraph,
      projects: List[ProjectName],
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
      platform: Option[Platform],
      say: Say,
  ): IO[ServerError, Unit] =
    Plan.run(graph, id, platform) match
      case Left(error) => fault(say, BuildFault.Preview(error))
      case Right(plan) => told(say, Plan.renderRun(plan)) *> ok(say)

  /** No names wipe every product root and do not read `graph`. */
  def clean(
      repo: Path,
      graph: ProjectGraph,
      projects: List[ProjectName],
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
      projects: List[ProjectName],
      patterns: List[SuitePattern],
      mode: TestMode,
      jdk: JdkRuntime,
      machine: Machine,
      say: Say,
  ): ZIO[Resolve & ContentStore, ServerError, Unit] =
    CompileConsole.open(repo).flatMap { console =>
      CompileConsole.notes.flatMap { notes =>
        Ref.make(Chunk.empty[TestedCell]).flatMap { ranCells =>
          Ref.make(List.empty[String]).flatMap { missed =>
            val ran =
              for
                runtimes <- Runtimes.read(machine)
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
                _ <- console.announce(text => told(say, text))
                _ <- ModuleTest
                  .suitesLocked(
                    TestRun(
                      repo,
                      graph,
                      projects,
                      jdk,
                      runtimes,
                      mode,
                      patterns,
                      (module, version, platform, facts) =>
                        notes(module, version, platform, facts)
                          .flatMap(line => told(say, line).ignore),
                    ),
                    cells.toList,
                    lock,
                  )
                  .foreach { feed =>
                    val sent = say(Message.Event(BuildEvent.Test(feed)))
                    feed match
                      case TestFeed.Done(cell) => ranCells.update(_ :+ cell) *> sent
                      case TestFeed.Begin(_, _, _) | TestFeed.Out(_) | TestFeed.Err(_) | TestFeed.Cached(_, _, _, _) =>
                        sent
                  }
                compileErrors <- missed.get
                cells         <- ranCells.get
              yield CallResult.reported(compileErrors.mkString("\n"), cells)
            ran.foldZIO(
              {
                case error: ServerError     => ZIO.fail(error)
                case error: ModuleTestError =>
                  console.announce(text => told(say, text)) *>
                    notes.announce(text => told(say, text)) *>
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
                notes.announce(text => told(say, text)) *>
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
      id: RunId,
      say: Say,
  ): IO[ServerError, Unit] =
    runs.modify(map => (map.get(id), map - id)).flatMap {
      case None        => fault(say, BuildFault.Task(TaskError.UnknownRun(id)))
      case Some(fiber) =>
        fiber.interrupt.flatMap { exit =>
          if exit.isInterrupted then told(say, "cancelled") *> ok(say)
          else fault(say, BuildFault.Task(TaskError.UnknownRun(id)))
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
        .provideSome[Resolve & ContentStore](
          ZLayer.succeed(ContentStore.actions(store)),
          RemoteCache.off,
        )
        .foldZIO(
          error => fault(say, BuildFault.Plugin(error)),
          result => told(say, result.text) *> ok(say),
        )
    }

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
    console.hear(feed) *> say(Message.Event(BuildEvent.Compile(feed)))

  private def compiled(feed: CompileFeed): Option[histos.zinc.CompiledCell] =
    feed match
      case CompileFeed.Done(cell)             => Some(cell)
      case CompileFeed.Began(_, _, _)         => None
      case CompileFeed.Diagnostic(_, _, _, _) => None
      case CompileFeed.Miss(_, _, _, _)       => None
      case CompileFeed.Failed(_)              => None

  private def failedSuite(status: TestStatus): Boolean =
    status match
      case TestStatus.Failed(_)                                          => true
      case TestStatus.NoTests | TestStatus.Unmatched | TestStatus.Passed =>
        false

  def tell(say: Say, text: String): IO[ServerError, Unit] =
    told(say, text)

  def finish(say: Say, result: CallResult): IO[ServerError, Unit] =
    done(say, result)

  private def hear(say: Say): String => UIO[Unit] =
    text => told(say, text).ignore

  private def told(say: Say, text: String): IO[ServerError, Unit] =
    if text.isEmpty then ZIO.unit else say(Message.Event(BuildEvent.Out(text)))

  private def errLine(say: Say, text: String): IO[ServerError, Unit] =
    say(Message.Event(BuildEvent.Err(text)))

  /** A reported summary is a live line. A fault is only the terminal frame. */
  private def done(say: Say, result: CallResult): IO[ServerError, Unit] =
    val live = result match
      case CallResult.Reported(report) =>
        ZIO.foreachDiscard(report.err)(line => errLine(say, line))
      case CallResult.Fault(_) => ZIO.unit
    live *> say(Message.Done(result))

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
