package histos.cli

import heddle.LinePipe
import histos.acp.Acp
import histos.bsp.{Bsp, Frame}
import histos.server.{Attach, Call, Client, Daemon, Decision, ServerError}
import histos.assembly.Assemble
import histos.load.BuildClasspath
import histos.cache.ContentStore
import histos.jdk.{Machine, Os, Settled, Sha256, Startup, Toolchain, ToolchainError}
import histos.load.{LoadError, LoadedBuild, Loader}
import histos.plugin.{PluginArgs, PluginTaskId}
import histos.schedule.RemoteCache
import histos.toolchain.Survey
import histos.mcp.HistosMcp
import histos.resolve.{BuildResolvers, Repositories, Resolve, ResolveError}
import histos.task.{TaskError, TaskId, Tasks}
import histos.version.VersionTask
import histos.workflow.{Workflow, WorkflowAction}
import histos.central.Publisher
import histos.publish.LocalPublish
import histos.testrun.{ModuleTest, ModuleTestError, SuitePattern, TestFeed, TestStatus, TestedCell}
import histos.watch.SourceWatch
import histos.zinc.{CompileFeed, CompiledCell, GraphCompile, GraphCompileError}
import histos.{Module, Platform}

import java.nio.file.{Files, Path}
import java.time.Instant

import zio.*

enum Command:
  case Version
  case Graph
  case Run(id: TaskId, platform: Option[Platform])
  case Plugin(id: PluginTaskId, args: PluginArgs)
  case Test(projects: List[String])
  case TestOnly(modules: List[String], patterns: List[SuitePattern])
  case TestQuick(modules: List[String], patterns: List[SuitePattern])
  case Watch(command: Command)
  case Compile(projects: List[String])
  case Assembly(destination: Option[String])
  case Publish(repository: String, projects: List[String])
  case Mcp
  case Acp
  case Bsp
  case Server
  case Toolchain
  case CachePath
  case CacheList
  case CacheClean(dryRun: Boolean)
  case Resolve(action: ResolveAction)
  case Workflow(action: WorkflowAction)

enum ResolveAction:
  case Verify, Update, Check

enum CliParseError:
  case Help
  case UnknownCommand
  case UnknownRoot
  case MissingArgument
  case UnexpectedArguments
  case BadTaskId
  case BadPluginTask(detail: String)
  case BadPlatform(detail: String)
  case TaskPlatform(name: String)
  case MissingPattern
  case Pattern(detail: String)
  case WatchCommand(detail: String)
  case Rejected

  def message: String = this match
    case Help | UnknownCommand | UnknownRoot | Rejected => Cli.usageText
    case MissingArgument                                 => "run needs one name"
    case UnexpectedArguments                             => Cli.usageText
    case BadTaskId                                       => "a task id is not empty"
    case BadPluginTask(detail)                           => detail
    case BadPlatform(detail)              => detail
    case TaskPlatform(name)               => s"run $name does not take a platform"
    case MissingPattern                   => "testOnly needs a pattern"
    case Pattern(detail)                  => detail
    case WatchCommand(detail)             => detail

enum CliExit:
  case Success, TaskFailed, Usage, LoadFailed, ToolchainFailed, ResolveFailed, WorkflowFailed, TestFailed, CompileFailed, PublishFailed
  case Program(status: Int)
  case Interrupted

  def code: Int = this match
    case Success         => 0
    case TaskFailed      => 1
    case Program(status) => status
    case Interrupted     => 130
    case Usage           => 2
    case LoadFailed      => 3
    case ToolchainFailed => 4
    case ResolveFailed   => 5
    case WorkflowFailed  => 6
    case TestFailed      => 7
    case CompileFailed   => 8
    case PublishFailed   => 9

object CliExit:
  def fromCode(code: Int): CliExit =
    code match
      case 0 => Success
      case 1 => TaskFailed
      case 2 => Usage
      case 3 => LoadFailed
      case 4 => ToolchainFailed
      case 5 => ResolveFailed
      case 6 => WorkflowFailed
      case 7 => TestFailed
      case 8 => CompileFailed
      case 9 => PublishFailed
      case _ => TaskFailed

final case class Completed(exit: CliExit, out: String, err: String)

object Cli:
  val VersionText: String = "histos 0.0.1-SNAPSHOT"

  val usageText: String =
    "usage: histos (version | graph | run <name> [jvm|js|native] | test [project ...] | testOnly [--module project ...] <pattern> ... | testQuick [--module project ...] [pattern ...] | ~ <command> | compile [project ...] | assembly [path] | publish <repository> [project ...] | mcp | acp | bsp | server | toolchain | cache path | cache list | cache clean [--dry-run] | resolve [--update | --check] | workflow [--check | --update])"

  def parse(args: List[String]): IO[CliParseError, Command] =
    Grammar.parse(args)

  def execute(args: List[String], repo: Path, apiJar: Path): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    parse(args).foldZIO(
      {
        case CliParseError.UnknownRoot => contributed(args, repo, apiJar)
        case detail                    => ZIO.succeed(Completed(CliExit.Usage, "", detail.message))
      },
      command => perform(repo, apiJar, command),
    )

  private def perform(repo: Path, apiJar: Path, command: Command): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    command match
          case Command.Version          => ZIO.succeed(Completed(CliExit.Success, VersionText, ""))
          case Command.Graph            => graph(repo, apiJar)
          case Command.Run(id, platform) => run(repo, apiJar, id, platform)
          case Command.Plugin(id, pluginArgs) => plugin(repo, apiJar, id, pluginArgs)
          case Command.Test(projects)   => test(repo, apiJar, projects, Nil, quick = false)
          case Command.TestOnly(modules, patterns) => test(repo, apiJar, modules, patterns, quick = false)
          case Command.TestQuick(modules, patterns) => test(repo, apiJar, modules, patterns, quick = true)
          case Command.Watch(inner)     => watch(repo, apiJar, inner)
          case Command.Compile(projects) => compile(repo, apiJar, projects)
          case Command.Assembly(destination) => assembly(repo, apiJar, destination)
          case Command.Publish(repository, projects) => publish(repo, apiJar, repository, projects)
          case Command.Mcp              => mcp(repo, apiJar)
          case Command.Acp              => acp
          case Command.Bsp              => bsp(repo, apiJar)
          case Command.Server          => serve(repo, apiJar)
          case Command.Toolchain        => toolchain(repo, apiJar)
          case Command.CachePath        => cachePath
          case Command.CacheList        => cacheList
          case Command.CacheClean(dry)  => cacheClean(dry)
          case Command.Resolve(action)  => resolve(repo, apiJar, action)
          case Command.Workflow(action) => workflow(repo, apiJar, action)

  /**
   * Run `inner`, then again after a source save. A save during a run waits until that run finishes.
   * Interrupt while a run is in flight exits 130. Interrupt while waiting exits 0.
   */
  private def watch(repo: Path, apiJar: Path, inner: Command): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    Ref.make(false).flatMap { running =>
      def loop: URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
        running.set(true) *>
          perform(repo, apiJar, inner).flatMap { _ =>
            running.set(false) *>
              say("watching") *>
              watchRoots(repo, apiJar, inner).flatMap { paths =>
                SourceWatch.changed(paths).foldZIO(
                  error => ZIO.succeed(Completed(CliExit.TaskFailed, "", error.message)),
                  _ => loop,
                )
              }
          }
      loop.foldCauseZIO(
        cause =>
          if cause.isInterruptedOnly then
            running.get.map(on => Completed(if on then CliExit.Interrupted else CliExit.Success, "", ""))
          else ZIO.succeed(Completed(CliExit.TaskFailed, "", "watch stopped")),
        done => ZIO.succeed(done),
      )
    }

  private def watchRoots(repo: Path, apiJar: Path, command: Command): URIO[Loader & Scope & Resolve, List[Path]] =
    ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)).either.map {
      case Left(_) => List(repo.resolve("histos.scala"))
      case Right(loaded) =>
        val modules = loaded.graph.projects.collect { case module: Module => module }
        val named = watchedProjects(command)
        val chosen = if named.isEmpty then modules else modules.filter(module => named.contains(module.name.value))
        SourceWatch.roots(repo, chosen)
    }

  private def watchedProjects(command: Command): List[String] =
    command match
      case Command.Test(projects)              => projects
      case Command.TestOnly(modules, _)        => modules
      case Command.TestQuick(modules, _)       => modules
      case Command.Compile(projects)           => projects
      case Command.Run(id, _)
          if id != histos.task.TaskId.Inspect && id != histos.task.TaskId.Versions =>
        List(id.value)
      case _ => Nil

  private def graph(repo: Path, apiJar: Path): URIO[Loader & Scope & Resolve, Completed] =
    ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)).either.map:
      case Left(err)     => Completed(CliExit.LoadFailed, "", err.message)
      case Right(loaded) => Completed(CliExit.Success, GraphText.render(loaded.graph, loaded.plugins.links), "")

  private def run(repo: Path, apiJar: Path, id: TaskId, platform: Option[Platform]): URIO[Loader & Scope & Machine & Resolve, Completed] =
    ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)).either.flatMap:
          case Left(err) => ZIO.succeed(Completed(CliExit.LoadFailed, "", err.message))
          case Right(loaded) =>
            if id == TaskId.Inspect || id == TaskId.Versions then task(repo, loaded, id)
            else program(repo, loaded, id.value, platform)

  /** `inspect` and `versions` stay tasks. Any other id is a module name. */
  private def task(repo: Path, loaded: histos.load.LoadedBuild, id: TaskId): URIO[Machine & Scope, Completed] =
    ZIO.serviceWithZIO[Machine]: machine =>
      val runners = Map(TaskId.Versions -> VersionTask.fromRepo(loaded.graph, repo, machine.env))
      ZIO.scoped:
        for
          run <- ZIO.serviceWithZIO[Tasks](_.run(id))
          out <- run.await
        yield Completed(CliExit.Success, out.text, "")
      .provide(Tasks.live(loaded.graph, runners))
      .catchAll(err => ZIO.succeed(Completed(taskExit(err), "", err.message)))

  /** Compile the main classpath of one platform, link when that platform needs it, and run the named class. */
  private def program(repo: Path, loaded: histos.load.LoadedBuild, name: String, platform: Option[Platform]): URIO[Machine & Resolve, Completed] =
    ZIO.serviceWithZIO[Machine]: machine =>
      Ref.make(0).flatMap: status =>
        val ran =
          for
            jdk <- GraphCompile.jdkHere.mapError(histos.program.ProgramError.Compile(_))
            _ <- histos.program.Program.console(repo, loaded.graph, name, jdk, machine.env, platform).foreach:
              case histos.program.ProgramFeed.Begin(module, version, target) =>
                say(s"${module.value} ${version.value} ${target.token}")
              case histos.program.ProgramFeed.Out(text) => say(text)
              case histos.program.ProgramFeed.Err(text) => sayError(text)
              case histos.program.ProgramFeed.Exited(_, _, _, code) =>
                ZIO.when(code != 0)(status.set(code))
            code <- status.get
          yield Completed(if code == 0 then CliExit.Success else CliExit.Program(code), "", "")
        ran.foldZIO(
          error => ZIO.succeed(Completed(programExit(error), "", error.message)),
          done => ZIO.succeed(done),
        )

  private def programExit(error: histos.program.ProgramError): CliExit =
    error match
      case histos.program.ProgramError.Compile(GraphCompileError.Lock(_) | GraphCompileError.LockRead(_)) =>
        CliExit.ResolveFailed
      case histos.program.ProgramError.Lock(_) | histos.program.ProgramError.LockRead(_) =>
        CliExit.ResolveFailed
      case histos.program.ProgramError.Compile(_) =>
        CliExit.CompileFailed
      case _ =>
        CliExit.TaskFailed

  private def compile(repo: Path, apiJar: Path, projects: List[String]): URIO[Loader & Scope & Resolve, Completed] =
    ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)).either.flatMap:
      case Left(error) => ZIO.succeed(Completed(CliExit.LoadFailed, "", error.message))
      case Right(loaded) =>
        attach(repo, Call.Compile(projects), compileLocal(repo, loaded, projects))

  private def compileLocal(repo: Path, loaded: histos.load.LoadedBuild, projects: List[String]): URIO[Resolve, Completed] =
    GraphCompile.jdkHere.foldZIO(
      error => ZIO.succeed(Completed(CliExit.CompileFailed, "", error.message)),
      jdk =>
        GraphCompile.console(repo, loaded.graph, projects, jdk).foreach(showCompile(repo, _)).foldZIO(
          error => ZIO.succeed(Completed(CliExit.CompileFailed, "", compileReport(error))),
          _ => ZIO.succeed(Completed(CliExit.Success, "", "")),
        ),
    )

  private def assembly(repo: Path, apiJar: Path, destination: Option[String]): URIO[Loader & Scope & Resolve, Completed] =
    ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)).either.flatMap:
      case Left(error) => ZIO.succeed(Completed(CliExit.LoadFailed, "", error.message))
      case Right(loaded) =>
        target(repo, destination) match
          case None => ZIO.succeed(Completed(CliExit.CompileFailed, "", "assembly path is not a path"))
          case Some(path) =>
            GraphCompile.jdkHere.either.flatMap:
              case Left(error) => ZIO.succeed(Completed(CliExit.CompileFailed, "", error.message))
              case Right(jdk) =>
                Assemble.run(repo, loaded.graph, jdk, path).either.flatMap:
                  case Left(error) => ZIO.succeed(Completed(CliExit.CompileFailed, "", error.message))
                  case Right(assembled) =>
                    pin(repo, assembled.path).foldZIO(
                      message => ZIO.succeed(Completed(CliExit.CompileFailed, "", message)),
                      _ => ZIO.succeed(Completed(CliExit.Success, relative(repo, assembled.path), "")),
                    )

  /** The launcher verifies this checksum of `dist/histos.jar`. Another destination is not that jar. */
  private def pin(repo: Path, jar: Path): IO[String, Unit] =
    val pinned = repo.resolve("dist").resolve("histos.jar").toAbsolutePath.normalize
    if jar.toAbsolutePath.normalize != pinned then ZIO.unit
    else
      ZIO.attempt {
        val sha = Sha256.of(Chunk.fromArray(Files.readAllBytes(jar))).value
        val dir = repo.resolve(".histos")
        Files.createDirectories(dir)
        Files.writeString(dir.resolve("assembly.sha256"), sha + "\n")
        ()
      }.mapError(_ => s"could not write ${repo.resolve(".histos").resolve("assembly.sha256")}")

  private def target(repo: Path, destination: Option[String]): Option[Path] =
    destination match
      case None => Some(repo.resolve("dist").resolve("histos.jar"))
      case Some(raw) =>
        try
          val path = Path.of(raw)
          Some(if path.isAbsolute then path else repo.resolve(path))
        catch case _: java.nio.file.InvalidPathException => None

  private def relative(repo: Path, path: Path): String =
    val base = repo.toAbsolutePath.normalize
    val full = path.toAbsolutePath.normalize
    (if full.startsWith(base) then base.relativize(full) else full).toString.replace('\\', '/')

  /** Diagnostics are already on the console. A compile failure keeps the one-line summary here. */
  private[histos] def compileFailure(error: GraphCompileError): String =
    compileReport(error)

  private[histos] def testFailure(error: ModuleTestError): (Int, String) =
    (testExit(error).code, error.message)

  private def compileReport(error: GraphCompileError): String =
    error match
      case GraphCompileError.Failed(module, version, _, _) =>
        s"'${module.value}' ${version.value} failed to compile"
      case other => other.message

  private def showCompile(repo: Path, feed: CompileFeed): UIO[Unit] =
    feed match
      case CompileFeed.Diagnostic(text) => sayError(text)
      case CompileFeed.Done(cell)       => say(compiledLine(repo, cell))

  private def compiledLine(repo: Path, cell: CompiledCell): String =
    Lines.compiled(repo, cell)

  private def publish(repo: Path, apiJar: Path, directory: String, projects: List[String]): URIO[Loader & Scope & Resolve & Machine, Completed] =
    ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)).either.flatMap:
      case Left(error) => ZIO.succeed(Completed(CliExit.LoadFailed, "", error.message))
      case Right(loaded) =>
        ZIO.attempt(Path.of(directory)).either.flatMap:
          case Left(_) => ZIO.succeed(Completed(CliExit.PublishFailed, "", s"'$directory' is not a path"))
          case Right(raw) =>
            val destination = if raw.isAbsolute then raw else repo.resolve(raw)
            GraphCompile.jdkHere.either.flatMap:
              case Left(error) => ZIO.succeed(Completed(CliExit.PublishFailed, "", error.message))
              case Right(jdk) =>
                ZIO.serviceWithZIO[Machine]: machine =>
                  LocalPublish
                    .run(repo, loaded.graph, projects, jdk, machine.env)
                    .provideSome[histos.resolve.Resolve](Publisher.local(destination))
                    .mapBoth(
                      failure => Completed(CliExit.PublishFailed, "", failure.message),
                      lines => Completed(CliExit.Success, lines.map(line => publishedLine(destination, line)).mkString("\n"), ""),
                    )
                    .merge

  private def publishedLine(destination: Path, line: histos.publish.PublishedModule): String =
    val base = destination.toAbsolutePath.normalize
    val full = line.jar.toAbsolutePath.normalize
    val shown = (if full.startsWith(base) then base.relativize(full) else full).toString.replace('\\', '/')
    s"${line.group.value} ${line.artifact} ${line.revision} $shown"

  /** Compile, then one subprocess per Scala version. `no tests` is success and is not a passing suite. */
  private def test(repo: Path, apiJar: Path, projects: List[String], patterns: List[SuitePattern], quick: Boolean): URIO[Loader & Scope & Resolve, Completed] =
    ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)).either.flatMap:
      case Left(error) => ZIO.succeed(Completed(CliExit.LoadFailed, "", error.message))
      case Right(loaded) =>
        val local = testLocal(repo, loaded, projects, patterns, quick)
        if quick then local
        else attach(repo, Call.Test(projects), local)

  private def testLocal(repo: Path, loaded: histos.load.LoadedBuild, projects: List[String], patterns: List[SuitePattern], quick: Boolean): URIO[Resolve, Completed] =
    Ref.make(false).flatMap { failed =>
          val ran =
            for
              jdk <- GraphCompile.jdkHere.mapError(ModuleTestError.Compile(_))
              cells <- GraphCompile.console(repo, loaded.graph, projects, jdk).mapError(ModuleTestError.Compile(_)).mapZIO { feed =>
                         showCompile(repo, feed).as {
                           feed match
                             case CompileFeed.Done(cell) => Some(cell)
                             case CompileFeed.Diagnostic(_) => None
                         }
                       }.collectSome.runCollect
              _ <- ModuleTest.suites(repo, loaded.graph, projects, jdk, cells.toList, patterns, quick).foreach {
                     case TestFeed.Begin(module, version, platform) => say(Lines.begin(module, version, platform))
                     case TestFeed.Out(text)              => say(text)
                     case TestFeed.Err(text)              => sayError(text)
                     case TestFeed.Cached(module, version, platform, suite) => say(Lines.cached(module, version, platform, suite))
                     case TestFeed.Done(cell) =>
                       ZIO.when(failedCell(cell.status))(failed.set(true)) *>
                         say(testedLine(cell)) *>
                         say("")
                   }
              bad <- failed.get
            yield Completed(if bad then CliExit.TestFailed else CliExit.Success, "", "")
          ran.foldZIO(
            error => ZIO.succeed(Completed(testExit(error), "", error.message)),
            done => ZIO.succeed(done),
          )
        }

  /** A missing lock is still a resolve failure. A test result, including a non-zero main, is not. */
  private def testExit(error: ModuleTestError): CliExit =
    error match
      case ModuleTestError.Compile(GraphCompileError.Lock(_) | GraphCompileError.LockRead(_)) =>
        CliExit.ResolveFailed
      case ModuleTestError.Lock(_) | ModuleTestError.LockRead(_) =>
        CliExit.ResolveFailed
      case _ =>
        CliExit.TestFailed

  private def say(text: String): UIO[Unit] =
    Console.printLine(text).orElseSucceed(())

  private def sayError(text: String): UIO[Unit] =
    Console.printLineError(text).orElseSucceed(())

  private def testedLine(cell: TestedCell): String =
    Lines.tested(cell)

  private def failedCell(status: TestStatus): Boolean =
    status match
      case TestStatus.Failed(_, _, _)        => true
      case TestStatus.NoTests | TestStatus.Unmatched | TestStatus.Passed(_, _) => false

  private def cachePath: URIO[ContentStore, Completed] =
    ZIO.serviceWith[ContentStore](store => Completed(CliExit.Success, store.root.toString, ""))

  private def cacheList: URIO[ContentStore, Completed] =
    ZIO.serviceWithZIO[ContentStore](_.list).mapBoth(
      err => Completed(CliExit.LoadFailed, "", err.message),
      entries =>
        val text =
          if entries.isEmpty then ""
          else entries.map(entry => s"${entry.digest.value} ${entry.bytes}").mkString("\n")
        Completed(CliExit.Success, text, ""),
    ).merge

  private def cacheClean(dryRun: Boolean): URIO[ContentStore, Completed] =
    ZIO.serviceWithZIO[ContentStore](_.clean(Instant.now(), ContentStore.Retention, dryRun)).mapBoth(
      err => Completed(CliExit.LoadFailed, "", err.message),
      entries =>
        val verb = if dryRun then "would remove" else "removed"
        val text =
          if entries.isEmpty then s"$verb nothing"
          else entries.map(entry => s"$verb ${entry.digest.value}").mkString("\n")
        Completed(CliExit.Success, text, ""),
    ).merge

  private def resolve(repo: Path, apiJar: Path, action: ResolveAction): URIO[Loader & Scope & Resolve, Completed] =
    ZIO.serviceWithZIO[Loader](_.scan(repo)).either.flatMap:
      case Left(error) => ZIO.succeed(Completed(CliExit.LoadFailed, "", error.message))
      case Right(scanned) =>
        val followed: ZIO[Loader & Scope & Resolve, LoadError | ResolveError, String] = action match
          case ResolveAction.Update =>
            Resolve.updatePlugins(repo, scanned.requests, Repositories.central) *>
              afterLoad(repo, apiJar)(loaded => Resolve.update(repo, loaded.graph, BuildResolvers(loaded.graph.resolvers)))
          case ResolveAction.Check =>
            Resolve.requirePlugins(repo, scanned.requests) *>
              Resolve.checkPlugins(repo, scanned.requests, Repositories.central) *>
              afterLoad(repo, apiJar)(loaded => Resolve.check(repo, loaded.graph, BuildResolvers(loaded.graph.resolvers)))
          case ResolveAction.Verify =>
            Resolve.requirePlugins(repo, scanned.requests) *>
              afterLoad(repo, apiJar)(loaded => Resolve.verify(repo, loaded.graph, BuildResolvers(loaded.graph.resolvers)))
        followed.fold(
          {
            case error: LoadError    => Completed(CliExit.LoadFailed, "", error.message)
            case error: ResolveError => Completed(CliExit.ResolveFailed, "", error.message)
          },
          text => Completed(CliExit.Success, text, ""),
        )

  private def afterLoad(repo: Path, apiJar: Path)(
      continue: LoadedBuild => ZIO[Resolve, ResolveError, String],
  ): ZIO[Loader & Scope & Resolve, LoadError | ResolveError, String] =
    ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)).flatMap(continue)

  private def workflow(repo: Path, apiJar: Path, action: WorkflowAction): URIO[Loader & Scope & Resolve, Completed] =
    ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)).either.flatMap:
      case Left(error) => ZIO.succeed(Completed(CliExit.LoadFailed, "", error.message))
      case Right(loaded) =>
        val effect = action match
          case WorkflowAction.Check  => Workflow.check(repo, loaded.graph)
          case WorkflowAction.Update => Workflow.update(repo, loaded.graph)
        effect.mapBoth(
          error => Completed(CliExit.WorkflowFailed, "", error.message),
          file => Completed(CliExit.Success, file.toString, ""),
        ).merge

  private def toolchain(repo: Path, apiJar: Path): URIO[Loader & Scope & Machine & Resolve, Completed] =
    ZIO.serviceWithZIO[Machine] { machine =>
      ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)).either.flatMap:
        case Left(LoadError.MissingFile(_)) =>
          Toolchain.explain(machine).mapBoth(
            err => Completed(CliExit.ToolchainFailed, "", err.message),
            found => Completed(CliExit.Success, Toolchain.text(found), ""),
          ).merge
        case Left(error) => ZIO.succeed(Completed(CliExit.LoadFailed, "", error.message))
        case Right(loaded) =>
          Survey.read(machine, loaded.graph).mapBoth(
            err => Completed(CliExit.ToolchainFailed, Survey.shown(err), err.message),
            found => Completed(CliExit.Success, Survey.text(found), ""),
          ).merge
    }

  private def mcp(repo: Path, apiJar: Path): URIO[Loader & Scope & Machine & Resolve & ContentStore, Completed] =
    ZIO.serviceWithZIO[Machine]: machine =>
      HistosMcp.serve(repo, apiJar, LinePipe.standard, machine).fold(
      err => Completed(CliExit.LoadFailed, "", err.message),
      _ => Completed(CliExit.Success, "", ""),
    )

  private def bsp(repo: Path, apiJar: Path): URIO[Loader & Scope & Resolve, Completed] =
    Attach.decide(repo, VersionText).flatMap:
      case Decision.Open(socket) =>
        Client.splice(socket).foldZIO(
          {
            case ServerError.Unreachable(_) | ServerError.Stale => bspLocal(repo, apiJar)
            case error => ZIO.succeed(Completed(CliExit.LoadFailed, "", error.message))
          },
          code => ZIO.succeed(Completed(if code == 0 then CliExit.Success else CliExit.TaskFailed, "", "")),
        )
      case Decision.Absent | Decision.Stale(_) => bspLocal(repo, apiJar)

  private def serve(repo: Path, apiJar: Path): URIO[Loader & Scope & Resolve, Completed] =
    ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)).foldZIO(
      error => ZIO.succeed(Completed(CliExit.LoadFailed, "", error.message)),
      loaded =>
        GraphCompile.jdkHere.foldZIO(
          error => ZIO.succeed(Completed(CliExit.CompileFailed, "", error.message)),
          jdk =>
            Daemon.open(repo, loaded.graph, jdk, VersionText).flatMap { record =>
              say(record.socket.toString) *> ZIO.never
            }.foldCauseZIO(
              cause =>
                cause.failureOrCause match
                  case Left(error) => ZIO.succeed(Completed(CliExit.LoadFailed, "", error.message))
                  case Right(rest) => ZIO.failCause(rest)
              ,
              _ => ZIO.succeed(Completed(CliExit.Success, "", "")),
            ),
        ),
    )

  /** A matching server runs the command. Anything else, including a dead socket, runs here. */
  private def attach(repo: Path, call: Call, local: URIO[Resolve, Completed]): URIO[Resolve, Completed] =
    Attach.decide(repo, VersionText).flatMap:
      case Decision.Open(socket) =>
        remote(socket, call).foldZIO(
          {
            case ServerError.Unreachable(_) | ServerError.Stale => local
            case error => ZIO.succeed(Completed(CliExit.LoadFailed, "", error.message))
          },
          done => ZIO.succeed(done),
        )
      case Decision.Absent | Decision.Stale(_) => local

  private def remote(socket: java.nio.file.Path, call: Call): ZIO[Any, ServerError, Completed] =
    zio.Ref.make(Completed(CliExit.Success, "", "")).flatMap { result =>
      Client.talk(socket, call).foreach {
        case histos.server.Feed.Out(text) => say(text)
        case histos.server.Feed.Err(text) => sayError(text)
        case histos.server.Feed.Exit(code, err) =>
          result.set(Completed(CliExit.fromCode(code), "", err))
      } *> result.get
    }

  private def bspLocal(repo: Path, apiJar: Path): URIO[Loader & Scope & Resolve, Completed] =
    ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)).foldZIO(
      error => ZIO.succeed(Completed(CliExit.LoadFailed, "", error.message)),
      loaded =>
        GraphCompile.jdkHere.foldZIO(
          error => ZIO.succeed(Completed(CliExit.CompileFailed, "", error.message)),
          jdk =>
            Bsp.open(repo, loaded.graph, jdk).flatMap: session =>
              speak(session).foldZIO(
                error => ZIO.succeed(Completed(CliExit.LoadFailed, "", error.message)),
                code => ZIO.succeed(Completed(if code == 0 then CliExit.Success else CliExit.TaskFailed, "", "")),
              ),
        ),
    )

  private def speak(session: Bsp.Session): ZIO[Resolve, Frame.FrameError, Int] =
    val input = java.lang.System.in
    val output = java.lang.System.out
    def loop: ZIO[Resolve, Frame.FrameError, Int] =
      Frame.read(input).flatMap:
        case None => ZIO.succeed(1)
        case Some(payload) =>
          session.answer(payload).flatMap:
            case Bsp.Answer.Reply(text) => Frame.write(output, text) *> loop
            case Bsp.Answer.Quiet       => loop
            case Bsp.Answer.Stop(code)  => ZIO.succeed(code)
    loop

  private def acp: UIO[Completed] =
    def loop: UIO[Completed] =
      LinePipe.standard.readLine.foldZIO(
        _ => ZIO.succeed(Completed(CliExit.LoadFailed, "", "the acp pipe failed")),
        {
          case None => ZIO.succeed(Completed(CliExit.Success, "", ""))
          case Some(line) =>
            LinePipe.standard.writeLine(Acp.answer(line)).foldZIO(
              _ => ZIO.succeed(Completed(CliExit.LoadFailed, "", "the acp pipe failed")),
              _ => loop,
            )
        },
      )
    loop

  private def taskExit(err: TaskError): CliExit =
    err match
      case TaskError.EmptyId | TaskError.NoRunner(_) | TaskError.UnknownRun(_) | TaskError.Version(_) | TaskError.Plugin(_) =>
        CliExit.TaskFailed

  private def contributed(args: List[String], repo: Path, apiJar: Path): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    val (watched, tokens) = args match
      case "~" :: rest => (true, rest)
      case other       => (false, other)
    ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)).either.flatMap:
      case Left(err) => ZIO.succeed(Completed(CliExit.LoadFailed, "", err.message))
      case Right(loaded) =>
        val commands = loaded.graph.plugins.flatMap: plugin =>
          plugin.contribution.commands.map(BoundCommand(plugin.id, _))
        PluginCommands.parse(commands, tokens) match
          case Left(_) => ZIO.succeed(Completed(CliExit.Usage, "", PluginCommands.usage(commands)))
          case Right((id, pluginArgs)) =>
            val command = Command.Plugin(id, pluginArgs)
            perform(repo, apiJar, if watched then Command.Watch(command) else command)

  private def plugin(
      repo: Path,
      apiJar: Path,
      id: PluginTaskId,
      args: PluginArgs,
  ): URIO[Loader & Scope & ContentStore & Resolve, Completed] =
    ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)).either.flatMap:
      case Left(err) => ZIO.succeed(Completed(CliExit.LoadFailed, "", err.message))
      case Right(loaded) =>
        ZIO.serviceWithZIO[ContentStore]: store =>
          loaded.plugins
            .run(id, args)
            .provide(ZLayer.succeed(ContentStore.actions(store)), RemoteCache.off)
            .foldCauseZIO(
              cause =>
                if cause.isInterruptedOnly then ZIO.succeed(Completed(CliExit.Interrupted, "", ""))
                else
                  cause.failureOption match
                    case Some(err) => ZIO.succeed(Completed(CliExit.TaskFailed, "", err.message))
                    case None      => ZIO.succeed(Completed(CliExit.TaskFailed, "", "plugin task failed")),
              output => ZIO.succeed(Completed(CliExit.Success, output.text, "")),
            )

object Main extends ZIOAppDefault:
  def run =
    jars.foldZIO(
      error => Console.printLineError(error.message).ignore *> stop(CliExit.LoadFailed.code),
      found =>
        for
          args <- getArgs.map(_.toList)
          _ <- dispatch(args, Path.of("").toAbsolutePath, found)
        yield (),
    )

  private def dispatch(args: List[String], repo: Path, jars: Jars): URIO[Scope, Unit] =
    machineHere(repo).foldZIO(
      err => finish(Completed(CliExit.ToolchainFailed, "", err.message)),
      machine =>
        Cli.parse(args).either.flatMap:
          case Left(_) => runCommand(args, repo, jars.api, machine)
          case Right(Command.Toolchain) => runCommand(args, repo, jars.api, machine)
          case Right(_) =>
            Toolchain.prepare(machine).foldZIO(
              err => finish(Completed(CliExit.ToolchainFailed, "", err.message)),
              {
                case Startup.Continue => runCommand(args, repo, jars.api, machine)
                case Startup.Refuse(text) => finish(Completed(CliExit.ToolchainFailed, "", text))
                case Startup.Replace(javaBin) =>
                  Toolchain.replace(javaBin, jars.program, args).foldZIO(
                    err => finish(Completed(CliExit.ToolchainFailed, "", err.message)),
                    code => stop(code),
                  )
              },
            )
    )

  private final case class Jars(program: Path, api: Path)

  private def runCommand(args: List[String], repo: Path, jar: Path, machine: Machine): URIO[Scope, Unit] =
    ZIO.scoped:
      Cli.execute(args, repo, jar).provideSome[Scope](
        Loader.live,
        ZLayer.succeed(ContentStore.in(machine.cache)),
        ZLayer.succeed(machine),
        Resolve.live(machine.cache.resolve("resolve")),
      )
    .flatMap(finish)

  /** A successful `run` is process status 0. `exit` is the status the operating system sees. */
  private def stop(code: Int): UIO[Unit] =
    exit(zio.ExitCode(code))

  private def finish(done: Completed): UIO[Unit] =
    Console.printLine(done.out).when(done.out.nonEmpty).ignore *>
      Console.printLineError(done.err).when(done.err.nonEmpty).ignore *>
      stop(done.exit.code)

  private def machineHere(repo: Path): IO[ToolchainError, Machine] =
    val env = scala.sys.env
    val os = Option(java.lang.System.getProperty("os.name")).filter(_.nonEmpty) match
      case Some(name) => Os.from(name)
      case None       => Os.Other
    val running = Option(java.lang.System.getProperty("java.home")).filter(_.nonEmpty).map(Path.of(_))
    Option(java.lang.System.getProperty("user.home")).filter(_.nonEmpty) match
      case None => ZIO.fail(ToolchainError.NoUserHome)
      case Some(home) =>
        val cache = ContentStore.home(env.get("XDG_CACHE_HOME"), home)
        ZIO.succeed(Machine(env, home, os, cache, repo, running, Settled.from(env)))

  private def jars: IO[LoadError, Jars] =
    located.flatMap: program =>
      cacheHome.flatMap: cache =>
        BuildClasspath(program, cache).map(api => Jars(program, api))

  private def located: IO[LoadError, Path] =
    val location = Option(classOf[histos.HistosBuild].getProtectionDomain)
      .flatMap(domain => Option(domain.getCodeSource))
      .flatMap(source => Option(source.getLocation))
    location match
      case None => ZIO.fail(LoadError.MissingFile(Path.of("histos-api.jar")))
      case Some(url) =>
        ZIO.attempt(Path.of(url.toURI)).orElseFail(LoadError.Io(histos.load.LoadIo.Classpath))

  private def cacheHome: IO[LoadError, Path] =
    for
      xdg <- System.env("XDG_CACHE_HOME").orElseFail(LoadError.Io(histos.load.LoadIo.Classpath))
      home <- System.property("user.home").orElseFail(LoadError.Io(histos.load.LoadIo.Classpath))
      root <- (xdg.filter(_.nonEmpty), home.filter(_.nonEmpty)) match
        case (Some(dir), _) => ZIO.succeed(Path.of(dir, "histos"))
        case (_, Some(dir)) => ZIO.succeed(Path.of(dir, ".cache", "histos"))
        case _               => ZIO.fail(LoadError.NoHome)
    yield root
