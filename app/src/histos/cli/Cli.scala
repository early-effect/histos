package histos.cli

import heddle.LinePipe
import histos.acp.Acp
import histos.cache.ContentStore
import histos.jdk.{Machine, Os, Settled, Startup, Toolchain, ToolchainError}
import histos.load.{LoadError, Loader}
import histos.native.{ClangProbe, NativeDeclaration, NativeProject}
import histos.node.{Node, NodeToolchain}
import histos.mcp.HistosMcp
import histos.resolve.{BuildResolvers, Resolve}
import histos.task.{TaskError, TaskId, Tasks}
import histos.version.VersionTask
import histos.workflow.{Workflow, WorkflowAction}
import histos.central.Publisher
import histos.publish.LocalPublish
import histos.testrun.{ModuleTest, ModuleTestError, TestStatus, TestedCell}
import histos.zinc.{CompiledCell, GraphCompile, GraphCompileError}
import histos.{Aggregate, Module, Project}

import java.nio.file.Path
import java.time.Instant

import zio.*

enum Command:
  case Version
  case Graph
  case Run(id: TaskId)
  case Test(projects: List[String])
  case Compile(projects: List[String])
  case Publish(repository: String, projects: List[String])
  case Mcp
  case Acp
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
  case MissingArgument
  case UnexpectedArguments
  case BadTaskId
  case Rejected

  def message: String = this match
    case Help | UnknownCommand | Rejected => Cli.usageText
    case MissingArgument                  => "run needs one task id"
    case UnexpectedArguments              => Cli.usageText
    case BadTaskId                        => "a task id is not empty"

enum CliExit:
  case Success, TaskFailed, Usage, LoadFailed, ToolchainFailed, ResolveFailed, WorkflowFailed, TestFailed, CompileFailed, PublishFailed

  def code: Int = this match
    case Success         => 0
    case TaskFailed      => 1
    case Usage           => 2
    case LoadFailed      => 3
    case ToolchainFailed => 4
    case ResolveFailed   => 5
    case WorkflowFailed  => 6
    case TestFailed      => 7
    case CompileFailed   => 8
    case PublishFailed   => 9

final case class Completed(exit: CliExit, out: String, err: String)

object Cli:
  val VersionText: String = "histos 0.0.1-SNAPSHOT"

  val usageText: String =
    "usage: histos (version | graph | run <id> | test [project ...] | compile [project ...] | publish <repository> [project ...] | mcp | acp | toolchain | cache path | cache list | cache clean [--dry-run] | resolve [--update | --check] | workflow [--check | --update])"

  def parse(args: List[String]): IO[CliParseError, Command] =
    Grammar.parse(args)

  def execute(args: List[String], repo: Path, apiJar: Path): URIO[Loader & Scope & ContentStore & Machine & Resolve, Completed] =
    parse(args).foldZIO(
      detail => ZIO.succeed(Completed(CliExit.Usage, "", detail.message)),
      command =>
        command match
          case Command.Version          => ZIO.succeed(Completed(CliExit.Success, VersionText, ""))
          case Command.Graph            => graph(repo, apiJar)
          case Command.Run(id)          => run(repo, apiJar, id)
          case Command.Test(projects)   => test(repo, apiJar, projects)
          case Command.Compile(projects) => compile(repo, apiJar, projects)
          case Command.Publish(repository, projects) => publish(repo, apiJar, repository, projects)
          case Command.Mcp              => mcp(repo, apiJar)
          case Command.Acp              => acp
          case Command.Toolchain        => toolchain(repo, apiJar)
          case Command.CachePath        => cachePath
          case Command.CacheList        => cacheList
          case Command.CacheClean(dry)  => cacheClean(dry)
          case Command.Resolve(action)  => resolve(repo, apiJar, action)
          case Command.Workflow(action) => workflow(repo, apiJar, action),
    )

  private def graph(repo: Path, apiJar: Path): URIO[Loader & Scope, Completed] =
    ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)).either.map:
      case Left(err)     => Completed(CliExit.LoadFailed, "", err.message)
      case Right(loaded) => Completed(CliExit.Success, render(loaded.graph.projects), "")

  private def run(repo: Path, apiJar: Path, id: TaskId): URIO[Loader & Scope & Machine, Completed] =
    ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)).either.flatMap:
          case Left(err) => ZIO.succeed(Completed(CliExit.LoadFailed, "", err.message))
          case Right(loaded) =>
            ZIO.serviceWithZIO[Machine]: machine =>
              val runners = Map(TaskId.Versions -> VersionTask.fromRepo(loaded.graph, repo, machine.env))
              ZIO.scoped:
                for
                  run <- ZIO.serviceWithZIO[Tasks](_.run(id))
                  out <- run.await
                yield Completed(CliExit.Success, out.text, "")
              .provide(Tasks.live(loaded.graph, runners))
              .catchAll(err => ZIO.succeed(Completed(taskExit(err), "", err.message)))

  private def compile(repo: Path, apiJar: Path, projects: List[String]): URIO[Loader & Scope & Resolve, Completed] =
    ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)).either.flatMap:
      case Left(error) => ZIO.succeed(Completed(CliExit.LoadFailed, "", error.message))
      case Right(loaded) =>
        GraphCompile.jdkHere.flatMap(jdk => GraphCompile.run(repo, loaded.graph, projects, jdk)).mapBoth(
          error => Completed(CliExit.CompileFailed, "", error.message),
          cells => Completed(CliExit.Success, cells.map(cell => compiledLine(repo, cell)).mkString("\n"), ""),
        ).merge

  /** `<module> <scala> jvm <class files> <classes dir>`. The directory is the classpath root, relative to the repo. */
  private def compiledLine(repo: Path, cell: CompiledCell): String =
    val base = repo.toAbsolutePath.normalize
    val full = cell.classes.toAbsolutePath.normalize
    val shown = (if full.startsWith(base) then base.relativize(full) else full).toString.replace('\\', '/')
    s"${cell.module.value} ${cell.version.value} jvm ${cell.classFiles} $shown"

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
  private def test(repo: Path, apiJar: Path, projects: List[String]): URIO[Loader & Scope & Resolve, Completed] =
    ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)).either.flatMap:
      case Left(error) => ZIO.succeed(Completed(CliExit.LoadFailed, "", error.message))
      case Right(loaded) =>
        GraphCompile.jdkHere
          .mapError(ModuleTestError.Compile(_))
          .flatMap(jdk => ModuleTest.run(repo, loaded.graph, projects, jdk))
          .mapBoth(
            error => Completed(testExit(error), "", error.message),
            cells => tested(cells),
          )
          .merge

  /** A missing lock is still a resolve failure. A test result, including a non-zero main, is not. */
  private def testExit(error: ModuleTestError): CliExit =
    error match
      case ModuleTestError.Compile(GraphCompileError.Lock(_) | GraphCompileError.LockRead(_)) =>
        CliExit.ResolveFailed
      case ModuleTestError.Lock(_) | ModuleTestError.LockRead(_) =>
        CliExit.ResolveFailed
      case _ =>
        CliExit.TestFailed

  private def tested(cells: List[TestedCell]): Completed =
    val out = cells.map(testedLine).mkString("\n")
    val err = cells.flatMap(failureText).mkString("\n")
    val exit = if cells.exists(cell => failed(cell.status)) then CliExit.TestFailed else CliExit.Success
    Completed(exit, out, err)

  private def testedLine(cell: TestedCell): String =
    s"${cell.module.value} ${cell.version.value} jvm ${cell.status.label}"

  private def failed(status: TestStatus): Boolean =
    status match
      case TestStatus.Failed(_, _, _)              => true
      case TestStatus.NoTests | TestStatus.Passed => false

  private def failureText(cell: TestedCell): Option[String] =
    cell.status match
      case TestStatus.Failed(_, out, err) =>
        val body = List(out, err).filter(_.nonEmpty).mkString("\n")
        if body.isEmpty then Some(testedLine(cell)) else Some(s"${testedLine(cell)}\n$body")
      case TestStatus.NoTests | TestStatus.Passed => None

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
    ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)).either.flatMap:
      case Left(error) => ZIO.succeed(Completed(CliExit.LoadFailed, "", error.message))
      case Right(loaded) =>
        val effect = action match
          case ResolveAction.Verify => Resolve.verify(repo, loaded.graph, BuildResolvers(loaded.graph.resolvers))
          case ResolveAction.Update => Resolve.update(repo, loaded.graph, BuildResolvers(loaded.graph.resolvers))
          case ResolveAction.Check  => Resolve.check(repo, loaded.graph, BuildResolvers(loaded.graph.resolvers))
        effect.mapBoth(
          error => Completed(CliExit.ResolveFailed, "", error.message),
          text => Completed(CliExit.Success, text, ""),
        ).merge

  private def workflow(repo: Path, apiJar: Path, action: WorkflowAction): URIO[Loader & Scope, Completed] =
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

  private def toolchain(repo: Path, apiJar: Path): URIO[Loader & Scope & Machine, Completed] =
    ZIO.serviceWithZIO[Machine] { machine =>
      Toolchain.explain(machine).either.flatMap:
        case Left(err) => ZIO.succeed(Completed(CliExit.ToolchainFailed, "", err.message))
        case Right(jdkText) =>
          ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)).either.flatMap:
            case Left(LoadError.MissingFile(_)) => ZIO.succeed(Completed(CliExit.Success, jdkText, ""))
            case Left(error)                    => ZIO.succeed(Completed(CliExit.LoadFailed, "", error.message))
            case Right(loaded) =>
              val afterNode =
                if !Node.required(loaded.graph) then ZIO.succeed(Completed(CliExit.Success, jdkText, ""))
                else
                  NodeToolchain.explain(machine).mapBoth(
                    err => Completed(CliExit.ToolchainFailed, jdkText, err.message),
                    text => Completed(CliExit.Success, s"$jdkText\n$text", ""),
                  ).merge
              afterNode.flatMap: soFar =>
                if soFar.exit != CliExit.Success then ZIO.succeed(soFar)
                else clang(loaded.graph, machine, soFar.out)
    }

  private def clang(graph: histos.ProjectGraph, machine: Machine, text: String): URIO[Any, Completed] =
    NativeProject.declaration(graph) match
      case NativeDeclaration.NotNative => ZIO.succeed(Completed(CliExit.Success, text, ""))
      case other @ (NativeDeclaration.Missing(_) | NativeDeclaration.Disagree(_, _)) =>
        ZIO.succeed(Completed(CliExit.ToolchainFailed, text, other.message))
      case NativeDeclaration.Major(major) =>
        ClangProbe.resolve(major, machine.env).mapBoth(
          err => Completed(CliExit.ToolchainFailed, text, err.message),
          found => Completed(CliExit.Success, s"$text\nchosen clang ${found.version.value} ${found.target.value} ${found.clang}", ""),
        ).merge

  private def mcp(repo: Path, apiJar: Path): URIO[Loader & Scope & Machine, Completed] =
    ZIO.serviceWithZIO[Machine]: machine =>
      HistosMcp.serve(repo, apiJar, LinePipe.standard, machine.env).fold(
      err => Completed(CliExit.LoadFailed, "", err.message),
      _ => Completed(CliExit.Success, "", ""),
    )

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
      case TaskError.EmptyId | TaskError.NoRunner(_) | TaskError.UnknownRun(_) | TaskError.Version(_) =>
        CliExit.TaskFailed

  private def render(projects: List[Project]): String =
    projects.map:
      case Module(name, platforms, versions, _, _, _, _, _, _) =>
        s"${name.value} ${platforms.all.map(_.toString.toLowerCase).mkString(",")} ${versions.all.map(_.value).mkString(",")}"
      case Aggregate(name, children, _) =>
        s"${name.value} aggregate ${children.map(_.value).mkString(",")}"
    .mkString("\n")

object Main extends ZIOAppDefault:
  def run =
    apiJar.foldZIO(
      exit => ZIO.succeed(zio.ExitCode(exit.code)),
      jar =>
        for
          args <- getArgs.map(_.toList)
          code <- dispatch(args, Path.of("").toAbsolutePath, jar)
        yield code,
    )

  private def dispatch(args: List[String], repo: Path, jar: Path): URIO[Scope, zio.ExitCode] =
    machineHere(repo).foldZIO(
      err => finish(Completed(CliExit.ToolchainFailed, "", err.message)),
      machine =>
        Cli.parse(args).either.flatMap:
          case Left(_) => runCommand(args, repo, jar, machine)
          case Right(Command.Toolchain) => runCommand(args, repo, jar, machine)
          case Right(_) =>
            Toolchain.prepare(machine).foldZIO(
              err => finish(Completed(CliExit.ToolchainFailed, "", err.message)),
              {
                case Startup.Continue => runCommand(args, repo, jar, machine)
                case Startup.Refuse(text) => finish(Completed(CliExit.ToolchainFailed, "", text))
                case Startup.Replace(javaBin) =>
                  Toolchain.replace(javaBin, jar, args).foldZIO(
                    err => finish(Completed(CliExit.ToolchainFailed, "", err.message)),
                    code => ZIO.succeed(zio.ExitCode(code)),
                  )
              },
            )
    )

  private def runCommand(args: List[String], repo: Path, jar: Path, machine: Machine): URIO[Scope, zio.ExitCode] =
    ZIO.scoped:
      Cli.execute(args, repo, jar).provideSome[Scope](
        Loader.live,
        ZLayer.succeed(ContentStore.in(machine.cache)),
        ZLayer.succeed(machine),
        Resolve.live(machine.cache.resolve("resolve")),
      )
    .flatMap(finish)

  private def finish(done: Completed): UIO[zio.ExitCode] =
    Console.printLine(done.out).when(done.out.nonEmpty).ignore *>
      Console.printLineError(done.err).when(done.err.nonEmpty).ignore *>
      ZIO.succeed(zio.ExitCode(done.exit.code))

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

  private def apiJar: IO[CliExit, Path] =
    val location = Option(classOf[histos.HistosBuild].getProtectionDomain)
      .flatMap(domain => Option(domain.getCodeSource))
      .flatMap(source => Option(source.getLocation))
    location match
      case None => ZIO.fail(CliExit.LoadFailed)
      case Some(url) =>
        ZIO.attempt(Path.of(url.toURI)).orElseFail(CliExit.LoadFailed)
