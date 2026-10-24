package histos.workflow

import histos.plugin.{CiJob, JobPermissions, JobWhen, PluginTaskId, RegisteredPlugin}
import histos.resolve.PlatformToken
import histos.version.Catalog
import histos.{Aggregate, Axis, Module, Platform, ProjectGraph, ProjectName, ScalaVersion, Splits}

import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** A pull request names the projects that changed. Anything else runs the full graph. */
enum CiEvent:
  case Other
  case PullRequest(changed: Set[ProjectName])

/** The module axis of one cell. `All` is not a split on [[Axis.Module]]. */
enum ModuleRef:
  case All
  case One(name: ProjectName)

/** The platform axis of one cell. `All` is not a split on [[Axis.Platform]]. */
enum PlatformRef:
  case All
  case One(platform: Platform)

/** The Scala axis of one cell. `All` is not a split on [[Axis.ScalaVersion]]. */
enum VersionRef:
  case All
  case One(version: ScalaVersion)

/** What `./histos test` is asked to run. `Entire` is the full graph, not a list of every name. */
enum TestTarget:
  case Entire
  case Named(first: ProjectName, rest: List[ProjectName])

enum TestJob:
  case Aggregate(target: TestTarget)
  case Split(module: ModuleRef, platform: PlatformRef, version: VersionRef)

enum CiIo:
  case Read, Write

enum WorkflowAction:
  case Check, Update

/** One file `renderAll` emits. `name` is the file name under `.github/workflows`, not a path. */
final case class RenderedWorkflow(name: String, body: String)

enum CiError:
  case BadName(raw: String)
  case Unknown(name: ProjectName)
  case NotModule(name: ProjectName)
  case Duplicate(name: ProjectName)
  case Missing(path: Path)
  case Drift
  case Io(op: CiIo)

  def message: String = this match
    case BadName(raw)  => s"'$raw' is not a project name"
    case Unknown(name) => s"'${name.value}' is not a project"
    case NotModule(name) => s"'${name.value}' is not a module"
    case Duplicate(name) => s"'${name.value}' is listed twice"
    case Missing(path) => s"no workflow at $path"
    case Drift         => "workflow does not match the graph"
    case Io(op)        => s"workflow failed while ${op.toString.toLowerCase}"

object Workflow:
  val fileName: String = "ci.yml"

  def path(repo: Path): Path =
    file(repo, fileName)

  /** A workflow file under `.github/workflows`. `name` is the file name, not a path. */
  def file(repo: Path, name: String): Path =
    repo.resolve(".github").resolve("workflows").resolve(name)

  /** Test cells for this event. Pull requests keep changed modules and their dependents. */
  def plan(graph: ProjectGraph, event: CiEvent): Either[CiError, List[TestJob]] =
    val modules = graph.projects.collect { case module: Module => module }
    event match
      case CiEvent.Other => Right(jobs(graph.splits, CiEvent.Other, modules, modules))
      case CiEvent.PullRequest(changed) =>
        affected(graph, changed).map: names =>
          val chosen = modules.filter(module => names.contains(module.name))
          jobs(graph.splits, event, modules, chosen)

  /** No names is the whole graph. Names are modules, in the order given, without adding dependents. */
  def target(graph: ProjectGraph, raw: List[String]): Either[CiError, TestTarget] =
    if raw.isEmpty then Right(TestTarget.Entire)
    else
      raw.foldLeft[Either[CiError, List[ProjectName]]](Right(Nil)): (acc, item) =>
        acc.flatMap: seen =>
          ProjectName.from(item) match
            case Left(_) => Left(CiError.BadName(item))
            case Right(name) if seen.contains(name) => Left(CiError.Duplicate(name))
            case Right(name) =>
              graph.get(name) match
                case None              => Left(CiError.Unknown(name))
                case Some(_: Aggregate) => Left(CiError.NotModule(name))
                case Some(_: Module)   => Right(seen :+ name)
      .flatMap:
        case first :: rest => Right(TestTarget.Named(first, rest))
        case Nil           => Right(TestTarget.Entire)

  def render(graph: ProjectGraph, setup: Boolean = false): Either[CiError, String] =
    plan(graph, CiEvent.Other).map(jobs => document(graph, jobs, setup))

  /** `ci.yml` plus one `histos-<job>.yml` for each scheduled plugin job. */
  def renderAll(graph: ProjectGraph, setup: Boolean = false): Either[CiError, List[RenderedWorkflow]] =
    render(graph, setup).map: body =>
      RenderedWorkflow(fileName, body) :: scheduled(graph)

  def differs(rendered: String, committed: String): Either[CiError, Unit] =
    if rendered == committed then Right(()) else Left(CiError.Drift)

  def check(repo: Path, graph: ProjectGraph): IO[CiError, Path] =
    for
      setup <- present(repo)
      rendered <- ZIO.fromEither(renderAll(graph, setup))
      _        <- ZIO.foreachDiscard(rendered)(compare(repo, _))
      extra    <- orphans(repo, rendered.map(_.name).toSet)
      _        <- ZIO.when(extra.nonEmpty)(ZIO.fail(CiError.Drift))
    yield path(repo)

  def update(repo: Path, graph: ProjectGraph): IO[CiError, Path] =
    for
      setup <- present(repo)
      rendered <- ZIO.fromEither(renderAll(graph, setup))
      _        <- ZIO.foreachDiscard(rendered)(item => write(file(repo, item.name), item.body))
      extra    <- orphans(repo, rendered.map(_.name).toSet)
      _        <- ZIO.foreachDiscard(extra): orphan =>
        ZIO.attemptBlocking(Files.deleteIfExists(orphan)).mapError(_ => CiError.Io(CiIo.Write))
    yield path(repo)

  /** The process boundary. The command is `./histos test` plus the cell, nothing else. */
  def argv(job: TestJob): List[String] =
    val head = List("./histos", "test")
    job match
      case TestJob.Aggregate(TestTarget.Entire) => head
      case TestJob.Aggregate(TestTarget.Named(first, rest)) =>
        head ++ (first :: rest).map(_.value)
      case TestJob.Split(module, platform, version) =>
        val projects = module match
          case ModuleRef.All        => Nil
          case ModuleRef.One(name)  => List(name.value)
        val platforms = platform match
          case PlatformRef.All            => Nil
          case PlatformRef.One(selected)  => List("--platform", PlatformToken.render(selected))
        val versions = version match
          case VersionRef.All            => Nil
          case VersionRef.One(selected)  => List("--scala", selected.value)
        head ++ projects ++ platforms ++ versions

  private def affected(graph: ProjectGraph, changed: Set[ProjectName]): Either[CiError, List[ProjectName]] =
    val known = graph.projects.map(_.name).toSet
    changed.toList.filter(name => !known.contains(name)).sortBy(_.value) match
      case missing :: _ => Left(CiError.Unknown(missing))
      case Nil =>
        val modules = graph.projects.collect { case module: Module => module }
        Right(close(modules, changed))

  private def close(modules: List[Module], roots: Set[ProjectName]): List[ProjectName] =
    def step(have: Set[ProjectName]): Set[ProjectName] =
      val extra = modules.collect {
        case module if module.upstreamNames.exists(have.contains) => module.name
      }.toSet
      if extra.subsetOf(have) then have else step(have ++ extra)
    val grown = step(roots)
    modules.collect { case module if grown.contains(module.name) => module.name }

  private def jobs(
      splits: Splits,
      event: CiEvent,
      all: List[Module],
      chosen: List[Module],
  ): List[TestJob] =
    if splits.axes.isEmpty then aggregate(event, all, chosen)
    else if chosen.isEmpty then Nil
    else matrix(splits, chosen)

  private def aggregate(event: CiEvent, all: List[Module], chosen: List[Module]): List[TestJob] =
    event match
      case CiEvent.Other => List(TestJob.Aggregate(TestTarget.Entire))
      case CiEvent.PullRequest(_) =>
        val names = all.collect { case module if chosen.exists(_.name == module.name) => module.name }
        names match
          case Nil          => Nil
          case first :: rest => List(TestJob.Aggregate(TestTarget.Named(first, rest)))

  private def matrix(splits: Splits, modules: List[Module]): List[TestJob] =
    val splitModule   = splits.axes.contains(Axis.Module)
    val splitPlatform = splits.axes.contains(Axis.Platform)
    val splitVersion  = splits.axes.contains(Axis.ScalaVersion)
    val groups        = if splitModule then modules.map(List(_)) else List(modules)
    groups.flatMap: group =>
      val moduleRef = if splitModule then oneModule(group) else ModuleRef.All
      val platforms =
        if splitPlatform then distinct(group.flatMap(_.platforms.all)).map(PlatformRef.One(_))
        else List(PlatformRef.All)
      val versions =
        if splitVersion then distinct(group.flatMap(_.versions.all)).map(VersionRef.One(_))
        else List(VersionRef.All)
      platforms.flatMap: platform =>
        versions.map: version =>
          TestJob.Split(moduleRef, platform, version)

  private def oneModule(group: List[Module]): ModuleRef =
    group match
      case module :: _ => ModuleRef.One(module.name)
      case Nil         => ModuleRef.All

  private def distinct[A](values: List[A]): List[A] =
    values.foldLeft(List.empty[A]): (seen, value) =>
      if seen.contains(value) then seen else seen :+ value

  private def toolsMarker(repo: Path): Path =
    repo.resolve(".github").resolve("ci-tools")

  private def present(repo: Path): IO[CiError, Boolean] =
    ZIO.attemptBlocking(Files.isRegularFile(toolsMarker(repo))).mapError(_ => CiError.Io(CiIo.Read))

  private def document(graph: ProjectGraph, jobs: List[TestJob], setup: Boolean): String =
    val test = jobs match
      case Nil => Nil
      case _   => testJob(jobs, setup)
    val versions = graph.catalog match
      case Catalog.Empty => Nil
      case _             => List(versionsJob)
    val body = test ++ versions ++ pluginJobs(graph.plugins) ++ List(workflowJob)
    (header ++ body).mkString("\n") + "\n"

  /** `JobWhen.Always` only. A schedule is its own file, so `cron` does not wrap `test`. */
  private def pluginJobs(plugins: List[RegisteredPlugin]): List[String] =
    plugins.flatMap: plugin =>
      plugin.contribution.jobs.flatMap: job =>
        job.when match
          case JobWhen.Always         => List(jobLines(plugin, job).mkString("\n"))
          case JobWhen.Schedule(_)    => Nil

  private def scheduled(graph: ProjectGraph): List[RenderedWorkflow] =
    graph.plugins.flatMap: plugin =>
      plugin.contribution.jobs.flatMap: job =>
        job.when match
          case JobWhen.Schedule(cron) =>
            val lines = List(
              s"name: histos-${job.name.value}",
              "on:",
              "  schedule:",
              s"    - cron: ${quote(cron.value)}",
              "permissions:",
              "  contents: read",
              "jobs:",
            ) ++ jobLines(plugin, job)
            List(RenderedWorkflow(s"histos-${job.name.value}.yml", lines.mkString("\n") + "\n"))
          case JobWhen.Always => Nil

  private def jobLines(plugin: RegisteredPlugin, job: CiJob): List[String] =
    List(
      s"  ${job.name.value}:",
      s"    runs-on: $Runner",
      "    permissions:",
      s"      contents: ${access(job.permissions)}",
      "    steps:",
      s"      - uses: $Checkout",
      s"      - run: ${quote(s"./histos run ${PluginTaskId(plugin.id, job.task).render}")}",
    )

  private def access(permissions: JobPermissions): String =
    permissions match
      case JobPermissions.ContentsRead  => "read"
      case JobPermissions.ContentsWrite => "write"

  /** A pull request runs once. `push` is only `main`, so the branch push that updates a PR does not start a second run. */
  private val header: List[String] =
    List(
      "name: ci",
      "on:",
      "  pull_request:",
      "  push:",
      "    branches:",
      "      - main",
      "permissions:",
      "  contents: read",
      "jobs:",
    )

  /** Node runs the Scala.js tests. LLVM supplies clang and lld. GitHub publishes no clang setup action.
    * Chekhov 0.1.3 reads Playwright 1.62.1 from `~/.cache/chekhov`, not from an npx cache.
    */
  private val tools: List[String] =
    List(
      "      - uses: actions/setup-node@v4",
      "        with:",
      "          node-version: \"22\"",
      "      - uses: KyleMayes/install-llvm-action@v2.0.9",
      "        with:",
      "          version: \"21.1.8\"",
      "      - run: |",
      "          dir=\"$HOME/.cache/chekhov/playwright/1.62.1\"",
      "          mkdir -p \"$dir\"",
      "          npm install --prefix \"$dir\" --no-fund --no-audit --no-progress playwright@1.62.1",
      "          node \"$dir/node_modules/playwright/cli.js\" install-deps firefox",
      "          node \"$dir/node_modules/playwright/cli.js\" install firefox",
    )

  private def testJob(jobs: List[TestJob], setup: Boolean): List[String] =
    val prepare = if setup then tools else Nil
    jobs match
      case (aggregate @ TestJob.Aggregate(_)) :: Nil =>
        List(
          "  test:",
          s"    runs-on: $Runner",
          "    steps:",
          s"      - uses: $Checkout",
        ) ++ prepare ++ List(
          s"      - run: ${quote(argv(aggregate).mkString(" "))}",
        )
      case _ =>
        List(
          "  test:",
          s"    runs-on: $Runner",
          "    strategy:",
          "      fail-fast: false",
          "      matrix:",
          "        include:",
        ) ++ jobs.flatMap(include) ++ List(
          "    steps:",
          s"      - uses: $Checkout",
        ) ++ prepare ++ List(
          "      - run: \"${{ matrix.command }}\"",
        )

  private def include(job: TestJob): List[String] =
    List(
      s"          - name: ${quote(slug(job))}",
      s"            command: ${quote(argv(job).mkString(" "))}",
    )

  private def slug(job: TestJob): String =
    job match
      case TestJob.Aggregate(_) => "all"
      case TestJob.Split(module, platform, version) =>
        val parts = List(modulePart(module), platformPart(platform), versionPart(version)).flatten
        if parts.isEmpty then "all" else parts.mkString("-")

  private def modulePart(module: ModuleRef): Option[String] =
    module match
      case ModuleRef.All       => None
      case ModuleRef.One(name) => Some(name.value)

  private def platformPart(platform: PlatformRef): Option[String] =
    platform match
      case PlatformRef.All           => None
      case PlatformRef.One(selected) => Some(PlatformToken.render(selected))

  private def versionPart(version: VersionRef): Option[String] =
    version match
      case VersionRef.All           => None
      case VersionRef.One(selected) => Some(selected.value)

  /** Pinned image. `ubuntu-latest` moves, and this render should not move with it. */
  private val Runner = "ubuntu-26.04"

  /** Current major. v4 still targets Node 20, which the runner rewrites to Node 24. */
  private val Checkout = "actions/checkout@v7"

  private val versionsJob: String =
    s"""  versions:
      |    runs-on: $Runner
      |    steps:
      |      - uses: $Checkout
      |      - run: "./histos run versions"""".stripMargin

  private val workflowJob: String =
    s"""  workflow:
      |    runs-on: $Runner
      |    steps:
      |      - uses: $Checkout
      |      - run: "./histos workflow --check"""".stripMargin

  private def quote(text: String): String =
    "\"" + text.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  private def compare(repo: Path, rendered: RenderedWorkflow): IO[CiError, Unit] =
    read(file(repo, rendered.name)).foldZIO(
      {
        case CiError.Missing(_) if rendered.name != fileName => ZIO.fail(CiError.Drift)
        case other                                            => ZIO.fail(other)
      },
      committed => ZIO.fromEither(differs(rendered.body, committed)),
    )

  /** `histos-*.yml` files this graph did not emit. Other workflow names stay. */
  private def orphans(repo: Path, emitted: Set[String]): IO[CiError, List[Path]] =
    val dir = repo.resolve(".github").resolve("workflows")
    ZIO.attemptBlocking {
      if Files.isDirectory(dir) then histosFiles(dir).filter(path => !emitted.contains(fileNameOf(path)))
      else Nil
    }.mapError(_ => CiError.Io(CiIo.Read))

  private def histosFiles(dir: Path): List[Path] =
    val stream = Files.list(dir)
    val found =
      try stream.iterator.asScala.toList
      finally stream.close()
    found.filter: path =>
      val name = fileNameOf(path)
      name.startsWith("histos-") && name.endsWith(".yml")

  private def fileNameOf(path: Path): String =
    Option(path.getFileName).map(_.toString).getOrElse("")

  private def read(file: Path): IO[CiError, String] =
    ZIO.attemptBlocking {
      if Files.isRegularFile(file) then Right(Files.readString(file)) else Left(CiError.Missing(file))
    }.mapError(_ => CiError.Io(CiIo.Read)).flatMap(ZIO.fromEither)

  private def write(file: Path, text: String): IO[CiError, Unit] =
    Option(file.getParent) match
      case None => ZIO.fail(CiError.Io(CiIo.Write))
      case Some(parent) =>
        ZIO.attemptBlocking(replace(parent, file, text)).mapError(_ => CiError.Io(CiIo.Write))

  private def replace(parent: Path, file: Path, text: String): Unit =
    Files.createDirectories(parent)
    val name = Option(file.getFileName).map(_.toString).getOrElse(fileName)
    val temp = parent.resolve(s"$name.tmp")
    Files.writeString(temp, text)
    try Files.move(temp, file, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
    catch
      case _: AtomicMoveNotSupportedException =>
        Files.move(temp, file, StandardCopyOption.REPLACE_EXISTING)
