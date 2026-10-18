package histos.workflow

import histos.resolve.PlatformToken
import histos.version.Catalog
import histos.{Aggregate, Axis, Module, Platform, ProjectGraph, ProjectName, ScalaVersion, Splits}

import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}

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
    repo.resolve(".github").resolve("workflows").resolve(fileName)

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

  def render(graph: ProjectGraph): Either[CiError, String] =
    plan(graph, CiEvent.Other).map(jobs => document(graph, jobs))

  def differs(rendered: String, committed: String): Either[CiError, Unit] =
    if rendered == committed then Right(()) else Left(CiError.Drift)

  def check(repo: Path, graph: ProjectGraph): IO[CiError, Path] =
    for
      rendered <- ZIO.fromEither(render(graph))
      file      = path(repo)
      committed <- read(file)
      _         <- ZIO.fromEither(differs(rendered, committed))
    yield file

  def update(repo: Path, graph: ProjectGraph): IO[CiError, Path] =
    for
      rendered <- ZIO.fromEither(render(graph))
      file      = path(repo)
      _         <- write(file, rendered)
    yield file

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

  private def document(graph: ProjectGraph, jobs: List[TestJob]): String =
    val test = jobs match
      case Nil => Nil
      case _   => testJob(jobs)
    val versions = graph.catalog match
      case Catalog.Empty => Nil
      case _             => List(versionsJob)
    val body = test ++ versions ++ List(workflowJob)
    (header ++ body).mkString("\n") + "\n"

  private val header: List[String] =
    List(
      "name: ci",
      "on:",
      "  pull_request:",
      "  push:",
      "permissions:",
      "  contents: read",
      "jobs:",
    )

  private def testJob(jobs: List[TestJob]): List[String] =
    jobs match
      case (aggregate @ TestJob.Aggregate(_)) :: Nil =>
        List(
          "  test:",
          "    runs-on: ubuntu-latest",
          "    steps:",
          "      - uses: actions/checkout@v4",
          s"      - run: ${quote(argv(aggregate).mkString(" "))}",
        )
      case _ =>
        List(
          "  test:",
          "    runs-on: ubuntu-latest",
          "    strategy:",
          "      fail-fast: false",
          "      matrix:",
          "        include:",
        ) ++ jobs.flatMap(include) ++ List(
          "    steps:",
          "      - uses: actions/checkout@v4",
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

  private val versionsJob: String =
    """  versions:
      |    runs-on: ubuntu-latest
      |    steps:
      |      - uses: actions/checkout@v4
      |      - run: "./histos run versions"""".stripMargin

  private val workflowJob: String =
    """  workflow:
      |    runs-on: ubuntu-latest
      |    steps:
      |      - uses: actions/checkout@v4
      |      - run: "./histos workflow --check"""".stripMargin

  private def quote(text: String): String =
    "\"" + text.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

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
