package histos.preview

import histos.clean.Products
import histos.plugin.PluginTaskId
import histos.publish.LocalPublishError
import histos.select.{SelectError, Selection}
import histos.task.TaskId
import histos.testrun.{ModuleTestError, SuitePatternError, TestCompile}
import histos.toolchain.VersionOrder
import histos.version.{Decision, HoldReason, PublishAction, ShipName, VersionError, VersionTask}
import histos.workspace.{IoFailure, IoFault}
import histos.zinc.{CompileError, GraphCompile, GraphCompileError, Sources}
import histos.{Aggregate, Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.Path

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** Why a module is in the plan. A module outside the closure is [[PlanUnit.NotSelected]], and the printed plan omits
  * it.
  */
enum Reason:
  case Selected
  case Upstream

  def word: String = this match
    case Selected => "selected"
    case Upstream => "upstream"
end Reason

/** Zinc reports invalid sources from `compile`, which writes class files and `inc.zip`. A preview does not call that,
  * so file-level staleness stays unknown. Clean has no sources and still says this.
  */
enum FileStaleness:
  case Unknown

enum PlanUnit:
  case Chosen(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      reason: Reason,
      upstreams: List[ProjectName],
      sources: List[Path],
  )
  case NotSelected(module: ProjectName)

final case class CompilePlan(units: List[PlanUnit], staleness: FileStaleness)

final case class TestPlan(units: List[PlanUnit], staleness: FileStaleness)

enum PublishUnit:
  case Module(
      module: ProjectName,
      ship: ShipName,
      revision: String,
      reason: Reason,
  )
  case Held(ship: ShipName, compile: String, reason: HoldReason)
  case Idle(ship: ShipName, compile: String)

final case class PublishPlan(
    units: List[PublishUnit],
    staleness: FileStaleness,
)

/** What `clean` would delete. [[StoreFate.Forgotten]] is `cleanFull`: this repo's content-store entries, with no byte
  * count.
  */
enum StoreFate:
  case Kept
  case Forgotten

final case class CleanPlan(
    roots: List[Path],
    staleness: FileStaleness,
    store: StoreFate,
)

enum RunUnit:
  case Module(
      module: ProjectName,
      platform: Platform,
      upstreams: List[ProjectName],
  )
  case Task(id: String)

final case class RunPlan(unit: RunUnit, staleness: FileStaleness)

enum RunMiss:
  case Absent(name: String)
  case Aggregate(name: ProjectName)
  case NoMain(name: ProjectName)
  case Choose(name: ProjectName, platforms: List[Platform])
  case Unbuilt(name: ProjectName, platform: Platform, built: List[Platform])
  case UnknownTask(id: String)
  case BadId(detail: String)

  def message: String = this match
    case Absent(name)            => s"'$name' is not a module"
    case Aggregate(name)         => s"'${name.value}' is an aggregate"
    case NoMain(name)            => s"'${name.value}' does not name a main"
    case Choose(name, platforms) =>
      val tokens = platforms.map(_.token).mkString(", ")
      s"'${name.value}' builds $tokens. Name one."
    case Unbuilt(name, platform, built) =>
      val tokens = built.map(_.token).mkString(", ")
      s"'${name.value}' does not build ${platform.token}. It builds $tokens"
    case UnknownTask(id) => s"'$id' is not a task"
    case BadId(detail)   => detail
end RunMiss

enum PreviewError:
  case Target(error: SelectError)
  case Cycle(module: ProjectName)
  case Sources(failure: IoFailure)
  case Version(error: VersionError)
  case Publish(error: LocalPublishError)
  case Run(miss: RunMiss)
  case Pattern(error: SuitePatternError)

  def message: String = this match
    case Target(error)    => error.message
    case Cycle(module)    => s"'${module.value}' is in a compile cycle"
    case Sources(failure) => failure.message
    case Version(error)   => error.message
    case Publish(error)   => error.message
    case Run(miss)        => miss.message
    case Pattern(error)   => error.message
end PreviewError

object Plan:
  def show(
      repo: Path,
      graph: ProjectGraph,
      projects: List[ProjectName],
  ): IO[PreviewError, String] =
    compile(repo, graph, projects).map(render(repo, _))

  def compile(
      repo: Path,
      graph: ProjectGraph,
      projects: List[ProjectName],
  ): IO[PreviewError, CompilePlan] =
    modulePlan(repo, graph, projects, Tree.Main).map: (units, staleness) =>
      CompilePlan(units, staleness)

  def test(
      repo: Path,
      graph: ProjectGraph,
      projects: List[ProjectName],
  ): IO[PreviewError, TestPlan] =
    modulePlan(repo, graph, projects, Tree.Test).map: (units, staleness) =>
      TestPlan(units, staleness)

  /** Reads the version decision already in the tree. Writes no jar and opens no repository.
    */
  def publish(
      repo: Path,
      graph: ProjectGraph,
      projects: List[ProjectName],
      env: Map[String, String],
  ): IO[PreviewError, PublishPlan] =
    VersionTask
      .decide(graph, repo, env)
      .mapError(PreviewError.Version(_))
      .flatMap(decision => ZIO.fromEither(publishPlan(graph, projects, decision)))

  /** Every product root. Does not read the graph and does not delete. */
  def cleanAll(repo: Path): CleanPlan =
    CleanPlan(fullRoots(repo), FileStaleness.Unknown, StoreFate.Kept)

  /** Empty names are [[cleanAll]] and ignore `graph`. A name selects modules only: an aggregate expands, and upstream
    * stays.
    */
  def clean(
      repo: Path,
      graph: ProjectGraph,
      projects: List[ProjectName],
  ): IO[PreviewError, CleanPlan] =
    if projects.isEmpty then ZIO.succeed(cleanAll(repo))
    else
      ZIO
        .fromEither(Selection.select(graph, projects))
        .mapBoth(
          PreviewError.Target(_),
          selected =>
            CleanPlan(
              selected.modules.flatMap(module => moduleRoots(repo, module)),
              FileStaleness.Unknown,
              StoreFate.Kept,
            ),
        )

  /** Product roots, plus this repo's content-store entries. Does not delete them and does not count bytes.
    */
  def cleanFull(repo: Path): CleanPlan =
    cleanAll(repo).copy(store = StoreFate.Forgotten)

  /** The module or task `run` would start. Does not start it. */
  def run(
      graph: ProjectGraph,
      id: String,
      platform: Option[Platform],
  ): Either[PreviewError, RunPlan] =
    if id == TaskId.Inspect.value || id == TaskId.Versions.value then taskOnly(id, platform)
    else if id.contains("/") then pluginTask(graph, id, platform)
    else runModule(graph, id, platform)

  def render(repo: Path, plan: CompilePlan): String =
    renderUnits(repo, plan.units, plan.staleness)

  def renderTest(repo: Path, plan: TestPlan): String =
    renderUnits(repo, plan.units, plan.staleness)

  def renderPublish(plan: PublishPlan): String =
    val body = plan.units.map:
      case PublishUnit.Module(module, ship, revision, reason) =>
        s"${module.value} ${ship.value} $revision ${reason.word}"
      case PublishUnit.Held(ship, compile, reason) =>
        val why = reason match
          case HoldReason.AlreadyReleased => "already-released"
        s"held ${ship.value} $compile $why"
      case PublishUnit.Idle(ship, compile) =>
        s"${ship.value} $compile not published"
    (stalenessLine(plan.staleness) :: body).mkString("\n")

  def renderClean(repo: Path, plan: CleanPlan): String =
    val roots = plan.roots.map(path => relative(repo, path))
    val store = plan.store match
      case StoreFate.Kept      => Nil
      case StoreFate.Forgotten =>
        List("content-store entries for this repo would be forgotten")
    (stalenessLine(plan.staleness) :: roots ++ store).mkString("\n")

  def renderRun(plan: RunPlan): String =
    val body = plan.unit match
      case RunUnit.Module(module, platform, upstreams) =>
        val names = upstreams.map(_.value).mkString(" ")
        val line  = if names.isEmpty then "upstreams" else s"upstreams $names"
        List(s"${module.value} ${platform.token}", line)
      case RunUnit.Task(id) => List(id)
    (stalenessLine(plan.staleness) :: body).mkString("\n")

  private enum Tree:
    case Main
    case Test

  private def modulePlan(
      repo: Path,
      graph: ProjectGraph,
      projects: List[ProjectName],
      tree: Tree,
  ): IO[PreviewError, (List[PlanUnit], FileStaleness)] =
    val modules = graph.projects.collect { case module: Module => module }
    for
      selected <- ZIO
        .fromEither(Selection.select(graph, projects))
        .mapError(PreviewError.Target(_))
      chosen  = selected.modules.map(_.name).toSet
      closure = Module.closure(modules, chosen)
      _     <- ZIO.fromEither(acyclic(closure)).mapError(PreviewError.Cycle(_))
      bases <- ZIO.foreach(closure): module =>
        GraphCompile
          .sourceBase(repo, module, modules)
          .mapBoth(rootFault(repo, _), base => module.name -> base)
      listed <- ZIO.foreach(modules): module =>
        cells(repo, module, modules, closure, chosen, bases.toMap, tree)
    yield (listed.flatten, FileStaleness.Unknown)

  private def renderUnits(
      repo: Path,
      units: List[PlanUnit],
      staleness: FileStaleness,
  ): String =
    val body = units.flatMap:
      case chosen: PlanUnit.Chosen => renderChosen(repo, chosen)
      case PlanUnit.NotSelected(_) => Nil
    (stalenessLine(staleness) :: body).mkString("\n")

  private def acyclic(modules: List[Module]): Either[ProjectName, Unit] =
    val names = modules.map(_.name).toSet
    def step(
        done: Set[ProjectName],
        rest: List[Module],
    ): Either[ProjectName, Unit] =
      val (ready, later) = rest.partition: module =>
        module.upstreamNames.forall: dependency =>
          !names.contains(dependency) || done.contains(dependency)
      ready match
        case Nil =>
          later match
            case module :: _ => Left(module.name)
            case Nil         => Right(())
        case _ =>
          step(done ++ ready.map(_.name), later)
    step(Set.empty, modules)

  private def cells(
      repo: Path,
      module: Module,
      modules: List[Module],
      closure: List[Module],
      selected: Set[ProjectName],
      bases: Map[ProjectName, Option[Path]],
      tree: Tree,
  ): IO[PreviewError, List[PlanUnit]] =
    if !closure.exists(_.name == module.name) then ZIO.succeed(List(PlanUnit.NotSelected(module.name)))
    else
      val reason =
        if selected.contains(module.name) then Reason.Selected
        else Reason.Upstream
      val versions =
        module.versions.all.sortWith: (left, right) =>
          VersionOrder.compare(left.value, right.value) < 0
      val platforms = List(Platform.Jvm, Platform.Js, Platform.Native).filter(
        module.platforms.all.contains
      )
      ZIO
        .foreach(versions): version =>
          ZIO.foreach(platforms): platform =>
            val upstreams = Module.chain(module, closure, platform).map(_.name)
            val base      = bases.get(module.name).flatten
            paths(repo, module, modules, version, platform, base, tree).map: found =>
              PlanUnit.Chosen(
                module.name,
                version,
                platform,
                reason,
                upstreams,
                found,
              )
        .map(_.flatten)

  private def paths(
      repo: Path,
      module: Module,
      modules: List[Module],
      version: ScalaVersion,
      platform: Platform,
      base: Option[Path],
      tree: Tree,
  ): IO[PreviewError, List[Path]] =
    tree match
      case Tree.Main =>
        base match
          case Some(root) =>
            Sources
              .select(root, version, platform)
              .mapBoth(sourceFault(root, _), identity)
          case None => ZIO.succeed(Nil)
      case Tree.Test =>
        TestCompile
          .testSources(repo, module, modules, version, platform)
          .mapError(testFault(repo, _))

  private enum ShipWord:
    case Publish(ship: ShipName, revision: String, members: ::[ProjectName])
    case Held(ship: ShipName, compile: String, reason: HoldReason)
    case Idle(ship: ShipName, compile: String)

  private def publishPlan(
      graph: ProjectGraph,
      projects: List[ProjectName],
      decision: Decision,
  ): Either[PreviewError, PublishPlan] =
    words(decision, graph.catalog).flatMap: found =>
      val idle       = found.collect { case word: ShipWord.Idle => word }
      val held       = found.collect { case word: ShipWord.Held => word }
      val publishing = found.collect { case word: ShipWord.Publish => word }
      if idle.nonEmpty then
        accept(graph, projects).map: _ =>
          PublishPlan(
            idle.map(word => PublishUnit.Idle(word.ship, word.compile)),
            FileStaleness.Unknown,
          )
      else if publishing.isEmpty then
        accept(graph, projects).map: _ =>
          PublishPlan(heldUnits(held), FileStaleness.Unknown)
      else
        val members = publishing.flatMap(_.members.toList).toSet
        chosen(graph, projects, members).flatMap: (roots, names) =>
          linked(graph, names).flatMap: _ =>
            organized(graph).map: _ =>
              PublishPlan(
                moduleUnits(graph, publishing, roots, names) ++ heldUnits(held),
                FileStaleness.Unknown,
              )

  private def words(
      decision: Decision,
      catalog: histos.version.Catalog,
  ): Either[PreviewError, List[ShipWord]] =
    decision match
      case Decision.NoCatalog =>
        Left(PreviewError.Publish(LocalPublishError.NothingToPublish))
      case Decision.Refused(refusals) =>
        Left(PreviewError.Version(VersionError.Refused(refusals)))
      case Decision.Develop(_, lines) =>
        Right(
          lines.toList.map(line => ShipWord.Idle(line.ship, line.compile.render))
        )
      case Decision.Manual(lines) =>
        lines.toList.foldLeft[Either[PreviewError, List[ShipWord]]](Right(Nil)): (acc, line) =>
          acc.flatMap: seen =>
            membersOf(catalog, line.ship).map: mods =>
              seen :+ ShipWord.Publish(line.ship, line.version.value, mods)
      case Decision.Publish(_, lines) =>
        lines.toList.foldLeft[Either[PreviewError, List[ShipWord]]](Right(Nil)): (acc, line) =>
          acc.flatMap: seen =>
            line.action match
              case PublishAction.Hold(compile, reason) =>
                Right(seen :+ ShipWord.Held(line.ship, compile.render, reason))
              case PublishAction.Release(version, _) =>
                membersOf(catalog, line.ship).map: mods =>
                  seen :+ ShipWord.Publish(line.ship, version.render, mods)
              case PublishAction.Snapshot(_, stored, pointer) =>
                membersOf(catalog, line.ship).map: mods =>
                  val storedWord =
                    ShipWord.Publish(line.ship, stored.render, mods)
                  val extra = pointer.toList.map(value => ShipWord.Publish(line.ship, value.render, mods))
                  seen ++ (storedWord :: extra)

  private def membersOf(
      catalog: histos.version.Catalog,
      ship: ShipName,
  ): Either[PreviewError, ::[ProjectName]] =
    catalog.get(ship) match
      case Some(row) => Right(row.members)
      case None      =>
        Left(PreviewError.Publish(LocalPublishError.UnknownShip(ship)))

  private def accept(
      graph: ProjectGraph,
      projects: List[ProjectName],
  ): Either[PreviewError, Unit] =
    if projects.isEmpty then Right(())
    else Selection.select(graph, projects).left.map(PreviewError.Target(_)).map(_ => ())

  private def chosen(
      graph: ProjectGraph,
      projects: List[ProjectName],
      members: Set[ProjectName],
  ): Either[PreviewError, (Set[ProjectName], Set[ProjectName])] =
    if projects.isEmpty then Right((members, members))
    else
      Selection.select(graph, projects) match
        case Left(error)     => Left(PreviewError.Target(error))
        case Right(selected) =>
          val direct = selected.modules.filter(module => !selected.expanded.contains(module.name))
          direct.find(module => !members.contains(module.name)) match
            case Some(module) =>
              Left(
                PreviewError.Publish(LocalPublishError.NotPublished(module.name))
              )
            case None =>
              val keep = selected.modules.filter(module => members.contains(module.name))
              keep match
                case Nil =>
                  Left(PreviewError.Publish(LocalPublishError.NothingToPublish))
                case _ =>
                  val asked = keep.map(_.name).toSet
                  Right((asked, shipClosure(graph, asked, members)))

  private def shipClosure(
      graph: ProjectGraph,
      asked: Set[ProjectName],
      members: Set[ProjectName],
  ): Set[ProjectName] =
    def step(seen: Set[ProjectName], more: Set[ProjectName]): Set[ProjectName] =
      if more.isEmpty then seen
      else
        val next = more.flatMap: name =>
          graph.get(name) match
            case Some(module: Module) =>
              upstream(module).filter(members.contains).toSet
            case _ => Set.empty
        step(seen ++ more, next -- seen)
    step(Set.empty, asked)

  private def upstream(module: Module): List[ProjectName] =
    module.platforms.all.flatMap(module.upstream).distinct

  private def linked(
      graph: ProjectGraph,
      chosen: Set[ProjectName],
  ): Either[PreviewError, Unit] =
    val modules = graph.projects.collect:
      case module: Module if chosen.contains(module.name) => module
    modules.foldLeft[Either[PreviewError, Unit]](Right(())): (acc, module) =>
      acc.flatMap: _ =>
        upstream(module).find(name => !chosen.contains(name)) match
          case Some(dependency) =>
            Left(
              PreviewError.Publish(
                LocalPublishError.UnpublishedDependency(module.name, dependency)
              )
            )
          case None => Right(())

  private def organized(graph: ProjectGraph): Either[PreviewError, Unit] =
    graph.organization match
      case Some(_) => Right(())
      case None    =>
        Left(PreviewError.Publish(LocalPublishError.NoOrganization))

  private def moduleUnits(
      graph: ProjectGraph,
      publishing: List[ShipWord.Publish],
      roots: Set[ProjectName],
      chosen: Set[ProjectName],
  ): List[PublishUnit] =
    publishing.flatMap: word =>
      val names = word.members.toList.toSet
      graph.projects.collect:
        case module: Module if names.contains(module.name) && chosen.contains(module.name) =>
          val reason =
            if roots.contains(module.name) then Reason.Selected
            else Reason.Upstream
          PublishUnit.Module(module.name, word.ship, word.revision, reason)

  private def heldUnits(held: List[ShipWord.Held]): List[PublishUnit] =
    held.map(word => PublishUnit.Held(word.ship, word.compile, word.reason))

  private def fullRoots(repo: Path): List[Path] =
    (Products.moduleRoots ++ Products.sharedRoots).map(name => dot(repo).resolve(name))

  private def moduleRoots(repo: Path, module: Module): List[Path] =
    Products.moduleRoots.map(name => dot(repo).resolve(name).resolve(module.name.value))

  private def dot(repo: Path): Path = repo.resolve(".histos")

  private def taskOnly(
      id: String,
      platform: Option[Platform],
  ): Either[PreviewError, RunPlan] =
    platform match
      case Some(_) =>
        Left(PreviewError.Run(RunMiss.BadId(s"run $id does not take a platform")))
      case None =>
        Right(RunPlan(RunUnit.Task(id), FileStaleness.Unknown))

  private def pluginTask(
      graph: ProjectGraph,
      id: String,
      platform: Option[Platform],
  ): Either[PreviewError, RunPlan] =
    platform match
      case Some(_) =>
        Left(PreviewError.Run(RunMiss.BadId(s"run $id does not take a platform")))
      case None =>
        PluginTaskId.parse(id) match
          case Left(error) =>
            Left(PreviewError.Run(RunMiss.BadId(error.message)))
          case Right(found) =>
            val known = graph.plugins.exists: plugin =>
              plugin.id == found.plugin &&
                plugin.contribution.tasks.exists(_.name == found.name)
            if known then Right(RunPlan(RunUnit.Task(found.render), FileStaleness.Unknown))
            else Left(PreviewError.Run(RunMiss.UnknownTask(found.render)))

  private def runModule(
      graph: ProjectGraph,
      id: String,
      platform: Option[Platform],
  ): Either[PreviewError, RunPlan] =
    ProjectName.from(id) match
      case Left(error) => Left(PreviewError.Run(RunMiss.BadId(error.message)))
      case Right(name) =>
        graph.get(name) match
          case None               => Left(PreviewError.Run(RunMiss.Absent(id)))
          case Some(_: Aggregate) =>
            Left(PreviewError.Run(RunMiss.Aggregate(name)))
          case Some(module: Module) =>
            module.application match
              case None    => Left(PreviewError.Run(RunMiss.NoMain(name)))
              case Some(_) =>
                pick(module, platform).map: chosen =>
                  val modules = graph.projects.collect { case item: Module =>
                    item
                  }
                  RunPlan(
                    RunUnit.Module(
                      module.name,
                      chosen,
                      Module.chain(module, modules, chosen).map(_.name),
                    ),
                    FileStaleness.Unknown,
                  )

  private def pick(
      module: Module,
      choice: Option[Platform],
  ): Either[PreviewError, Platform] =
    val built = module.platforms.all
    choice match
      case Some(platform) if built.contains(platform) => Right(platform)
      case Some(platform)                             =>
        Left(PreviewError.Run(RunMiss.Unbuilt(module.name, platform, built)))
      case None =>
        built match
          case only :: Nil => Right(only)
          case many        =>
            Left(PreviewError.Run(RunMiss.Choose(module.name, many)))

  private def renderChosen(repo: Path, unit: PlanUnit.Chosen): List[String] =
    val names     = unit.upstreams.map(_.value).mkString(" ")
    val upstreams = if names.isEmpty then "upstreams" else s"upstreams $names"
    val sources   =
      if unit.sources.isEmpty then List("sources")
      else unit.sources.map(path => s"sources ${relative(repo, path)}")
    List(
      s"${unit.module.value} ${unit.version.value} ${unit.platform.token} ${unit.reason.word}",
      upstreams,
    ) ++ sources

  private def stalenessLine(staleness: FileStaleness): String =
    staleness match
      case FileStaleness.Unknown => "file-level staleness: unknown"

  private def relative(repo: Path, path: Path): String =
    val root  = repo.toAbsolutePath.normalize
    val file  = path.toAbsolutePath.normalize
    val shown = if file.startsWith(root) then root.relativize(file) else file
    shown.iterator.asScala.map(_.toString).mkString("/")

  private def rootFault(repo: Path, error: GraphCompileError): PreviewError =
    error match
      case GraphCompileError.RootRead(path) =>
        PreviewError.Sources(IoFailure(path, IoFault.Stat))
      case GraphCompileError.Runtime(_) | GraphCompileError.Target(_) | GraphCompileError.UnknownCell(_, _) |
          GraphCompileError.NoRoot(_) | GraphCompileError.NoSources(_, _) |
          GraphCompileError.MissingUpstream(_, _, _, _) | GraphCompileError.Cycle(_) | GraphCompileError.Lock(_) |
          GraphCompileError.LockRead(_) | GraphCompileError.Output(_) | GraphCompileError.Failed(_, _, _, _) |
          GraphCompileError.Cells(_) =>
        PreviewError.Sources(IoFailure(repo, IoFault.Stat))

  private def sourceFault(root: Path, error: CompileError): PreviewError =
    error match
      case CompileError.Io(failure) => PreviewError.Sources(failure)
      case CompileError.Resolved(_) | CompileError.UnsupportedPlatform(_) | CompileError.UnsupportedScala(_) |
          CompileError.NoSources | CompileError.Banned(_, _) | CompileError.Failed(_) | CompileError.Toolchain(_) |
          CompileError.JsLibrary(_) | CompileError.NativeFetch(_) | CompileError.NativeOrigin(_) |
          CompileError.NativeLibrary(_) =>
        PreviewError.Sources(IoFailure(root, IoFault.Walk))

  private def testFault(repo: Path, error: ModuleTestError): PreviewError =
    error match
      case ModuleTestError.Target(found)                            => PreviewError.Target(found)
      case ModuleTestError.Compile(GraphCompileError.Cycle(module)) =>
        PreviewError.Cycle(module)
      case ModuleTestError.Compile(GraphCompileError.Target(found)) =>
        PreviewError.Target(found)
      case ModuleTestError.Compile(GraphCompileError.RootRead(path)) =>
        PreviewError.Sources(IoFailure(path, IoFault.Stat))
      case ModuleTestError.Sources(_, _, CompileError.Io(failure)) =>
        PreviewError.Sources(failure)
      case ModuleTestError.Root(path) =>
        PreviewError.Sources(IoFailure(path, IoFault.Stat))
      case ModuleTestError.Pattern(error) =>
        PreviewError.Pattern(error)
      case ModuleTestError.Tool(_, _, _) | ModuleTestError.Compile(_) | ModuleTestError.Lock(_) |
          ModuleTestError.LockRead(_) | ModuleTestError.Sources(_, _, _) | ModuleTestError.MissingSources(_, _) |
          ModuleTestError.MissingProvider(_, _) | ModuleTestError.ProviderDependency(_, _) |
          ModuleTestError.NoneDiscovered(_, _) | ModuleTestError.NoneMatched(_) | ModuleTestError.Mixed(_) |
          ModuleTestError.Process(_, _, _) | ModuleTestError.Main(_) | ModuleTestError.Absent(_) |
          ModuleTestError.MissingUpstream(_, _, _) | ModuleTestError.Work(_) | ModuleTestError.Runner(_) |
          ModuleTestError.LinkFailed(_, _, _, _) | ModuleTestError.NoNode(_, _, _) | ModuleTestError.Stamp(_) |
          ModuleTestError.Record(_) =>
        PreviewError.Sources(IoFailure(repo, IoFault.Stat))
end Plan
