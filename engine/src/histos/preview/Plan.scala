package histos.preview

import histos.clean.Products
import histos.plugin.PluginTaskId
import histos.central.SnapshotRepository
import histos.publish.{LocalPublishError, PublishName, PublishSelect, PublishWrite, SessionSettle, ShipWrite}
import histos.select.{SelectError, Selection}
import histos.task.TaskId
import histos.testrun.SuitePattern
import histos.plugin.SourceSet
import histos.build.{BuildDag, BuildDagError, BuildNode, ExecutionPlan}
import histos.zinc.{GeneratedRoots, StageFiles, StageRoot, StageRootError, StageRoots}
import histos.{Cell, CompilationStage}
import histos.version.{Decision, HoldReason, PublishAction, ShipName, VersionError, VersionTask}
import histos.zinc.{AnalysisKind, FileStaleness, SourceStale}
import histos.{Aggregate, Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.Path

import scala.jdk.CollectionConverters.*

import zio.{IO, UIO, ZIO}

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

enum PlanUnit:
  case Chosen(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      reason: Reason,
      upstreams: List[ProjectName],
      sources: List[Path],
      staleness: FileStaleness,
      quick: Option[QuickAnswer],
  )
  case NotSelected(module: ProjectName)

final case class CompilePlan(units: List[PlanUnit])

final case class TestPlan(units: List[PlanUnit])

enum PublishUnit:
  case Module(
      module: ProjectName,
      ship: ShipName,
      revision: String,
      reason: Reason,
  )
  case Held(ship: ShipName, compile: String, reason: HoldReason)
  case Idle(ship: ShipName, compile: String)
  case Skipped(module: ProjectName)
  case Refused(ship: ShipName, dependency: String)

final case class PublishPlan(place: String, units: List[PublishUnit])

/** What `clean` would delete. [[StoreFate.Forgotten]] is `clean --full`: this repo's content-store entries, with no
  * byte count.
  */
enum StoreFate:
  case Kept
  case Forgotten

final case class CleanPlan(roots: List[Path], store: StoreFate)

enum RunUnit:
  case Module(
      module: ProjectName,
      platform: Platform,
      upstreams: List[ProjectName],
  )
  case Task(id: String)

final case class RunPlan(unit: RunUnit)

final case class ProjectedStage(stage: CompilationStage, roots: List[StageRoot], files: StageFiles)
final case class StageProjection(plan: ExecutionPlan, stages: List[ProjectedStage])

object Plan:
  type ProjectionError = PreviewError | BuildDagError | StageRootError

  def failure(error: ProjectionError): PreviewError = error match
    case error: PreviewError   => error
    case error: BuildDagError  => PreviewError.Scoped(histos.testrun.TestFailures.graph(error))
    case error: StageRootError => PreviewError.Scoped(histos.testrun.TestFailures.root(error))

  def project(
      repo: Path,
      graph: ProjectGraph,
      nodes: List[BuildNode],
      generated: GeneratedRoots,
  ): IO[BuildDagError | StageRootError, StageProjection] =
    val inventory = StageRoots.of(graph)
    for
      dag    <- ZIO.fromEither(BuildDag.plan(graph, nodes))
      stages <- ZIO.foreach(dag.nodes.collect { case BuildNode.Compile(stage) => stage }) { stage =>
        for
          roots <- ZIO.fromEither(inventory.inventory(stage))
          files <- inventory.read(repo, stage, generated)
        yield ProjectedStage(stage, roots, files)
      }
    yield StageProjection(dag, stages)

  def show(
      repo: Path,
      graph: ProjectGraph,
      projects: List[ProjectName],
      generated: GeneratedRoots = GeneratedRoots.Existing,
      platforms: histos.PlatformSelection = histos.PlatformSelection.All,
  ): IO[ProjectionError, String] =
    modulePlan(repo, graph, projects, Tree.Main, generated, platforms).map(units => render(repo, CompilePlan(units)))

  def compile(
      repo: Path,
      graph: ProjectGraph,
      projects: List[ProjectName],
      generated: GeneratedRoots = GeneratedRoots.Existing,
  ): IO[ProjectionError, CompilePlan] =
    modulePlan(repo, graph, projects, Tree.Main, generated).map(CompilePlan(_))

  def test(
      repo: Path,
      graph: ProjectGraph,
      projects: List[ProjectName],
      generated: GeneratedRoots = GeneratedRoots.Existing,
  ): IO[ProjectionError, TestPlan] =
    modulePlan(repo, graph, projects, Tree.Test, generated).map(TestPlan(_))

  def testSelection(
      repo: Path,
      graph: ProjectGraph,
      selection: histos.TestSelection,
      generated: GeneratedRoots = GeneratedRoots.Existing,
  ): IO[ProjectionError, TestPlan] =
    modulePlan(repo, graph, Nil, Tree.Test, generated, selection.platforms, Some(selection)).map(TestPlan(_))

  /** Drop quick-answer names `--only` would not run. The plan's modules stay. */
  def narrow(plan: TestPlan, patterns: List[SuitePattern]): TestPlan =
    if patterns.isEmpty then plan
    else
      TestPlan(
        plan.units.map:
          case chosen: PlanUnit.Chosen =>
            chosen.copy(quick = chosen.quick.map(answer => QuickAnswer.limited(answer, patterns)))
          case other => other
      )

  /** Names the snapshot coordinate a local write would install. Writes no jar. The place is Coursier's `ivy2Local`. */
  def publishLocal(
      repo: Path,
      graph: ProjectGraph,
      tokens: List[String],
      env: Map[String, String],
  ): IO[PreviewError, PublishPlan] =
    ZIO
      .fromEither(PublishSelect.resolve(graph, tokens))
      .mapError(PreviewError.Publish(_))
      .flatMap(names => planned(repo, graph, names, env, Place.Local))

  /** Names the snapshot coordinate a snapshot write would store. Names Central. Writes no jar and opens no repository.
    */
  def publishSnapshots(
      repo: Path,
      graph: ProjectGraph,
      tokens: List[String],
      env: Map[String, String],
  ): IO[PreviewError, PublishPlan] =
    ZIO
      .fromEither(PublishSelect.resolve(graph, tokens))
      .mapError(PreviewError.Publish(_))
      .flatMap(names => planned(repo, graph, names, env, Place.Snapshots))

  private def planned(
      repo: Path,
      graph: ProjectGraph,
      names: List[PublishName],
      env: Map[String, String],
      place: Place,
  ): IO[PreviewError, PublishPlan] =
    val write = place match
      case Place.Local     => PublishWrite.Local
      case Place.Snapshots => PublishWrite.Snapshots
    SessionSettle
      .prepare(repo, graph.catalog, env, write)
      .mapError(PreviewError.Publish(_))
      .flatMap: settled =>
        VersionTask
          .decide(graph, repo, settled)
          .mapError(PreviewError.Version(_))
          .flatMap(decision => ZIO.fromEither(publishPlan(graph, names, decision, place)))

  /** Every product root. Does not read the graph and does not delete. */
  def cleanAll(repo: Path): CleanPlan =
    CleanPlan(fullRoots(repo), StoreFate.Kept)

  /** Includes generated products from the validated graph; named requests use the producer closure. */
  def clean(
      repo: Path,
      graph: ProjectGraph,
      projects: List[ProjectName],
  ): IO[PreviewError | BuildDagError, CleanPlan] =
    ZIO
      .fromEither(Products.project(repo, graph, projects))
      .mapBoth(
        error =>
          error match
            case found: SelectError   => PreviewError.Target(found)
            case found: BuildDagError => found,
        projection => CleanPlan(projection.roots.map(_.path), StoreFate.Kept),
      )

  /** Product roots, plus this repo's content-store entries. Does not delete them and does not count bytes.
    */
  def cleanFull(repo: Path): CleanPlan =
    cleanAll(repo).copy(store = StoreFate.Forgotten)

  def runSelected(
      graph: ProjectGraph,
      id: String,
      platform: Option[histos.PlatformSelection],
      main: Option[histos.MainClass],
  ): Either[PreviewError, RunPlan] =
    if TaskId.from(id).toOption.exists(_.builtIn) || id.contains("/") then
      if platform.nonEmpty || main.nonEmpty then
        Left(PreviewError.Run(RunMiss.BadId(s"run $id does not take module selectors")))
      else run(graph, id, None)
    else
      ProjectName.from(id).left.map(_ => PreviewError.Run(RunMiss.Absent(id))).flatMap { name =>
        graph.get(name) match
          case None                 => Left(PreviewError.Run(RunMiss.Absent(id)))
          case Some(_: Aggregate)   => Left(PreviewError.Run(RunMiss.Aggregate(name)))
          case Some(module: Module) =>
            if main.orElse(module.application).isEmpty then Left(PreviewError.Run(RunMiss.NoMain(name)))
            else
              histos.RunPlatform.resolve(module, platform).left.map(PreviewError.Selection(_)).map { selected =>
                val modules = graph.projects.collect { case module: Module => module }
                RunPlan(RunUnit.Module(name, selected, Module.chain(module, modules, selected).map(_.name)))
              }
      }

  /** The module or task `run` would start. Does not start it. */
  def run(
      graph: ProjectGraph,
      id: String,
      platform: Option[Platform],
  ): Either[PreviewError, RunPlan] =
    if TaskId.from(id).toOption.exists(_.builtIn) then taskOnly(id, platform)
    else if id.contains("/") then pluginTask(graph, id, platform)
    else runModule(graph, id, platform)

  def render(repo: Path, plan: CompilePlan): String =
    renderUnits(repo, plan.units)

  def renderTest(repo: Path, plan: TestPlan): String =
    renderUnits(repo, plan.units)

  def renderPublish(plan: PublishPlan): String =
    val body = plan.units
      .map:
        case PublishUnit.Module(module, ship, revision, reason) =>
          s"${module.value} ${ship.value} $revision ${reason.word}"
        case PublishUnit.Held(ship, compile, reason) =>
          val why = reason match
            case HoldReason.AlreadyReleased => "already-released"
          s"held ${ship.value} $compile $why"
        case PublishUnit.Idle(ship, compile) =>
          s"${ship.value} $compile not published"
        case PublishUnit.Skipped(module) =>
          LocalPublishError.NotPublished(module).message
        case PublishUnit.Refused(ship, dependency) =>
          LocalPublishError.SnapshotDependency(ship, dependency).message
      .mkString("\n")
    if body.isEmpty then plan.place else s"${plan.place}\n$body"

  def renderClean(repo: Path, plan: CleanPlan): String =
    val roots = plan.roots.map(path => relative(repo, path))
    val store = plan.store match
      case StoreFate.Kept      => Nil
      case StoreFate.Forgotten =>
        List("content-store entries for this repo would be forgotten")
    (roots ++ store).mkString("\n")

  def renderRun(plan: RunPlan): String =
    val body = plan.unit match
      case RunUnit.Module(module, platform, upstreams) =>
        val names = upstreams.map(_.value).mkString(" ")
        val line  = if names.isEmpty then "upstreams" else s"upstreams $names"
        List(s"${module.value} ${platform.token}", line)
      case RunUnit.Task(id) => List(id)
    body.mkString("\n")

  private enum Tree:
    case Main
    case Test

  private def modulePlan(
      repo: Path,
      graph: ProjectGraph,
      projects: List[ProjectName],
      tree: Tree,
      generated: GeneratedRoots,
      platforms: histos.PlatformSelection = histos.PlatformSelection.All,
      selection: Option[histos.TestSelection] = None,
  ): IO[ProjectionError, List[PlanUnit]] =
    val modules = graph.projects.collect { case module: Module => module }
    for
      nodes <- tree match
        case Tree.Main =>
          ZIO
            .fromEither(histos.build.BuildRuntime.compileRoots(graph, projects, platforms))
            .mapError(PreviewError.Test(_))
        case Tree.Test =>
          ZIO
            .fromEither(
              histos.testrun.TestCells.plan(graph, selection.getOrElse(histos.TestSelection.named(projects, platforms)))
            )
            .mapError(error => PreviewError.Scoped(histos.testrun.ScopedTestFailure.Selection(error)))
            .map(_.compileStageRoots.map(BuildNode.Compile(_)))
      chosen = nodes.collect { case BuildNode.Compile(stage) => stage.cell.module }.toSet
      projection <- project(repo, graph, nodes, generated)
      listed     <- ZIO.foreach(projection.stages) { projected =>
        val stage = projected.stage
        val cell  = stage.cell
        val found = projected.files.sources.map(_.path)
        val kind  = stage.scope match
          case SourceSet.Main => AnalysisKind.Main
          case SourceSet.Test => AnalysisKind.Test
        val stageTree = stage.scope match
          case SourceSet.Main => Tree.Main
          case SourceSet.Test => Tree.Test
        for
          dependencies <- ZIO.fromEither(projection.plan.dependencies(BuildNode.Compile(stage)))
          stale        <- SourceStale.read(
            repo,
            SourceStale.directory(repo, cell.module, cell.version, cell.platform, kind),
            found,
          )
          quick <- ZIO
            .foreach(modules.find(_.name == cell.module))(module =>
              quickOf(repo, module, modules, cell.version, cell.platform, stageTree, graph)
            )
            .map(_.flatten)
        yield PlanUnit.Chosen(
          cell.module,
          cell.version,
          cell.platform,
          if chosen.contains(cell.module) then Reason.Selected else Reason.Upstream,
          dependencies.collect {
            case BuildNode.Compile(upstream) if upstream.cell.module != cell.module => upstream.cell.module
          }.distinct,
          found,
          stale,
          quick,
        )
      }
    yield listed ++ modules
      .filterNot(module => projection.stages.exists(_.stage.cell.module == module.name))
      .map(module => PlanUnit.NotSelected(module.name))

  private def renderUnits(repo: Path, units: List[PlanUnit]): String =
    units
      .flatMap:
        case chosen: PlanUnit.Chosen => renderChosen(repo, chosen)
        case PlanUnit.NotSelected(_) => Nil
      .mkString("\n")

  private enum ShipWord:
    case Publish(ship: ShipName, revision: String, members: ::[ProjectName])
    case Held(ship: ShipName, compile: String, reason: HoldReason)
    case Idle(ship: ShipName, compile: String)

  private enum Place:
    case Local
    case Snapshots

    def text: String = this match
      case Local     => "ivy2Local"
      case Snapshots => SnapshotRepository.central.root.render
  end Place

  private def publishPlan(
      graph: ProjectGraph,
      names: List[PublishName],
      decision: Decision,
      place: Place,
  ): Either[PreviewError, PublishPlan] =
    words(decision, graph.catalog).flatMap: found =>
      val idle       = found.collect { case word: ShipWord.Idle => word }
      val held       = found.collect { case word: ShipWord.Held => word }
      val publishing = found.collect { case word: ShipWord.Publish => word }
      val shown      = place.text
      if idle.nonEmpty then Right(PublishPlan(shown, idle.map(word => PublishUnit.Idle(word.ship, word.compile))))
      else if publishing.isEmpty then Right(PublishPlan(shown, heldUnits(held)))
      else
        PublishSelect
          .pick(graph, names, writes(publishing))
          .left
          .map(PreviewError.Publish(_))
          .flatMap: picked =>
            organized(graph).map: _ =>
              val units =
                skippedUnits(picked.skipped) ++
                  refusedUnits(picked.refused) ++
                  moduleUnits(graph, publishing, picked.roots, picked.modules) ++
                  heldUnits(held)
              PublishPlan(shown, units)

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
        Right(lines.toList.map(line => ShipWord.Idle(line.ship, line.compile.render)))
      case Decision.Publish(_, lines) =>
        lines.toList.foldLeft[Either[PreviewError, List[ShipWord]]](Right(Nil)): (acc, line) =>
          acc.flatMap: seen =>
            line.action match
              case PublishAction.Hold(compile, reason) =>
                Right(seen :+ ShipWord.Held(line.ship, compile.render, reason))
              case PublishAction.Release(version) =>
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

  private def writes(publishing: List[ShipWord.Publish]): List[ShipWrite] =
    publishing.foldLeft(List.empty[ShipWrite]): (seen, word) =>
      val snapshot = PublishSelect.snapshotCoordinate(word.revision)
      seen.find(_.ship == word.ship) match
        case None    => seen :+ ShipWrite(word.ship, word.members.toList, snapshot)
        case Some(_) =>
          seen.map: item =>
            if item.ship == word.ship then item.copy(snapshot = item.snapshot || snapshot)
            else item

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

  private def skippedUnits(names: List[ProjectName]): List[PublishUnit] =
    names.map(PublishUnit.Skipped(_))

  private def refusedUnits(refused: List[LocalPublishError.SnapshotDependency]): List[PublishUnit] =
    refused.map(error => PublishUnit.Refused(error.ship, error.dependency))

  private def heldUnits(held: List[ShipWord.Held]): List[PublishUnit] =
    held.map(word => PublishUnit.Held(word.ship, word.compile, word.reason))

  private def fullRoots(repo: Path): List[Path] =
    (Products.moduleRoots ++ Products.sharedRoots).map(name => dot(repo).resolve(name))

  private def dot(repo: Path): Path = repo.resolve(".histos")

  private def taskOnly(
      id: String,
      platform: Option[Platform],
  ): Either[PreviewError, RunPlan] =
    platform match
      case Some(_) =>
        Left(PreviewError.Run(RunMiss.BadId(s"run $id does not take a platform")))
      case None =>
        Right(RunPlan(RunUnit.Task(id)))

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
            if known then Right(RunPlan(RunUnit.Task(found.render)))
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
          case None =>
            Left(
              PreviewError.Run(RunMiss.Absent(id, histos.doctor.Suggest.runs(id, graph.projects.map(_.name.value))))
            )
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
                    )
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
    ) ++ sources ++ stalenessLines(repo, unit.staleness) ++ quickLines(unit.quick)

  private def stalenessLines(repo: Path, staleness: FileStaleness): List[String] =
    staleness match
      case FileStaleness.Initial(sources) =>
        "no previous compile" :: sources.map(path => s"stale ${relative(repo, path)}")
      case FileStaleness.Known(Nil) =>
        List("fresh")
      case FileStaleness.Known(stale) =>
        stale.map(path => s"stale ${relative(repo, path)}")
      case FileStaleness.Unknown =>
        List("file-level staleness: unknown")

  private def quickLines(quick: Option[QuickAnswer]): List[String] =
    quick match
      case None                         => Nil
      case Some(QuickAnswer.Uncompiled) =>
        List("quick: no suite would be skipped")
      case Some(QuickAnswer.Named(skip, run, unknown)) =>
        skip.map(name => s"quick skip $name") ++
          run.map(name => s"quick run $name") ++
          unknown.map(name => s"quick: not known to be skippable $name")
      case Some(QuickAnswer.Partial(skip)) =>
        skip.map(name => s"quick skip $name") :+ "quick: the rest are not known to be skippable"

  private def quickOf(
      repo: Path,
      module: Module,
      modules: List[Module],
      version: ScalaVersion,
      platform: Platform,
      tree: Tree,
      graph: ProjectGraph,
  ): UIO[Option[QuickAnswer]] =
    tree match
      case Tree.Main => ZIO.succeed(None)
      case Tree.Test =>
        QuickAnswer.read(repo, module, modules, version, platform, graph).map(Some(_))

  private def relative(repo: Path, path: Path): String =
    val root  = repo.toAbsolutePath.normalize
    val file  = path.toAbsolutePath.normalize
    val shown = if file.startsWith(root) then root.relativize(file) else file
    shown.iterator.asScala.map(_.toString).mkString("/")

end Plan
