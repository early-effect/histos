package histos.workflow

import histos.doctor.Suggest
import histos.plugin.{CiExtra, CiHeader}
import histos.publish.DistLayout
import histos.schedule.{CacheConfig, CacheNamespace}
import histos.Promote
import histos.version.{Catalog, Ship}
import histos.workspace.{IoFailure, IoFault}
import histos.{
  Aggregate,
  Axis,
  BuildError,
  ClientLink,
  CoverageFloors,
  CoverageJob,
  Formatting,
  DocCheck,
  FollowUp,
  JobTimeout,
  Module,
  NodeFloor,
  Platform,
  ProjectGraph,
  ProjectName,
  Publishing,
  Resolution,
  RunnerTools,
  ToolRevision,
  VerifyTask,
}

import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}
import zio.json.*

/** A pull request names the projects that changed. Anything else runs the full graph. */
enum CiEvent:
  case Other
  case PullRequest(changed: Set[ProjectName])

/** Each rendered request retains the local planner's requested suites and execution groups. */
final case class TestJob(selection: histos.TestSelection, cells: histos.testrun.TestCellPlan)

/** One file `renderAll` emits. `name` is the file name under `.github/workflows`, not a path. */
final case class RenderedWorkflow(name: String, body: String)

object Workflow:
  val fileName: String = "ci.yml"

  /** Owned like a scheduled `histos-*.yml`. An empty catalog does not emit it, so update deletes a stale copy. */
  val releaseName: String = "histos-release.yml"

  /** `histos-` so update deletes a stale copy when no ship names an asset. Dispatch publishes snapshots. On the default
    * branch that command also uploads the dist channel.
    */
  val snapshotsName: String = "histos-snapshots.yml"

  /** `histos-` so update deletes a stale copy when `followUp` is Finished, on the same orphan pass as the release file.
    */
  val promoteName: String = "histos-promote.yml"

  /** `histos-` so update deletes a stale copy when no job asks for a manual deploy. */
  val pagesName: String = "histos-pages.yml"

  def path(repo: Path): Path =
    file(repo, fileName)

  /** A workflow file under `.github/workflows`. `name` is the file name, not a path. */
  def file(repo: Path, name: String): Path =
    repo.resolve(".github").resolve("workflows").resolve(name)

  private def workflowPath(name: String): String =
    s".github/workflows/$name"

  private def unknownProject(graph: ProjectGraph, name: ProjectName): CiError =
    CiError.Unknown(name, Suggest.projects(name, graph.projects.map(_.name)))

  private def firstDiff(rendered: String, committed: String): String =
    val want  = rendered.split("\n", -1).toVector
    val have  = committed.split("\n", -1).toVector
    val index = (0 until want.length.max(have.length)).find(i => want.lift(i) != have.lift(i))
    index match
      case None        => "the files differ"
      case Some(found) =>
        val removed = have.lift(found).getOrElse("")
        val added   = want.lift(found).getOrElse("")
        s"line ${found + 1}\n- $removed\n+ $added"

  /** Axes partition existing execution groups, never a newly focused subset of their members. */
  def plan(graph: ProjectGraph, event: CiEvent): Either[CiError | histos.testrun.TestSelectionError, List[TestJob]] =
    for
      _         <- axes(graph)
      selection <- event match
        case CiEvent.Other                => Right(histos.TestSelection())
        case CiEvent.PullRequest(changed) =>
          affected(graph, changed).map(names => histos.TestSelection(affected = Some(names)))
      initial <- histos.testrun.TestCells.plan(graph, selection)
      rows    <-
        if initial.requestedCells.isEmpty then Right(Nil)
        else if graph.splits.axes.isEmpty then Right(List(TestJob(selection, initial)))
        else partition(graph, selection, initial)
    yield rows

  private def axes(graph: ProjectGraph): Either[CiError, Unit] =
    if graph.splits.axes.contains(Axis.ScalaVersion) then Left(CiError.UnsupportedAxis(Axis.ScalaVersion))
    else Right(())

  private def partition(
      graph: ProjectGraph,
      selection: histos.TestSelection,
      initial: histos.testrun.TestCellPlan,
  ): Either[CiError | histos.testrun.TestSelectionError, List[TestJob]] =
    import histos.testrun.TestExecutionGroup
    def platform(group: TestExecutionGroup): Platform = group match
      case TestExecutionGroup.Direct(cell)              => cell.platform
      case TestExecutionGroup.NativeAggregate(owner, _) => owner.platform
    def names(groups: List[TestExecutionGroup]): List[ProjectName] =
      groups.flatMap(_.requestedCells).map(_.module).distinct
    val buckets = initial.executionGroups.foldLeft(List.empty[(Option[Platform], List[TestExecutionGroup])]):
      (rows, group) =>
        val key = Option.when(graph.splits.axes.contains(Axis.Platform))(platform(group))
        rows.find(_._1 == key) match
          case None    => rows :+ (key, List(group))
          case Some(_) =>
            rows.map((found, groups) => if found == key then (found, groups :+ group) else (found, groups))
    val components = buckets.flatMap: (key, groups) =>
      val rows = if !graph.splits.axes.contains(Axis.Module) then List(groups)
      else
        groups.foldLeft(List.empty[List[TestExecutionGroup]]): (rows, group) =>
          def overlaps(row: List[TestExecutionGroup]): Boolean = names(row).exists(names(List(group)).contains)
          val index                                            = rows.indexWhere(overlaps)
          if index < 0 then rows :+ List(group)
          else
            val merged = rows.filter(overlaps).flatten :+ group
            rows.take(index) ++ List(merged) ++ rows.drop(index).filterNot(overlaps)
      rows.map(key -> _)
    components.foldLeft[Either[CiError | histos.testrun.TestSelectionError, List[TestJob]]](Right(Nil)): (acc, row) =>
      val (key, groups) = row
      val targets       = if graph.splits.axes.contains(Axis.Module) then histos.TestTargets.Filtered(names(groups))
      else selection.targets
      val requested = histos.TestSelection(
        targets,
        key.fold(histos.PlatformSelection.All)(histos.PlatformSelection.only(_)),
        selection.affected,
      )
      for
        previous <- acc
        planned  <- histos.testrun.TestCells.plan(graph, requested)
        roots = initial.compileStageRoots.filter(stage =>
          groups.exists(group =>
            group.requestedCells.contains(stage.cell) || (group match
              case TestExecutionGroup.Direct(_)                 => false
              case TestExecutionGroup.NativeAggregate(owner, _) => owner == stage.cell)
          )
        )
        _ <-
          if planned.requestedCells.toSet == groups
              .flatMap(_.requestedCells)
              .toSet && planned.executionGroups.toSet == groups.toSet && planned.compileStageRoots.toSet == roots.toSet
          then Right(())
          else Left(CiError.InvalidPartition(requested))
      yield previous :+ TestJob(requested, planned)

  /** The same pure contract used by local execution and rendered argv verification. */
  def cells(
      graph: ProjectGraph,
      selection: histos.TestSelection,
  ): Either[histos.testrun.TestSelectionError, histos.testrun.TestCellPlan] =
    histos.testrun.TestCells.plan(graph, selection)

  /** No names is the whole graph. Names are modules, in the order given, without adding dependents. */
  def target(graph: ProjectGraph, raw: List[String]): Either[CiError, histos.TestTargets] =
    if raw.isEmpty then Right(histos.TestTargets.All)
    else
      raw
        .foldLeft[Either[CiError, List[ProjectName]]](Right(Nil)): (acc, item) =>
          acc.flatMap: seen =>
            ProjectName.from(item) match
              case Left(_)                            => Left(CiError.BadName(item))
              case Right(name) if seen.contains(name) => Left(CiError.Duplicate(name))
              case Right(name)                        =>
                graph.get(name) match
                  case None               => Left(unknownProject(graph, name))
                  case Some(_: Aggregate) => Left(CiError.NotModule(name))
                  case Some(_: Module)    => Right(seen :+ name)
        .flatMap:
          case first :: rest => Right(histos.TestTargets.Named(first, rest))
          case Nil           => Right(histos.TestTargets.All)

  def render(graph: ProjectGraph, lock: NpmLock)(using
      CiBranch,
      RenderingShip,
  ): Either[CiError | BuildError | histos.testrun.TestSelectionError, String] =
    prepared(graph, lock).map(_._1)

  /** `ci.yml`, `histos-release.yml` when the catalog names a ship, `histos-promote.yml` when that release's `followUp`
    * is Deployment, `histos-snapshots.yml` when a ship names an asset, then one file per dispatch, cron, or trigger
    * that must not wake `ci.yml`.
    */
  def renderAll(
      graph: ProjectGraph,
      sona: Publishing,
      lock: NpmLock,
  )(using
      CiBranch,
      RenderingShip,
  ): Either[CiError | BuildError | histos.testrun.TestSelectionError, List[RenderedWorkflow]] =
    for
      (body, files) <- prepared(graph, lock)
      release       <- releaseWorkflow(graph, sona)
      snapshots     <- snapshotWorkflow(graph)
    yield
      val promote =
        release match
          case None => Nil
          case _    => promoteWorkflow(graph, sona).toList
      RenderedWorkflow(fileName, body) :: release.toList ++ promote ++ snapshots.toList ++ SupplyYaml.files(graph) ++
        files

  private def prepared(
      graph: ProjectGraph,
      lock: NpmLock,
  )(using
      branch: CiBranch
  )(using
      RenderingShip
  ): Either[CiError | BuildError | histos.testrun.TestSelectionError, (String, List[RenderedWorkflow])] =
    for
      _       <- axes(graph)
      _       <- limits(graph)
      _       <- reserved(graph)
      _       <- llvmPinned(graph)
      planned <- JobYaml.render(graph, branch.name)
      jobs    <- plan(graph, CiEvent.Other)
      verify  <- graph.verify
        .foldLeft[Either[histos.testrun.TestSelectionError, List[(VerifyTask, Option[histos.testrun.TestCellPlan])]]](
          Right(Nil)
        ): (acc, task) =>
          for
            previous <- acc
            cells    <- task.run match
              case histos.VerifyRun.Tests(selection) => histos.testrun.TestCells.plan(graph, selection).map(Some(_))
              case histos.VerifyRun.BuiltIn(_) | histos.VerifyRun.Plugin(_) => Right(None)
          yield previous :+ (task, cells)
    yield (document(graph, jobs, planned, branch.name, lock, verify), planned.files)

  private def reserved(graph: ProjectGraph): Either[CiError, Unit] =
    val compatibility = histos.plugin.JobName("compatibility")
    val taken         = graph.verify.exists(_.name == compatibility) || graph.plugins.exists(
      _.contribution.jobs.exists(_.name == compatibility)
    )
    if graph.catalog.ships.nonEmpty && taken then Left(CiError.Fault(histos.CiFault.RenderedJob(compatibility)))
    else Right(())

  private def limits(graph: ProjectGraph): Either[CiError, Unit] =
    graph.ci.timeout match
      case JobTimeout.Unbounded                                  => Right(())
      case JobTimeout.Minutes(count) if JobTimeout.admits(count) => Right(())
      case JobTimeout.Minutes(count)                             => Left(CiError.JobTimeout(count))

  /** Archives this render will curl. A missing pin fails before any workflow text is built. */
  private def llvmPinned(graph: ProjectGraph): Either[CiError, Unit] =
    llvmArchives(graph).foldLeft[Either[CiError, Unit]](Right(())):
      case (left @ Left(_), _) => left
      case (Right(_), name)    => LlvmDigest.of(name).map(_ => ())

  private def llvmArchives(graph: ProjectGraph): List[String] =
    graph.runnerTools.llvm.toList.flatMap: pinned =>
      val version = pinned.value
      val native  = graph.projects
        .collect { case module: Module => module }
        .exists: module =>
          module.platforms.all.contains(Platform.Native)
      val linux =
        if native then List(s"LLVM-$version-Linux-X64.tar.xz") else Nil
      linux.distinct

  def differs(rendered: String, committed: String): Either[CiError, Unit] =
    differsNamed(fileName, rendered, committed)

  /** `name` is the workflow file, such as `ci.yml`. The error names `.github/workflows/<name>`. */
  def differsNamed(name: String, rendered: String, committed: String): Either[CiError, Unit] =
    if rendered == committed then Right(())
    else Left(CiError.Drift(workflowPath(name), firstDiff(rendered, committed)))

  /** A missing `ci.yml` is reported before git is asked for the default branch. */
  def present(repo: Path): IO[CiError, Unit] =
    val path = file(repo, fileName)
    ZIO
      .attemptBlocking(Files.isRegularFile(path))
      .mapError(_ => CiError.Io(IoFailure(path, IoFault.Stat)))
      .flatMap(found => if found then ZIO.unit else ZIO.fail(CiError.Missing(path)))

  def check(repo: Path, graph: ProjectGraph, sona: Publishing)(using
      CiBranch,
      RenderingShip,
  ): IO[CiError | BuildError | histos.testrun.TestSelectionError, Path] =
    for
      lock     <- npmLock(repo)
      rendered <- ZIO.fromEither(renderAll(graph, sona, lock))
      _        <- ZIO.foreachDiscard(rendered)(compare(repo, _))
      extra    <- orphans(repo, rendered.map(_.name).toSet)
      _        <- ZIO.foreachDiscard(extra.headOption): orphan =>
        ZIO.fail(CiError.Drift(workflowPath(fileNameOf(orphan)), "this file is not rendered by the graph"))
      _ <- PinScan.check(repo, graph)
    yield path(repo)

  def update(repo: Path, graph: ProjectGraph, sona: Publishing)(using
      CiBranch,
      RenderingShip,
  ): IO[CiError | BuildError | histos.testrun.TestSelectionError, Path] =
    for
      lock     <- npmLock(repo)
      rendered <- ZIO.fromEither(renderAll(graph, sona, lock))
      _        <- ZIO.foreachDiscard(rendered)(item => write(file(repo, item.name), item.body))
      extra    <- orphans(repo, rendered.map(_.name).toSet)
      _        <- ZIO.foreachDiscard(extra): orphan =>
        ZIO
          .attemptBlocking(Files.deleteIfExists(orphan))
          .mapError(_ => CiError.Io(IoFailure(orphan, IoFault.Delete)))
    yield path(repo)

  /** The process boundary. The command is `./histos test` plus the cell, nothing else. */
  def argv(job: TestJob): List[String] = selectionArgv(job.selection)

  def selectionArgv(selection: histos.TestSelection): List[String] =
    val names = selection.targets match
      case histos.TestTargets.All                => Nil
      case histos.TestTargets.Named(first, rest) => (first :: rest).map(_.value).distinct
      case histos.TestTargets.Filtered(modules)  => List("--suite", modules.map(_.value).distinct.toJson)
    val platforms = selection.platforms match
      case histos.PlatformSelection.All          => Nil
      case histos.PlatformSelection.Only(chosen) => List("--platform", chosen.all.distinct.map(_.token).mkString(","))
    val mask = selection.affected.toList.flatMap(names => List("--affected", names.map(_.value).distinct.toJson))
    List("./histos", "test") ++ names ++ platforms ++ mask

  private def shellArgv(args: List[String]): String =
    args.map(token => if token.startsWith("[") then s"'$token'" else token).mkString(" ")

  private def affected(graph: ProjectGraph, changed: Set[ProjectName]): Either[CiError, List[ProjectName]] =
    val known = graph.projects.map(_.name).toSet
    changed.toList.filter(name => !known.contains(name)).sortBy(_.value) match
      case missing :: _ => Left(unknownProject(graph, missing))
      case Nil          =>
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

  private def document(
      graph: ProjectGraph,
      jobs: List[TestJob],
      planned: JobYaml.Rendered,
      branch: String,
      lock: NpmLock,
      verification: List[(VerifyTask, Option[histos.testrun.TestCellPlan])],
  )(using RenderingShip): String =
    val modules = graph.projects.collect { case module: Module => module }
    val test    = jobs match
      case Nil => Nil
      case _   => testJob(jobs, graph, lock)
    val versions = graph.catalog match
      case Catalog.Empty => Nil
      case _             =>
        val proof = if graph.catalog.ships.nonEmpty then List(compatibilityJob(graph)) else Nil
        List(versionsJob(graph)) ++ proof
    val doc = graph.docCheck match
      case DocCheck.Off => Nil
      case DocCheck.On  => List(docJob(graph))
    val coverage = graph.coverage match
      case CoverageJob.Off        => Nil
      case CoverageJob.Ask(asked) => List(coverageJob(graph, asked))
    val format = graph.formatting match
      case Formatting.Off         => Nil
      case Formatting.Scalafmt(_) => List(formatJob(graph))
    val compile = if compiles(graph) then List(compileJob(graph)) else Nil
    val seed    = if modules.nonEmpty then List(cacheSeed(jobs, graph)) else Nil
    val verify  = verification.map((task, cells) => verifyJob(graph, task, cells))
    val body    =
      format ++ List(affectedJob(graph)) ++ compile ++ test ++ versions ++ doc ++ coverage ++ verify ++ planned.ci ++
        seed ++ List(workflowJob(graph)) ++ List(SupplyYaml.supplyChain(graph)) ++ SupplyYaml.snapshot(graph).toList
    (header(planned.header, graph, branch) ++ body).mkString("\n") + "\n"

  /** Dispatch only. No tag, no inputs, no `if`. The publish step sets `HISTOS_SESSION` to `release`, then lists
    * `secretNames` in that order. A ship that names `.toolchain` uploads the jar, the shim, and the link kit to the
    * existing `dist` release.
    */
  private def releaseWorkflow(
      graph: ProjectGraph,
      sona: Publishing,
  )(using RenderingShip): Either[CiError, Option[RenderedWorkflow]] =
    graph.catalog.ships match
      case Nil   => Right(None)
      case ships => releaseDocument(graph, ships, sona).map(body => Some(RenderedWorkflow(releaseName, body)))

  /** Every ship publishes snapshots. A graph with no ship emits nothing, so update deletes a stale copy. A toolchain
    * ship also publishes the jar and the link kit. Two toolchain ships fail the render.
    */
  private[workflow] def snapshotWorkflow(
      graph: ProjectGraph
  )(using RenderingShip): Either[CiError, Option[RenderedWorkflow]] =
    toolchain(graph).map: _ =>
      if graph.catalog.ships.nonEmpty then Some(RenderedWorkflow(snapshotsName, snapshotDocument(graph)))
      else None

  private val snapshotAuthorization: List[String] = List(
    "        env:",
    "          GH_TOKEN: ${{ github.token }}",
    "          GH_REPO: ${{ github.repository }}",
    "          CI_SHA: ${{ github.event.workflow_run.head_sha || github.sha }}",
    "          CI_BRANCH: ${{ github.event.workflow_run.head_branch || github.ref_name }}",
    "          DEFAULT_BRANCH: ${{ github.event.repository.default_branch }}",
    "          EVENT: ${{ github.event_name }}",
    "        run: |",
    "          set -euo pipefail",
    "          publish=false",
    "          session=snapshot",
    "          pr=",
    "          if [ \"$EVENT\" = workflow_dispatch ] || [ \"$CI_BRANCH\" = \"$DEFAULT_BRANCH\" ]; then",
    "            current=$(gh api \"repos/$GH_REPO/commits/$CI_BRANCH\" --jq .sha)",
    "            [ \"$current\" = \"$CI_SHA\" ] && publish=true",
    "          else",
    "            owner=${GH_REPO%%/*}",
    "            pulls=$(gh api --method GET \"repos/$GH_REPO/pulls\" -f \"head=$owner:$CI_BRANCH\")",
    "            pr=$(printf '%s' \"$pulls\" | jq -r '[.[] | select(.head.sha == env.CI_SHA and .head.repo.full_name == env.GH_REPO and any(.labels[]; .name == \"snapshots\"))] | first | .number // empty')",
    "            if [ -n \"$pr\" ]; then publish=true; session=pr; fi",
    "          fi",
    "          printf 'publish=%s\\nsession=%s\\npr=%s\\n' \"$publish\" \"$session\" \"$pr\" >> \"$GITHUB_OUTPUT\"",
  )

  /** Recheck the current head and PR label immediately before publication. */
  private def snapshotDocument(graph: ProjectGraph)(using RenderingShip): String =
    val verified =
      "github.event.workflow_run.conclusion == 'success' && github.event.workflow_run.head_repository.full_name == github.repository"
    val eligible = s"github.event_name == 'workflow_dispatch' || ($verified)"
    val lines    = List(
      "name: snapshots",
      "on:",
      "  workflow_dispatch:",
      "  workflow_run:",
      "    workflows: [ci]",
      "    types: [completed]",
      "permissions:",
      "  contents: read",
      "jobs:",
      "  eligibility:",
      "    outputs:",
      "      publish: ${{ steps.eligibility.outputs.publish }}",
      s"    if: ${quote(eligible)}",
      s"    runs-on: $Runner",
    ) ++ jobLimit(graph) ++ List(
      "    permissions:",
      "      contents: read",
      "      pull-requests: read",
      "    steps:",
      "      - id: eligibility",
    ) ++ snapshotAuthorization ++ List(
      "  snapshots:",
      "    needs: eligibility",
      "    if: needs.eligibility.outputs.publish == 'true'",
      s"    runs-on: $Runner",
    ) ++ jobLimit(graph) ++ List(
      "    permissions:",
      "      contents: write",
      "      pull-requests: read",
      "    env:",
      "      SONATYPE_USERNAME: ${{ secrets.SONATYPE_USERNAME }}",
      "      SONATYPE_PASSWORD: ${{ secrets.SONATYPE_PASSWORD }}",
      "    steps:",
      "      - name: Require snapshot credentials",
      "        run: test -n \"$SONATYPE_USERNAME\" && test -n \"$SONATYPE_PASSWORD\"",
      s"      - uses: $Checkout",
      "        with:",
      "          ref: ${{ github.event.workflow_run.head_sha || github.sha }}",
    ) ++ coursierCli(graph, "snapshots") ++ List(
      "      - id: authorized"
    ) ++ snapshotAuthorization ++ List(
      s"      - run: ${quote("./histos publish snapshots")}",
      "        if: steps.authorized.outputs.publish == 'true'",
      "        env:",
      "          HISTOS_SESSION: ${{ steps.authorized.outputs.session }}",
      "          HISTOS_PR: ${{ steps.authorized.outputs.pr }}",
      "          HISTOS_SHA: ${{ github.event.workflow_run.head_sha || github.sha }}",
      "          HISTOS_BRANCH: default",
      "          SONATYPE_USERNAME: ${{ secrets.SONATYPE_USERNAME }}",
      "          SONATYPE_PASSWORD: ${{ secrets.SONATYPE_PASSWORD }}",
      "          GITHUB_TOKEN: ${{ github.token }}",
      "          GITHUB_REPOSITORY: ${{ github.repository }}",
    )
    lines.mkString("\n") + "\n"

  /** Promote follows a release file. An empty catalog emits neither, so update deletes a stale copy. */
  private def promoteWorkflow(graph: ProjectGraph, sona: Publishing)(using RenderingShip): Option[RenderedWorkflow] =
    sona.followUp match
      case FollowUp.Finished   => None
      case FollowUp.Deployment => Some(RenderedWorkflow(promoteName, promoteDocument(graph)))

  private def releaseDocument(
      graph: ProjectGraph,
      ships: List[Ship],
      sona: Publishing,
  )(using RenderingShip): Either[CiError, String] =
    toolchain(graph).flatMap: install =>
      val header = List(
        "name: release",
        "on:",
        "  workflow_dispatch:",
        "permissions:",
        "  contents: read",
        "jobs:",
      )
      val jobs = ships.foldLeft[Either[CiError, List[String]]](Right(Nil)): (seen, ship) =>
        seen.flatMap: lines =>
          val shape =
            if install.exists(_.ship.name == ship.name) then ReleaseShape.WithInstall else ReleaseShape.Publish
          releaseJob(graph, ship, sona, shape).map(lines ++ _)
      install match
        case None        => jobs.map(lines => (header ++ lines).mkString("\n") + "\n")
        case Some(found) =>
          for
            lines   <- jobs
            install <- installJob(graph, ships, found)
          yield (header ++ lines ++ install).mkString("\n") + "\n"

  private def releaseJob(
      graph: ProjectGraph,
      ship: Ship,
      sona: Publishing,
      shape: ReleaseShape,
  )(using RenderingShip): Either[CiError, List[String]] =
    val name = ship.name.value
    val head = List(
      s"  release-$name:",
      s"    runs-on: $Runner",
    ) ++ jobLimit(graph) ++ List(
      "    permissions:"
    ) ++ releasePermissions(shape) ++ List(
      "    steps:",
      s"      - uses: $Checkout",
    ) ++ coursierCli(graph, s"release-$name")
    val publish = List(
      s"      - run: ${quote(s"./histos publish release $name")}",
      "        env:",
      "          HISTOS_SESSION: release",
    ) ++ sona.secretNames.map(secret)
    val tail = shape match
      case ReleaseShape.Publish     => Nil
      case ReleaseShape.WithInstall => kitArtifact ++ shimArtifact
    Right(head ++ publish ++ tail)

  /** `upload-artifact` writes through the Actions API. A permissions block that omits `actions` sets it to none. */
  private def releasePermissions(shape: ReleaseShape): List[String] =
    shape match
      case ReleaseShape.Publish     => List("      contents: read")
      case ReleaseShape.WithInstall =>
        List(
          "      actions: write",
          "      contents: read",
        )

  /** The link kit `publish release` compiled, uploaded beside the shim. */
  private def kitArtifact: List[String] =
    List(
      s"      - uses: ${ActionPins.uploadArtifact.line}",
      "        with:",
      "          name: client-kit",
      s"          path: ${DistLayout.BuiltKit}",
      "          if-no-files-found: error",
    )

  /** The one toolchain ship. None when no ship names a toolchain. Two ships fail. */
  private final case class Install(ship: Ship)

  /** One ship with `.toolchain`. None when no ship names a toolchain. Two ships fail. */
  private def toolchain(graph: ProjectGraph): Either[CiError, Option[Install]] =
    graph.catalog.ships.filter(_.toolchain.isDefined) match
      case Nil         => Right(None)
      case ship :: Nil => Right(Some(Install(ship)))
      case ship :: _   => Left(CiError.SnapshotClients(ship.name))

  private def shipVersion(ship: Ship): String =
    ship match
      case line: Ship.Line => line.number.render

  private def installJob(
      graph: ProjectGraph,
      ships: List[Ship],
      install: Install,
  )(using RenderingShip): Either[CiError, List[String]] =
    val needed = ships.map(row => s"release-${row.name.value}")
    install.ship.toolchain match
      case None         => Left(CiError.SnapshotClients(install.ship.name))
      case Some(linked) =>
        coordinate(graph, linked.launcher).map: (group, artifact) =>
          distJob(graph, shipVersion(install.ship), group, artifact, needed)

  /** Organization and published artifact of the launcher. The install manifest names this coordinate. */
  private def coordinate(graph: ProjectGraph, name: ProjectName): Either[CiError, (String, String)] =
    graph.organization match
      case None =>
        Left(CiError.Pins("the build has no organization, so the install manifest cannot name a coordinate"))
      case Some(org) =>
        graph.get(name) match
          case Some(module: Module) =>
            val artifact = module.published.fold(module.name.value)(_.value)
            Right(org.value -> artifact)
          case _ => Left(CiError.NotModule(name))

  /** The installer the README curls. A piped `sh` cannot see its own file, so the release publishes this copy. */
  private def shimArtifact: List[String] =
    List(
      s"      - uses: ${ActionPins.uploadArtifact.line}",
      "        with:",
      "          name: shim",
      "          path: install/histos",
      "          if-no-files-found: error",
    )

  private def distJob(
      graph: ProjectGraph,
      version: String,
      group: String,
      artifact: String,
      needed: List[String],
  )(using RenderingShip): List[String] =
    List(
      "  install:",
      s"    needs: [${needed.mkString(", ")}]",
      s"    runs-on: $Runner",
    ) ++ jobLimit(graph) ++ List(
      "    permissions:",
      "      actions: read",
      "      contents: write",
      "    steps:",
      s"      - uses: $Checkout",
    ) ++ CiInstall.steps(graph) ++ SupplyYaml.preflight(graph, "install") ++ List(
      s"      - uses: ${ActionPins.downloadArtifact.line}",
      "        with:",
      "          path: dist-in",
      "      - run: |",
      "          set -eu",
      s"          version=$version",
      s"          group=$group",
      s"          artifact=$artifact",
      "          kit=dist-in/client-kit/client-kit.zip",
      "          sha() { sha256sum \"$1\" | awk '{print $1}'; }",
      "          kit_sha=$(sha \"$kit\")",
      "          cp \"$kit\" \"$version-client-kit.zip\"",
      "          shim_file=dist-in/shim/histos",
      "          shim_sha=$(sha \"$shim_file\")",
      "          cp \"$shim_file\" shim",
      "          cp \"$shim_file\" \"shim-$shim_sha\"",
      "          printf '%s\\n' \"$shim_sha\" > shim.sha256",
      "          cat > \"$version-manifest.json\" <<EOF",
      "          {",
      "            \"version\": \"$version\",",
      "            \"group\": \"$group\",",
      "            \"artifact\": \"$artifact\",",
      "            \"revision\": \"$version\",",
      "            \"clientKit\": \"$version-client-kit.zip\",",
      "            \"clientKitSha256\": \"$kit_sha\"",
      "          }",
      "          EOF",
      "          gh release upload dist \"$version-manifest.json\" \"$version-client-kit.zip\" \"shim-$shim_sha\" shim shim.sha256 --clobber --repo \"$GITHUB_REPOSITORY\"",
      "        env:",
      "          GH_TOKEN: ${{ github.token }}",
    )

  /** Promote does not sign, so this file does not list `secretNames`. */
  private val promoteSecrets: List[String] =
    List("SONATYPE_USERNAME", "SONATYPE_PASSWORD")

  private def promoteDocument(graph: ProjectGraph)(using RenderingShip): String =
    val lines = List(
      "name: promote",
      "on:",
      "  workflow_dispatch:",
      "    inputs:",
      "      deployment:",
      "        description: deployment id",
      "        required: true",
      "        type: string",
      "permissions:",
      "  contents: read",
      "jobs:",
      "  promote:",
      s"    runs-on: $Runner",
    ) ++ jobLimit(graph) ++ List(
      "    steps:",
      s"      - uses: $Checkout",
    ) ++ coursierCli(graph, "promote") ++ List(
      "      - run: \"./histos publish promote ${{ inputs.deployment }}\""
    ) ++ secretEnv(promoteSecrets)
    lines.mkString("\n") + "\n"

  private def secretEnv(names: List[String]): List[String] =
    names match
      case Nil => Nil
      case _   => "        env:" :: names.map(secret)

  private def secret(name: String): String =
    s"          $name: $${{ secrets.$name }}"

  /** A matrix step runs for every cell, or only for the cells that name the tool. */
  private enum MatrixScope:
    case Whole
    case Cell

  /** `npm ci` at the repository root. Not a tool install. */
  private enum NpmInstall:
    case Off
    case On(scope: MatrixScope)

  /** Matrix rows are a name and a command, or those plus the tool flags. */
  private enum ToolColumns:
    case Off
    case On

  /** A release job publishes, or it also uploads the install artifacts. */
  private enum ReleaseShape:
    case Publish
    case WithInstall

  /** On a push, `github.ref` is `refs/heads/<branch>` and `repository.default_branch` is the short name. */
  private val OnDefaultBranch: String =
    "github.ref == format('refs/heads/{0}', github.event.repository.default_branch)"

  private val OnDefaultPush: String =
    s"github.event_name == 'push' && $OnDefaultBranch"

  /** Required verification runs on pull requests, main pushes, and manual dispatch. */
  private val OnVerify: String =
    "github.event_name == 'pull_request' || github.event_name == 'workflow_dispatch' || github.event_name == 'push'"

  private val NoneFailed: String =
    "!cancelled() && !contains(needs.*.result, 'failure')"

  private def verifyIf: List[String] =
    List(s"    if: ${quote(OnVerify)}")

  private def verifyNeeds: List[String] =
    List(s"    if: ${quote(s"($OnVerify) && ($NoneFailed)")}")

  /** `on.push.branches` is a static glob. GitHub evaluates it before the `github` context exists, so `branch` is the
    * short name resolved when this file is written. `pull_request` still validates every other branch. A push to a
    * feature branch does not start this file, so one commit on an open pull request is one run. Verify jobs repeat
    * [[OnVerify]] on PR, main push, and dispatch. `$default-branch` is a workflow-template token and is not written
    * here. `workflow_dispatch` is on this file because verify accepts it. A label, a path, a cron, or `workflow_run` is
    * not: those are their own files.
    */
  private def header(extra: CiHeader, graph: ProjectGraph, branch: String): List[String] =
    val tags =
      if extra.tags.isEmpty then Nil
      else "    tags:" :: extra.tags.map(tag => s"      - ${quote(tag)}")
    val merge   = if extra.extras.contains(CiExtra.MergeGroup) then List("  merge_group:") else Nil
    val release = if extra.extras.contains(CiExtra.Release) then List("  release:") else Nil
    List(
      "name: ci",
      "on:",
      "  pull_request:",
      "    types: [opened, synchronize, reopened, labeled]",
      "  push:",
      "    branches:",
      s"      - ${quote(branch)}",
    ) ++ tags ++ dispatchInputs(extra.inputs) ++ merge ++ release ++ List(
      "permissions:",
      "  contents: read",
    ) ++ workflowEnv(graph) ++ List(
      "jobs:"
    )

  private def workflowEnv(graph: ProjectGraph): List[String] =
    val plain = graph.ci.env.filter {
      case (_, histos.plugin.EnvValue.Secret(_)) => false
      case _                                     => true
    }
    plain match
      case Nil => Nil
      case _   =>
        "env:" :: plain.map:
          case (name, value) => s"  ${name.value}: ${workflowValue(value)}"

  private def workflowValue(value: histos.plugin.EnvValue): String =
    value match
      case histos.plugin.EnvValue.Plain(text)   => quote(text)
      case histos.plugin.EnvValue.Secret(name)  => s"$${{ secrets.${name.value} }}"
      case histos.plugin.EnvValue.Github(field) =>
        field match
          case histos.plugin.GithubField.Sha     => "${{ github.sha }}"
          case histos.plugin.GithubField.RefName => "${{ github.ref_name }}"
          case histos.plugin.GithubField.Token   => "${{ github.token }}"
      case histos.plugin.EnvValue.Env(name) => s"$${{ env.${name.value} }}"

  private def dispatchInputs(inputs: List[histos.plugin.DispatchInput]): List[String] =
    inputs match
      case Nil => List("  workflow_dispatch:")
      case _   =>
        "  workflow_dispatch:" :: "    inputs:" :: inputs.flatMap: input =>
          val kind = input.kind match
            case histos.plugin.InputKind.Text    => "string"
            case histos.plugin.InputKind.Boolean => "boolean"
            case histos.plugin.InputKind.Choice  => "choice"
          List(
            s"      ${input.name.value}:",
            s"        description: ${input.name.value}",
            "        required: false",
            s"        type: $kind",
          ) ++ input.default.toList.map(value => s"        default: ${quote(value)}")

  /** The test job, and the scaladoc check when a build asks for it. The scaladoc job restores the store because it
    * compiles, and it does not save: the suite is the writer. It does not wait for the suite. Versions, plugin docs,
    * deploy, workflow, and release do not restore these. This workflow does not fetch a JDK, so it emits no JDK key.
    * Node is installed for a JavaScript module, or when Playwright is installed. `setup-node` does not set
    * `cache: npm`: that key is a package lock, and Chekhov does not read it. The test job runs `npm ci` when it runs a
    * JavaScript module and the repo root has `package-lock.json`. LLVM is installed for a native module. The runner is
    * `ubuntu-26.04`, so a miss downloads `LLVM-<revision>-Linux-X64.tar.xz` from the llvm-project release and unpacks
    * it. A hit does not. The step runs either way: it checks `clang` and puts `bin` on `PATH`. Chekhov reads Playwright
    * from `~/.cache/chekhov`. An unset `PLAYWRIGHT_BROWSERS_PATH` is `~/.cache/ms-playwright` on this runner. A
    * Playwright revision installs the browser even when no module is Firefox, because a JVM suite can launch it.
    * `npm install` and `install firefox` run when that cache misses. The step still runs on a hit, checks the package,
    * and runs `install-deps firefox`: it is apt, and a runner does not keep those packages. The runner's Azure Ubuntu
    * mirror hangs that apt with no output, so the step rewrites `azure.archive.ubuntu.com` to `archive.ubuntu.com`
    * first and gives the command three minutes. A step `if` that skips the download leaves a job that did not run.
    * Coursier is the Linux default of `Resolve.coursier` when `COURSIER_CACHE` is unset. `hashFiles` names the build
    * file while the build resolves, and the pin while it is locked. No step sets `restore-keys`. Every job that runs
    * `./histos` installs Histos the way the build chose and does not install a JDK or the `cs` binary.
    */
  private def steps(
      tools: RunnerTools,
      resolution: Resolution,
      need: ToolNeed,
      scope: MatrixScope,
      install: NpmInstall,
  ): List[String] =
    nodeStep(if need.node then tools.node else None, scope) ++
      npmCi(install) ++
      llvmStep(if need.llvm then tools.llvm else None, scope) ++
      opensslStep(need.openssl, scope) ++
      playwrightStep(if need.playwright then tools.playwright else None, tools.browsers, scope) ++
      coursierStep(resolution)

  private def npmCi(install: NpmInstall): List[String] =
    install match
      case NpmInstall.Off       => Nil
      case NpmInstall.On(scope) =>
        val guard = cellFlag(scope, "npm").toList.map(text => s"        if: $text")
        List(s"      - run: ${quote("npm ci")}") ++ guard

  private def cellFlag(scope: MatrixScope, flag: String): Option[String] =
    scope match
      case MatrixScope.Whole => None
      case MatrixScope.Cell  => Some(s"matrix.$flag == 'true'")

  /** Current major. v4 still targets Node 20. */
  private val ToolCache = ActionPins.cache.line

  private val CoursierCache = "~/.cache/coursier/v1"

  private val PlaywrightBrowsers = "~/.cache/ms-playwright"

  private def coursierKey(resolution: Resolution): String =
    val named = resolution match
      case Resolution.Solve => "histos.scala"
      case Resolution.Lock  => "dependencies.lock"
    s"coursier-$${{ hashFiles('$named') }}-$${{ runner.os }}-$${{ runner.arch }}"

  private def toolKey(name: String, revision: String): String =
    s"$name-$revision-$${{ runner.os }}-$${{ runner.arch }}"

  private def cacheStep(
      id: Option[String],
      paths: List[String],
      key: String,
      condition: Option[String] = None,
  ): List[String] =
    paths match
      case Nil => Nil
      case _   =>
        val ident = id match
          case None       => Nil
          case Some(name) => List(s"        id: $name")
        val guard = condition.toList.map(text => s"        if: $text")
        List(s"      - uses: $ToolCache") ++ ident ++ guard ++ List("        with:") ++ cachePaths(paths) ++ List(
          s"          key: ${quote(key)}"
        )

  private def cachePaths(paths: List[String]): List[String] =
    paths match
      case path :: Nil => List(s"          path: ${quote(path)}")
      case many        => "          path: |" :: many.map(path => s"            $path")

  private def coursierStep(resolution: Resolution): List[String] =
    cacheStep(None, List(CoursierCache), coursierKey(resolution))

  /** Histos the way the build chose, then the job's preflight. This does not install a JDK. */
  private def coursierCli(graph: ProjectGraph, job: String)(using RenderingShip): List[String] =
    CiInstall.steps(graph) ++ SupplyYaml.preflight(graph, job)

  private def nodeStep(floor: Option[NodeFloor], scope: MatrixScope): List[String] =
    floor match
      case None        => Nil
      case Some(major) =>
        val guard = cellFlag(scope, "node").toList.map(text => s"        if: $text")
        List(s"      - uses: ${ActionPins.setupNode.line}") ++ guard ++ List(
          "        with:",
          s"          node-version: ${quote(major.value.toString)}",
        )

  private def llvmStep(revision: Option[ToolRevision], scope: MatrixScope): List[String] =
    revision match
      case None         => Nil
      case Some(pinned) =>
        val directory = "${{ runner.temp }}/llvm"
        val version   = pinned.value
        val archive   = s"LLVM-$version-Linux-X64.tar.xz"
        val url       = s"https://github.com/llvm/llvm-project/releases/download/llvmorg-$version/$archive"
        val cell      = cellFlag(scope, "llvm")
        val hex       = LlvmDigest.of(archive).getOrElse("missing")
        cacheStep(Some("llvm"), List(directory), toolKey("llvm", version), cell) ++ List(
          "      - run: |",
          s"          dir=${quote(directory)}",
          "          if [ \"${{ steps.llvm.outputs.cache-hit }}\" != \"true\" ]; then",
          "            mkdir -p \"$dir\"",
          s"            curl -fsSL ${quote(url)} -o \"$$dir/llvm.tar.xz\"",
          s"            expected=${quote(hex)}",
          "            got=$(openssl dgst -sha256 \"$dir/llvm.tar.xz\" | awk '{print $NF}')",
          "            [ \"$got\" = \"$expected\" ] || { echo \"llvm checksum mismatch\" >&2; exit 1; }",
          "            tar -xJf \"$dir/llvm.tar.xz\" -C \"$dir\" --strip-components=1",
          "            rm \"$dir/llvm.tar.xz\"",
          "          fi",
          "          test -x \"$dir/bin/clang\"",
          "          echo \"$dir/bin\" >> \"$GITHUB_PATH\"",
          "          if [ -n \"$LD_LIBRARY_PATH\" ]; then",
          s"""            echo "LD_LIBRARY_PATH=$$dir/lib:$$LD_LIBRARY_PATH" >> "$$GITHUB_ENV"""",
          "          else",
          "            echo \"LD_LIBRARY_PATH=$dir/lib\" >> \"$GITHUB_ENV\"",
          "          fi",
        ) ++ cell.toList.map(text => s"        if: $text")

  /** Native modules that link OpenSSL need the headers and the shared libraries on the runner. */
  private def opensslStep(needed: Boolean, scope: MatrixScope): List[String] =
    if !needed then Nil
    else
      val names = NativeCi.packages.mkString(" ")
      val cell  = cellFlag(scope, "openssl")
      List(
        "      - name: Install native link libraries",
        "        run: |",
        "          sudo apt-get update",
        s"          sudo apt-get install -y $names",
      ) ++ cell.toList.map(text => s"        if: $text")

  private def playwrightStep(
      revision: Option[ToolRevision],
      browsers: Set[histos.Browser],
      scope: MatrixScope,
  ): List[String] =
    revision match
      case None         => Nil
      case Some(pinned) =>
        val version = pinned.value
        val named   = browsers.toList.map(_.token).sorted.mkString(" ")
        val cell    = cellFlag(scope, "playwright")
        cacheStep(
          Some("playwright"),
          List(s"~/.cache/chekhov/playwright/$version", PlaywrightBrowsers),
          toolKey("playwright", version),
          cell,
        ) ++ List(
          "      - run: |",
          s"          dir=\"$$HOME/.cache/chekhov/playwright/$version\"",
          "          if [ \"${{ steps.playwright.outputs.cache-hit }}\" != \"true\" ]; then",
          "            mkdir -p \"$dir\"",
          s"            npm install --prefix \"$$dir\" --no-fund --no-audit --no-progress playwright@$version",
          s"            node \"$$dir/node_modules/playwright/cli.js\" install $named",
          "          fi",
          "          test -d \"$dir/node_modules/playwright\"",
          "          find /etc/apt -type f -print0 | xargs -0 -r sudo sed -i 's|azure.archive.ubuntu.com|archive.ubuntu.com|g'",
          s"          timeout 180s node \"$$dir/node_modules/playwright/cli.js\" install-deps $named",
        ) ++ cell.toList.map(text => s"        if: $text")

  /** PR base, else the last successful push on this branch, else `all`. A failed ancestor check keeps `all`. */
  private val diffBase: String =
    """base=all
      |if [ "$GITHUB_EVENT_NAME" = "pull_request" ]; then
      |  base="$PR_BASE"
      |elif [ "$GITHUB_EVENT_NAME" = "push" ]; then
      |  last=$(gh api "/repos/${GITHUB_REPOSITORY}/actions/workflows/ci.yml/runs?event=push&status=success&branch=${GITHUB_REF_NAME}&per_page=1" --jq '.workflow_runs[0].head_sha // empty' || true)
      |  if [ -n "$last" ] && git merge-base --is-ancestor "$last" HEAD; then
      |    base="$last"
      |  fi
      |fi""".stripMargin

  private def affectedJob(graph: ProjectGraph)(using RenderingShip): String =
    val script = runScript(
      diffBase + "\n" +
        """modules=$(./histos affected --base "$base" || echo '["all"]')
          |echo "modules=$modules" >> "$GITHUB_OUTPUT"""".stripMargin
    )
    (List(
      "  affected:"
    ) ++ List(
      s"    runs-on: $Runner"
    ) ++ jobLimit(graph) ++ List(
      "    permissions:",
      "      actions: read",
      "      contents: read",
      "    outputs:",
      "      modules: ${{ steps.affected.outputs.modules }}",
      "    steps:",
    ) ++ fullGitCheckout() ++ CiInstall.prepare(graph) ++ SupplyYaml.preflight(graph, "affected") ++ List(
      "      - id: affected",
      "        env:",
      "          GH_TOKEN: ${{ github.token }}",
      "          PR_BASE: ${{ github.event.pull_request.base.sha }}",
      "        run: |",
      script,
    )).mkString("\n")

  /** A push to the default branch fills `main` and saves the store. Rebuild compiles. CopyIndex copies the pull-request
    * index alongside required main verification.
    */
  private def cacheSeed(jobs: List[TestJob], graph: ProjectGraph)(using RenderingShip): String =
    val needs   = jobs.map(job => toolNeed(graph, stagesOf(job), graph.runnerTools))
    val any     = needs.foldLeft(ToolNeed(Set.empty))(_ || _)
    val prepare = steps(graph.runnerTools, graph.resolution, any, MatrixScope.Whole, NpmInstall.Off)
    (List(
      "  cache-seed:"
    ) ++ afterCompile(graph) ++ List(
      s"    if: ${quote(s"($OnDefaultPush) && ($NoneFailed)")}",
      s"    runs-on: $Runner",
    ) ++ jobLimit(graph) ++ List(
      "    permissions:",
      "      actions: write",
      "      contents: read",
      "    steps:",
      s"      - uses: $Checkout",
    ) ++ coursierCli(graph, "cache-seed") ++ prepare ++ storeRestore ++ cacheWrite(
      "main",
      seedCommand(graph.promote),
    ) ++ storeSave).mkString("\n")

  private def testJob(jobs: List[TestJob], graph: ProjectGraph, lock: NpmLock)(using RenderingShip): List[String] =
    val needs     = jobs.map(job => toolNeed(graph, stagesOf(job), graph.runnerTools))
    val any       = needs.foldLeft(ToolNeed(Set.empty))(_ || _)
    val matrix    = jobs.size > 1
    val scope     = if matrix then MatrixScope.Cell else MatrixScope.Whole
    val install   = npmInstall(jobs, graph, lock, scope)
    val prepare   = steps(graph.runnerTools, graph.resolution, any, scope, install)
    val columns   = if matrix && any.any then ToolColumns.On else ToolColumns.Off
    val installed = coursierCli(graph, "test")
    jobs match
      case aggregate :: Nil =>
        List(
          "  test:"
        ) ++ afterCompile(graph) ++ verifyNeeds ++ List(
          s"    runs-on: $Runner"
        ) ++ jobLimit(graph) ++ List(
          "    permissions:",
          "      contents: read",
          "    steps:",
          s"      - uses: $Checkout",
        ) ++ installed ++ prepare ++ storeRestore ++ testRun(
          shellArgv(argv(aggregate))
        )
      case _ =>
        List(
          "  test:"
        ) ++ afterCompile(graph) ++ verifyNeeds ++ List(
          s"    runs-on: $Runner"
        ) ++ jobLimit(graph) ++ List(
          "    permissions:",
          "      contents: read",
          "    strategy:",
          "      fail-fast: false",
          "      matrix:",
          "        include:",
        ) ++ jobs.flatMap(job => include(job, graph, columns, install)) ++ List(
          "    steps:",
          s"      - uses: $Checkout",
        ) ++ installed ++ prepare ++ storeRestore ++ testRun("${{ matrix.command }}")

  private def include(
      job: TestJob,
      graph: ProjectGraph,
      columns: ToolColumns,
      install: NpmInstall,
  ): List[String] =
    val rows = List(
      s"          - name: ${quote(slug(job))}",
      s"            command: ${quote(shellArgv(argv(job)))}",
    )
    val flags = columns match
      case ToolColumns.Off => Nil
      case ToolColumns.On  =>
        val need = toolNeed(graph, stagesOf(job), graph.runnerTools)
        List(
          s"            node: ${quote(need.node.toString)}",
          s"            llvm: ${quote(need.llvm.toString)}",
          s"            openssl: ${quote(need.openssl.toString)}",
          s"            playwright: ${quote(need.playwright.toString)}",
        )
    rows ++ flags ++ npmColumn(job, install)

  /** True only for a cell whose modules contain JavaScript. The column exists only when the job installs from the
    * lockfile, so the node floor and the lock are already decided.
    */
  private def npmColumn(job: TestJob, install: NpmInstall): List[String] =
    install match
      case NpmInstall.On(MatrixScope.Cell) =>
        val runsJs = stagesOf(job).exists(_.cell.platform == Platform.Js)
        List(s"            npm: ${quote(runsJs.toString)}")
      case NpmInstall.On(MatrixScope.Whole) => Nil
      case NpmInstall.Off                   => Nil

  private def npmInstall(
      jobs: List[TestJob],
      graph: ProjectGraph,
      lock: NpmLock,
      scope: MatrixScope,
  ): NpmInstall =
    val runsJs = jobs.exists(job => stagesOf(job).exists(_.cell.platform == Platform.Js))
    if lock == NpmLock.Present && graph.runnerTools.node.isDefined && runsJs then NpmInstall.On(scope)
    else NpmInstall.Off

  enum Tool:
    case Node, Llvm, Playwright, OpenSsl

  final case class ToolNeed(required: Set[Tool]):
    def ||(other: ToolNeed): ToolNeed = ToolNeed(required ++ other.required)
    def node: Boolean                 = required.contains(Tool.Node)
    def llvm: Boolean                 = required.contains(Tool.Llvm)
    def playwright: Boolean           = required.contains(Tool.Playwright)
    def openssl: Boolean              = required.contains(Tool.OpenSsl)
    def any: Boolean                  = required.nonEmpty

  /** `stages` is the planned stage closure, including producer inputs when supplied by BuildDag. */
  def toolNeed(graph: ProjectGraph, stages: List[histos.CompilationStage], tools: RunnerTools): ToolNeed =
    val cells         = stages.map(_.cell).distinct
    val native        = cells.exists(_.platform == Platform.Native)
    val javascript    = cells.exists(_.platform == Platform.Js)
    val browser       = tools.playwright.isDefined && stages.exists(_.scope == histos.plugin.SourceSet.Test)
    val nativeModules = cells
      .filter(_.platform == Platform.Native)
      .flatMap(cell =>
        graph
          .get(cell.module)
          .collect:
            case module: Module => module
      )
    ToolNeed(
      List(
        Option.when(tools.node.isDefined && (javascript || browser))(Tool.Node),
        Option.when(tools.llvm.isDefined && native)(Tool.Llvm),
        Option.when(browser)(Tool.Playwright),
        Option.when(NativeCi.openssl(nativeModules))(Tool.OpenSsl),
      ).flatten.toSet
    )

  def toolSteps(graph: ProjectGraph, stages: List[histos.CompilationStage]): List[String] =
    steps(
      graph.runnerTools,
      graph.resolution,
      toolNeed(graph, stages, graph.runnerTools),
      MatrixScope.Whole,
      NpmInstall.Off,
    )

  private def stagesOf(job: TestJob): List[histos.CompilationStage] = job.cells.compileStageRoots

  private def slug(job: TestJob): String =
    val names = job.selection.targets match
      case histos.TestTargets.All                => Nil
      case histos.TestTargets.Named(first, rest) => (first :: rest).map(_.value).distinct
      case histos.TestTargets.Filtered(modules)  => modules.map(_.value).distinct
    val platforms = job.selection.platforms match
      case histos.PlatformSelection.All          => Nil
      case histos.PlatformSelection.Only(chosen) => chosen.all.map(_.token).distinct
    (names ++ platforms).mkString("-") match
      case ""   => "all"
      case name => name

  /** Restore, then the work, then save. The work writes new products into the restored store. A taken key is deleted
    * and saved again, because GitHub will not replace one. The delete step always runs. It calls the API only when the
    * restore hit, because a miss has no key to replace. A matrix is more than one writer, so it does not open this
    * scope. The compile job restores and saves. Test, scaladoc, coverage, compatibility, and a site job restore and do
    * not save. Format, versions, deploy, workflow, and release do not either.
    */
  private val StorePath = "~/.cache/histos"

  private val StoreRestore = s"actions/cache/restore@${ActionPins.cache.pin.sha.value} # ${ActionPins.cache.pin.tag}"

  private val StoreSave = s"actions/cache/save@${ActionPins.cache.pin.sha.value} # ${ActionPins.cache.pin.tag}"

  private def storeKey: String =
    val namespace =
      "${{ case(github.event_name == 'pull_request', format('pr-{0}', github.event.pull_request.number), 'main') }}"
    s"histos-store-$namespace-$${{ runner.os }}-$${{ runner.arch }}"

  private[workflow] def storeRestore: List[String] =
    List(
      s"      - uses: $StoreRestore",
      "        id: store",
      "        with:",
      s"          path: ${quote(StorePath)}",
      s"          key: ${quote(storeKey)}",
    )

  private def storeSave: List[String] =
    List(
      "      - if: \"always()\"",
      "        env:",
      "          GH_TOKEN: ${{ github.token }}",
      s"          STORE_KEY: ${quote(storeKey)}",
      "        run: |",
      "          if [ \"${{ steps.store.outputs.cache-hit }}\" = \"true\" ]; then",
      "            gh api --method DELETE /repos/${GITHUB_REPOSITORY}/actions/caches?key=${STORE_KEY}",
      "          fi",
      "      - if: \"always()\"",
      s"        uses: $StoreSave",
      "        with:",
      s"          path: ${quote(StorePath)}",
      s"          key: ${quote(storeKey)}",
    )

  /** The test step only. A pull request is `pr/<n>`. Any other run of this file is a push to the default branch, and
    * that event is the `main` cache namespace: `CacheNamespace.Main`, not the git branch name. Docs, deploy, versions,
    * workflow, and release do not set it. None of them `needs` a cache job or restores a remote disk. This render does
    * not emit that job: the remote URI is configuration.
    */
  private val cacheNamespace: String =
    "${{ case(github.event_name == 'pull_request', format('pr/{0}', github.event.pull_request.number), 'main') }}"

  /** `Rebuild` compiles into `main`. `CopyIndex` copies the pull-request index and does not compile. */
  private def seedCommand(promote: Promote): String =
    promote match
      case Promote.Rebuild   => "./histos compile"
      case Promote.CopyIndex =>
        "./histos cache promote --from pr/${{ github.event.pull_request.number }} --to main"

  /** A writing step. The namespace is the caller's. The mode is write, and a laptop that copies this file still opens
    * as read.
    */
  /** The same base script as `affected`, then `publish check`. */
  private def proofStep(command: String): List[String] =
    script(diffBase + "\n" + command) ++ List(
      "        env:",
      "          GH_TOKEN: ${{ github.token }}",
      "          PR_BASE: ${{ github.event.pull_request.base.sha }}",
    )

  private def cacheWrite(namespace: String, command: String): List[String] =
    List(
      s"      - run: ${quote(command)}",
      "        env:",
      s"          ${CacheNamespace.Key}: ${quote(namespace)}",
      s"          ${CacheConfig.ModeKey}: ${quote("write")}",
    )

  private def testRun(command: String): List[String] =
    val narrowed = s"$command --affected '$${{ needs.affected.outputs.modules }}'"
    cacheWrite(cacheNamespace, narrowed)

  /** Pinned image. `ubuntu-latest` moves, and this render should not move with it. */
  private val Runner = "ubuntu-26.04"

  /** `timeout-minutes` for one job. Unbounded emits nothing. An out-of-range count emits nothing and [[limits]] fails
    * the render.
    */
  private[workflow] def jobLimit(graph: ProjectGraph): List[String] =
    graph.ci.timeout match
      case JobTimeout.Unbounded                                  => Nil
      case JobTimeout.Minutes(count) if JobTimeout.admits(count) => List(s"    timeout-minutes: $count")
      case JobTimeout.Minutes(_)                                 => Nil

  /** The whole history and the tags, the way zipx checks out. `fetch-depth: 0` is all branches and tags. A fetch still
    * does not write `refs/remotes/origin/HEAD`, so a job that renders a workflow follows this with
    * [[CiInstall.originHead]].
    */
  private[workflow] def fullGitCheckout(persistCredentials: Boolean = true): List[String] =
    val creds =
      if persistCredentials then Nil else List("          persist-credentials: false")
    List(
      s"      - uses: ${ActionPins.checkout.line}",
      "        with:",
      s"          fetch-depth: ${quote("0")}",
      s"          fetch-tags: ${quote("true")}",
    ) ++ creds

  /** Current major. v4 still targets Node 20, which the runner rewrites to Node 24. */
  private val Checkout = ActionPins.checkout.line

  private[workflow] def runner: String = Runner

  private[workflow] def onDefaultPush: String = OnDefaultPush

  /** Beside the suite. It does not wait for the suite, so a scaladoc warning does not hold the tests. */
  private def docJob(graph: ProjectGraph)(using RenderingShip): String =
    val modules   = graph.projects.collect { case module: Module => module }
    val published = graph.catalog.ships.flatMap(_.members.toList)
    val chosen    = modules.filter(module => published.contains(module.name))
    val prepare   = steps(
      graph.runnerTools,
      graph.resolution,
      toolNeed(
        graph,
        chosen.flatMap(module =>
          module.versions.all.flatMap(version =>
            module.platforms.all.map(platform =>
              histos.CompilationStage(histos.Cell(module.name, version, platform), histos.plugin.SourceSet.Main)
            )
          )
        ),
        graph.runnerTools,
      ),
      MatrixScope.Whole,
      NpmInstall.Off,
    )
    val lines = List(
      "  scaladoc:"
    ) ++ afterCompile(graph) ++ verifyNeeds ++ List(
      s"    runs-on: $Runner"
    ) ++ jobLimit(graph) ++ List(
      "    permissions:",
      "      contents: read",
      "    steps:",
      s"      - uses: $Checkout",
    ) ++ coursierCli(graph, "scaladoc") ++ prepare ++ storeRestore ++ List(
      s"      - run: ${quote("./histos run doc")}"
    )
    lines.mkString("\n")

  /** Beside the suite. It does not wait for the suite. A floor on the ask is a flag. Excludes recorded on the build are
    * flags too. No floor does not fail on a rate.
    */
  private def coverageJob(graph: ProjectGraph, floors: CoverageFloors)(using RenderingShip): String =
    val command = ("./histos coverage" :: (floors.argv ++ graph.coverageExcludes.argv)).mkString(" ")
    val prepare =
      steps(
        graph.runnerTools,
        graph.resolution,
        ToolNeed(Set.empty),
        MatrixScope.Whole,
        NpmInstall.Off,
      )
    val lines = List(
      "  coverage:"
    ) ++ afterCompile(graph) ++ verifyNeeds ++ List(
      s"    runs-on: $Runner"
    ) ++ jobLimit(graph) ++ List(
      "    permissions:"
    ) ++ List(
      "      contents: read",
      "    steps:",
      s"      - uses: $Checkout",
    ) ++ coursierCli(graph, "coverage") ++ prepare ++ storeRestore ++ List(
      s"      - run: ${quote(command)}"
    )
    lines.mkString("\n")

  /** No toolchain and no `needs`. Compile's condition refuses a failed format, so the compile does not start. */
  private def formatJob(graph: ProjectGraph)(using RenderingShip): String =
    (List(
      "  format:"
    ) ++ verifyIf ++ List(
      s"    runs-on: $Runner"
    ) ++ jobLimit(graph) ++ List(
      "    permissions:",
      "      contents: read",
      "    steps:",
      s"      - uses: $Checkout",
    ) ++ script(scalaCliCheck(graph))).mkString("\n")

  private val ScalaCliRelease = "1.14.0"

  private val ScalaCliX64 =
    "7a55547894ecbc9cbf8682c5b2eb3680df0a305dc490e80a491ab873601baaa7"

  private val ScalaCliArm =
    "cccd7f0226118c5ee84435d5760c58c12c08972f15f70c528592cb638d83c5ee"

  /** `scala-cli fmt` reads `.scalafmt.conf`, including a configuration that is not at the repository root. */
  private def scalaCliCheck(graph: ProjectGraph): String =
    val conf = graph.formatting match
      case Formatting.Scalafmt(histos.ScalafmtConf.At(path)) => s" --scalafmt-conf ${path.value}"
      case Formatting.Scalafmt(histos.ScalafmtConf.Root)     => ""
      case Formatting.Off                                    => ""
    s"""set -euo pipefail
       |case "$${{ runner.arch }}" in
       |  X64) asset=scala-cli-x86_64-pc-linux; expected=$ScalaCliX64 ;;
       |  ARM64) asset=scala-cli-aarch64-pc-linux; expected=$ScalaCliArm ;;
       |  *) echo "scala-cli has no binary for $${{ runner.arch }}" >&2; exit 1 ;;
       |esac
       |dir="$${{ runner.temp }}/scala-cli"
       |mkdir -p "$$dir"
       |url="https://github.com/VirtusLab/scala-cli/releases/download/v$ScalaCliRelease/$$asset.gz"
       |curl -fsSL "$$url" -o "$$dir/scala-cli.gz"
       |got=$$(openssl dgst -sha256 "$$dir/scala-cli.gz" | awk '{print $$NF}')
       |[ "$$got" = "$$expected" ] || { echo "scala-cli checksum mismatch" >&2; exit 1; }
       |gzip -dc "$$dir/scala-cli.gz" > "$$dir/scala-cli"
       |chmod +x "$$dir/scala-cli"
       |"$$dir/scala-cli" fmt --check$conf .""".stripMargin

  /** Every module, main and test. The saved store is what test, docs, scaladoc, coverage, and compatibility restore. */
  private def compileJob(graph: ProjectGraph)(using RenderingShip): String =
    val modules = graph.projects.collect { case module: Module => module }
    val stages  = modules.flatMap: module =>
      module.versions.all.flatMap: version =>
        module.platforms.all.map: platform =>
          histos.CompilationStage(histos.Cell(module.name, version, platform), histos.plugin.SourceSet.Main)
    val prepare = steps(
      graph.runnerTools,
      graph.resolution,
      toolNeed(graph, stages, graph.runnerTools),
      MatrixScope.Whole,
      NpmInstall.Off,
    )
    val format = graph.formatting match
      case Formatting.Off         => Nil
      case Formatting.Scalafmt(_) => List("format")
    (List(
      s"  $compileJobName:"
    ) ++ afterAffected(format) ++ verifyNeeds ++ List(
      s"    runs-on: $Runner"
    ) ++ jobLimit(graph) ++ List(
      "    permissions:",
      "      actions: write",
      "      contents: read",
      "    steps:",
      s"      - uses: $Checkout",
    ) ++ coursierCli(graph, compileJobName) ++ prepare ++ storeRestore ++ cacheWrite(
      cacheNamespace,
      "./histos compile --tests",
    ) ++ storeSave).mkString("\n")

  /** The required check. It runs on a pull request and runs the full doctor. */
  private def workflowJob(graph: ProjectGraph)(using RenderingShip): String =
    (List(
      "  workflow:"
    ) ++ verifyIf ++ afterAffected(Nil) ++ List(
      s"    runs-on: $Runner"
    ) ++ jobLimit(graph) ++ List(
      "    permissions:"
    ) ++ List(
      "      contents: read",
      "    steps:",
    ) ++ fullGitCheckout() ++ CiInstall.originHead ++ CiInstall.steps(graph) ++ SupplyYaml.preflight(
      graph,
      "workflow",
    ) ++ SupplyYaml.fullDoctor(
      graph.supply.updates.author
    ) ++ List(
      "      - run: \"./histos workflow --check\""
    )).mkString("\n")

  def requiredJobs(graph: ProjectGraph): List[String] =
    val suite = if graph.projects.exists:
        case module: Module => module.lane == histos.TestLane.Suite
        case _: Aggregate   => false
    then List("test")
    else Nil
    val catalog =
      if graph.catalog.ships.isEmpty then Nil
      else List("versions", "compatibility")
    val doc    = if graph.docCheck == DocCheck.On then List("scaladoc") else Nil
    val format = graph.formatting match
      case Formatting.Off         => Nil
      case Formatting.Scalafmt(_) => List("format")
    val coverage = graph.coverage match
      case CoverageJob.Off    => Nil
      case CoverageJob.Ask(_) => List("coverage")
    val compiled = if compiles(graph) then List(compileJobName) else Nil
    List(
      "affected"
    ) ++ compiled ++ List(
      "workflow",
      graph.supply.checkName.value,
    ) ++ suite ++ catalog ++ doc ++ format ++ coverage ++ graph.verify.map(_.name.value)

  private def compatibilityJob(graph: ProjectGraph)(using RenderingShip): String =
    (List("  compatibility:") ++ afterCompile(graph) ++ verifyNeeds ++ List(
      s"    runs-on: $Runner"
    ) ++ jobLimit(graph) ++ List(
      "    permissions:",
      "      contents: read",
      "    steps:",
      s"      - uses: $Checkout",
    ) ++ CiInstall.steps(graph) ++ coursierStep(graph.resolution) ++ storeRestore ++ proofStep(
      """./histos publish check --base "$base""""
    )).mkString("\n")

  private def versionsJob(graph: ProjectGraph)(using RenderingShip): String =
    (List(
      "  versions:"
    ) ++ verifyIf ++ afterAffected(Nil) ++ List(
      s"    runs-on: $Runner"
    ) ++ jobLimit(graph) ++ List(
      "    permissions:"
    ) ++ List(
      "      contents: read",
      "    steps:",
      s"      - uses: $Checkout",
    ) ++ CiInstall.steps(graph) ++ SupplyYaml.preflight(graph, "versions") ++ List(
      "      - run: \"./histos run versions\""
    )).mkString("\n")

  /** `affected` installs the toolchain. Every later job in this workflow waits for that install. */
  private def afterAffected(extra: List[String]): List[String] =
    val names = "affected" :: extra
    List(s"    needs: [${names.mkString(", ")}]")

  /** Compile has saved the store. Format is a need when this graph checks it, so a format failure is a failure these
    * jobs can see. A custom `if` would otherwise start them after a skipped compile.
    */
  private def afterCompile(graph: ProjectGraph): List[String] =
    afterAffected(compileGate(graph))

  /** Format, then compile. Empty when this graph has neither. */
  private[workflow] def compileGate(graph: ProjectGraph): List[String] =
    val format = graph.formatting match
      case Formatting.Off         => Nil
      case Formatting.Scalafmt(_) => List("format")
    val compiled = if compiles(graph) then List(compileJobName) else Nil
    format ++ compiled

  private[workflow] val compileJobName = "compile"

  private[workflow] def compiles(graph: ProjectGraph): Boolean =
    graph.projects.exists:
      case _: Module    => true
      case _: Aggregate => false

  private def verifyJob(graph: ProjectGraph, task: VerifyTask, cells: Option[histos.testrun.TestCellPlan])(using
      RenderingShip
  ): String =
    val lines = List(
      s"  ${task.name.value}:"
    ) ++ verifyIf ++ afterAffected(Nil) ++ List(
      s"    runs-on: $Runner"
    ) ++ jobLimit(graph) ++ List(
      "    permissions:",
      "      actions: read",
      "      contents: read",
      "    steps:",
      s"      - uses: $Checkout",
    ) ++ verifySteps(graph, task, cells)
    lines.mkString("\n")

  /** Native puts clang on `PATH` before `toolchain install --client`. Skip does not add LLVM: a verify task names no
    * other tool.
    */
  private def verifySteps(graph: ProjectGraph, task: VerifyTask, cells: Option[histos.testrun.TestCellPlan])(using
      RenderingShip
  ): List[String] =
    val need     = toolNeed(graph, cells.toList.flatMap(_.compileStageRoots), graph.runnerTools)
    val selected = steps(graph.runnerTools, graph.resolution, need, MatrixScope.Whole, NpmInstall.Off)
    val clang    = task.link match
      case ClientLink.Native if !need.llvm     => llvmStep(graph.runnerTools.llvm, MatrixScope.Whole)
      case ClientLink.Native | ClientLink.Skip => Nil
    val command = task.run match
      case histos.VerifyRun.Tests(selection) => shellArgv(selectionArgv(selection))
      case histos.VerifyRun.BuiltIn(name)    => s"./histos run ${name.value}"
      case histos.VerifyRun.Plugin(id)       => s"./histos run ${id.render}"
    val run = List(s"      - run: ${quote(command)}")
    CiInstall.steps(graph) ++ selected ++ clang ++ run

  private def script(body: String): List[String] =
    List("      - run: |") ++ body.linesIterator.map(line => "          " + line).toList

  private def runScript(body: String): String =
    body.linesIterator.map(line => "          " + line).mkString("\n")

  private def quote(text: String): String =
    "\"" + text.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  /** A regular file is present. A missing path or a directory of that name is absent. A failed stat is an io error. */
  private def npmLock(repo: Path): IO[CiError, NpmLock] =
    val path = repo.resolve("package-lock.json")
    ZIO
      .attemptBlocking(Files.isRegularFile(path))
      .mapError(_ => CiError.Io(IoFailure(path, IoFault.Stat)))
      .map(found => if found then NpmLock.Present else NpmLock.Absent)

  private def compare(repo: Path, rendered: RenderedWorkflow): IO[CiError, Unit] =
    read(file(repo, rendered.name)).foldZIO(
      {
        case CiError.Missing(_) if rendered.name != fileName =>
          ZIO.fail(CiError.Drift(workflowPath(rendered.name), "the file is missing"))
        case other => ZIO.fail(other)
      },
      committed => ZIO.fromEither(differsNamed(rendered.name, rendered.body, committed)),
    )

  /** `histos-*.yml` files this graph did not emit. Other workflow names stay. */
  private def orphans(repo: Path, emitted: Set[String]): IO[CiError, List[Path]] =
    val dir = repo.resolve(".github").resolve("workflows")
    ZIO
      .attemptBlocking {
        if Files.isDirectory(dir) then histosFiles(dir).filter(path => !emitted.contains(fileNameOf(path)))
        else Nil
      }
      .mapError(_ => CiError.Io(IoFailure(dir, IoFault.Walk)))

  private def histosFiles(dir: Path): List[Path] =
    val stream = Files.list(dir)
    val found  =
      try stream.iterator.asScala.toList
      finally stream.close()
    found.filter: path =>
      val name = fileNameOf(path)
      name.startsWith("histos-") && name.endsWith(".yml")

  private def fileNameOf(path: Path): String =
    Option(path.getFileName).map(_.toString).getOrElse("")

  private def read(file: Path): IO[CiError, String] =
    ZIO
      .attemptBlocking {
        if Files.isRegularFile(file) then Right(Files.readString(file)) else Left(CiError.Missing(file))
      }
      .mapError(_ => CiError.Io(IoFailure(file, IoFault.Read)))
      .flatMap(ZIO.fromEither)

  private def write(file: Path, text: String): IO[CiError, Unit] =
    Option(file.getParent) match
      case None         => ZIO.fail(CiError.Io(IoFailure(file, IoFault.Write)))
      case Some(parent) =>
        ZIO
          .attemptBlocking(replace(parent, file, text))
          .mapError(_ => CiError.Io(IoFailure(file, IoFault.Write)))

  private def replace(parent: Path, file: Path, text: String): Unit =
    Files.createDirectories(parent)
    val name = Option(file.getFileName).map(_.toString).getOrElse(fileName)
    val temp = parent.resolve(s"$name.tmp")
    Files.writeString(temp, text)
    try Files.move(temp, file, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
    catch
      case _: AtomicMoveNotSupportedException =>
        Files.move(temp, file, StandardCopyOption.REPLACE_EXISTING)
end Workflow
