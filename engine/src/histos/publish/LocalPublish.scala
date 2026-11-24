package histos.publish

import histos.jdk.{JdkRuntime, Sha256, Sha256Digest}
import histos.central.{
  Artifact,
  ArtifactId,
  DependencyVersion,
  MavenDependency,
  PomDocument,
  Published,
  Publisher,
  Revision,
}
import histos.native.NativeOrigin
import histos.cache.{ContentStore, ProductCache}
import histos.pack.{ClassJar, JarError, JavadocStub, PackageKey}
import histos.plugin.{ApiDigest, PluginCoordinate, RecipeError, RecipeFeature, RecipeNodeId}
import histos.resolve.{ArtifactName, Resolve}
import histos.version.{Decision, PublishAction, Ship, ShipName, VersionError, VersionTask}
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.{
  CompileError,
  GraphCompile,
  GraphCompileError,
  ProductDigest,
  GeneratedRoots,
  StageRoots,
  StageRootError,
}
import histos.plugin.SourceSet
import histos.{Cell, CompilationStage}
import histos.build.{BuildNode, ExecutionError, ExecutionProducts}
import histos.{
  Dep,
  DepExclusion,
  DepLink,
  MavenScope,
  Module,
  Organization,
  Platform,
  PomFact,
  ProjectGraph,
  ProjectName,
  Publication,
  ScalaVersion,
  ToolchainIdentity,
}

import java.nio.file.{Files, Path}

import histos.schedule.{CacheConfig, CacheError, Histos, Outcome, Parallelism, RemoteCache, RemoteError, StepFailure}
import histos.step.{Need, Phase, Step, StepId, StepInfo}

import zio.{Chunk, Layer, ZIO, ZLayer}

final case class PublishedModule(
    module: ProjectName,
    group: Organization,
    artifact: String,
    revision: String,
    jar: Option[Path],
    pom: Option[Path],
    sources: Option[Path],
    javadoc: Option[Path],
    ship: ShipName,
    key: Sha256,
    unique: Option[String],
    dependencies: List[MavenDependency],
):
  /** A snapshot prints its unique version. A directory publish prints the jar under `root`.
    */
  def rendered(root: Option[Path]): String =
    val place = unique match
      case Some(id) => id
      case None     =>
        jar match
          case None       => revision
          case Some(path) =>
            root match
              case None       => path.toString.replace('\\', '/')
              case Some(base) =>
                val from  = base.toAbsolutePath.normalize
                val full  = path.toAbsolutePath.normalize
                val shown =
                  if full.startsWith(from) then from.relativize(full) else full
                shown.toString.replace('\\', '/')
    s"${group.value} $artifact $revision $place"

/** Skipped aggregate members are named. Holds are the decision's hold lines. The modules are what was written.
  */
final case class PublishOutput(
    skipped: List[String],
    modules: List[PublishedModule],
    held: List[String],
    refused: List[String],
    notes: List[String] = Nil,
)

final case class PublishInputs(cells: List[histos.zinc.CompiledCell], generated: GeneratedRoots)

final case class PublishCompiler[R, E](
    ensure: List[BuildNode] => ZIO[R & zio.Scope, E | ExecutionError, ExecutionProducts]
)

object LocalPublish:
  type ProjectionError = LocalPublishError | StageRootError

  /** The executor owns generation. Its caller-scope leases stay live through jar and POM publication. */
  def runUsing[R, E](compiler: PublishCompiler[R, E])(
      repo: Path,
      graph: ProjectGraph,
      names: List[PublishName],
      env: Map[String, String],
      write: PublishWrite,
      api: ApiDigest,
      scope: ProofScope = ProofScope.All,
  ): ZIO[R & PublishEnv, E | ExecutionError | ProjectionError, PublishOutput] =
    runUsing(compiler, PublicationCompatibility.live)(repo, graph, names, env, write, api, scope)

  def runUsing[R, E](
      compiler: PublishCompiler[R, E],
      compatibility: Layer[histos.doctor.CompatibilityError, PublicationCompatibility],
  )(
      repo: Path,
      graph: ProjectGraph,
      names: List[PublishName],
      env: Map[String, String],
      write: PublishWrite,
      api: ApiDigest,
      scope: ProofScope,
  ): ZIO[R & PublishEnv, E | ExecutionError | ProjectionError, PublishOutput] =
    ZIO.scoped:
      go(
        repo,
        graph,
        names,
        env,
        write,
        api,
        compatibility,
        scope,
        stages =>
          for
            products <- compiler.ensure(stages.map(BuildNode.Compile(_)))
            cells    <- ZIO.foreach(stages)(stage => ZIO.fromEither(products.require(stage)).map(_.compiled))
          yield PublishInputs(cells, products.generatedRoots),
      )

  def sourceJar(
      repo: Path,
      graph: ProjectGraph,
      cell: Cell,
      generated: GeneratedRoots,
      destination: Path,
  ): zio.IO[StageRootError | JarError, Path] =
    for
      files <- StageRoots.of(graph).read(repo, CompilationStage(cell, SourceSet.Main), generated)
      jar   <- ClassJar.writeNamed(files.sources.map(file => file.entry -> file.path), destination)
    yield jar.path

  /** Compile the modules a decision releases and write them. A hold is reported and not written. A development session
    * is the snapshot at HEAD for check, local, and snapshots. A release write is not settled. Check returns before any
    * Ivy write.
    */
  def run(
      repo: Path,
      graph: ProjectGraph,
      names: List[PublishName],
      jdk: JdkRuntime,
      env: Map[String, String],
      write: PublishWrite,
      api: ApiDigest,
      report: histos.zinc.CompileFeed => zio.UIO[Unit] = _ => zio.ZIO.unit,
      plugins: Option[histos.load.PluginRuntime] = None,
      scope: ProofScope = ProofScope.All,
  ): ZIO[histos.resolve.Resolve & Publisher & ContentStore, ProjectionError, PublishOutput] =
    ZIO.scoped:
      for
        process    <- CacheConfig.processEnv
        remote     <- CacheConfig.open(process).mapError(LocalPublishError.Remote(_))
        resolution <- histos.resolve.ResolutionService.open(repo, graph).mapError(LocalPublishError.Classpath(_))
        lock       <- resolution.current
        runtime    <- histos.build.BuildRuntime
          .open(plugins)
          .provideSomeLayer(ZLayer.succeed(remote) ++ ZLayer.succeed(resolution))
        compiler = PublishCompiler[zio.Scope, histos.build.ExecutionError](roots =>
          runtime.ensure(
            histos.build.ExecutionRequest(
              histos.zinc.CompileSession(repo, graph, jdk, NativeOrigin.read(env), lock),
              roots,
              report,
            )
          )
        )
        written <- runUsing(compiler)(repo, graph, names, env, write, api, scope)
          .mapError {
            case error: LocalPublishError => error
            case error: StageRootError    => error
            case error: ExecutionError    => LocalPublishError.Build(histos.testrun.TestFailures(error))
          }
          .provideSomeLayer(ZLayer.succeed(remote))
      yield written

  private def go[R, E](
      repo: Path,
      graph: ProjectGraph,
      names: List[PublishName],
      env: Map[String, String],
      write: PublishWrite,
      api: ApiDigest,
      compatibility: Layer[histos.doctor.CompatibilityError, PublicationCompatibility],
      scope: ProofScope,
      compile: List[CompilationStage] => ZIO[R, E, PublishInputs],
  ): ZIO[R & PublishEnv, E | ProjectionError, PublishOutput] =
    for
      prepared <- SessionSettle.prepare(repo, graph.catalog, env, write)
      decision <- VersionTask
        .decide(graph, repo, prepared)
        .mapError(LocalPublishError.Version(_))
      chosen  <- ZIO.fromEither(scheme(decision, graph.catalog))
      written <-
        val skipped = (picked: Picked) => picked.skipped.map(name => LocalPublishError.NotPublished(name).message)
        val refused = (picked: Picked) => picked.refused.map(_.message)
        chosen.plans match
          case Nil =>
            chosen.held match
              case Nil => ZIO.fail(LocalPublishError.NothingToPublish)
              case _   => ZIO.succeed(PublishOutput(Nil, Nil, chosen.held, Nil))
          case plans if write == PublishWrite.Release && !plans.exists(plan => releaseRevision(plan.revision)) =>
            val members = plans.flatMap(_.members).toSet
            ZIO.succeed(PublishOutput(Nil, Nil, chosen.held, snapshotNotes(plans, members)))
          case plans =>
            ZIO
              .fromEither(choose(graph, names, plans))
              .flatMap: picked =>
                val (live, notes) = write match
                  case PublishWrite.Release                                             => releaseShips(plans, picked)
                  case PublishWrite.Check | PublishWrite.Local | PublishWrite.Snapshots => (picked, Nil)
                if live.modules.isEmpty then
                  if live.refused.nonEmpty || live.skipped.nonEmpty || chosen.held.nonEmpty || notes.nonEmpty then
                    ZIO.succeed(PublishOutput(skipped(live), Nil, chosen.held, refused(live) ++ notes))
                  else ZIO.fail(LocalPublishError.NothingToPublish)
                else
                  val checked = graph.catalog.ships.filter(_.members.exists(live.modules.contains))
                  val members = jvmMembers(graph, checked)
                  val proving = members.filter(PublicationCompatibility.enforced(write, scope).allows)
                  val noted   = proofNotes(write, scope, members, proving)
                  if write == PublishWrite.Check && proving.isEmpty then
                    ZIO.succeed(PublishOutput(skipped(live), Nil, chosen.held, refused(live) ++ notes, noted))
                  else
                    for
                      group  <- ZIO.fromEither(organization(graph))
                      _      <- ZIO.fromEither(requireFacts(graph, plans, live.modules))
                      inputs <- compile(compileStages(graph, write, live.modules, proving))
                      _      <- PublicationCompatibility.verify(
                        graph,
                        checked,
                        group,
                        inputs.cells,
                        compatibility,
                        PublicationCompatibility.enforced(write, scope),
                      )
                      modules <- write match
                        case PublishWrite.Check                                                 => ZIO.succeed(Nil)
                        case PublishWrite.Local | PublishWrite.Snapshots | PublishWrite.Release =>
                          publishJobs(
                            repo,
                            graph,
                            group,
                            jobs(plans, live.modules, inputs.cells),
                            write,
                            api,
                            inputs.generated,
                          )
                    yield PublishOutput(
                      skipped(live),
                      modules.sortBy(line => (line.group.value, line.artifact, line.revision)),
                      chosen.held,
                      refused(live) ++ notes,
                      noted,
                    )
    yield written

  private type PublishEnv = Publisher & Resolve & ContentStore & RemoteCache

  private def publishJobs(
      repo: Path,
      graph: ProjectGraph,
      group: Organization,
      jobs: List[Job],
      write: PublishWrite,
      api: ApiDigest,
      generated: GeneratedRoots,
  ): ZIO[PublishEnv, ProjectionError, List[PublishedModule]] =
    execute(combine(jobs.map: job =>
      val cell = job.cell
      val info = StepInfo(
        StepId.Cell(cell.module, cell.version, cell.platform, Phase.Publish),
        Chunk.empty,
        Set(Need.Io),
      )
      Step.local[PublishedModule](info)(stage(repo, graph, group, job, jobs, write, api, generated))))

  private def combine(
      steps: List[Step[PublishEnv, ProjectionError, PublishedModule]]
  ): Step[PublishEnv, ProjectionError, List[PublishedModule]] =
    steps match
      case Nil =>
        Step.local[List[PublishedModule]](
          StepInfo(StepId.Work("publish"), Chunk.empty, Set(Need.Io))
        )(ZIO.succeed(Nil))
      case head :: tail =>
        tail
          .foldLeft[Step[PublishEnv, ProjectionError, List[PublishedModule]]](
            head.map(List(_))
          ): (acc, step) =>
            acc.zip(step).map((all, next) => all :+ next)

  private def execute[A](
      goal: Step[PublishEnv, ProjectionError, A]
  ): ZIO[PublishEnv, ProjectionError, A] =
    ZIO.scoped:
      ZIO
        .serviceWithZIO[Histos](_.start(goal))
        .flatMap(_.outcome)
        .provideSome[PublishEnv & zio.Scope](Parallelism.live >>> Histos.live)
        .flatMap:
          case Outcome.Done(value)      => ZIO.succeed(value)
          case Outcome.Failed(failures) =>
            failures.head match
              case StepFailure.Failed(_, cause) => ZIO.failCause(cause)
              case StepFailure.Skipped(_, _)    =>
                ZIO.dieMessage("publish step was skipped")

  private final case class Planned(
      ship: ShipName,
      revision: Revision,
      members: List[ProjectName],
  )

  private final case class Scheme(plans: List[Planned], held: List[String])

  private final case class Job(
      ship: ShipName,
      revision: Revision,
      cell: histos.zinc.CompiledCell,
  )

  private enum RevisionKind:
    case Ci, Release, Snapshot, Pointer

  private def scheme(
      decision: Decision,
      catalog: histos.version.Catalog,
  ): Either[LocalPublishError, Scheme] =
    decision match
      case Decision.NoCatalog         => Left(LocalPublishError.NothingToPublish)
      case Decision.Develop(_, lines) =>
        lines match
          case line :: _ =>
            Left(LocalPublishError.Development(line.ship, line.compile))
      case Decision.Refused(refusals) =>
        Left(LocalPublishError.Version(VersionError.Refused(refusals)))
      case Decision.Publish(_, lines) =>
        lines.toList
          .foldLeft[Either[LocalPublishError, (List[Planned], List[String])]](
            Right((Nil, Nil))
          ): (acc, line) =>
            acc.flatMap: (seen, held) =>
              line.action match
                case PublishAction.Hold(_, _) =>
                  Right((seen, held :+ Decision.rendered(line)))
                case PublishAction.Release(version) =>
                  members(catalog, line.ship).map(mods =>
                    (
                      seen :+ Planned(line.ship, Revision.Release(version), mods.toList),
                      held,
                    )
                  )
                case PublishAction.Snapshot(_, stored, pointer) =>
                  members(catalog, line.ship).map: mods =>
                    val names      = mods.toList
                    val storedPlan = Planned(line.ship, Revision.Snapshot(stored), names)
                    val extra      = pointer.toList
                      .map(value => Planned(line.ship, Revision.Pointer(value), names))
                    (seen ++ (storedPlan :: extra), held)
          .flatMap: (plans, held) =>
            if plans.isEmpty && held.isEmpty then Left(LocalPublishError.NothingToPublish)
            else Right(Scheme(plans, held))

  private def members(
      catalog: histos.version.Catalog,
      ship: ShipName,
  ): Either[LocalPublishError, ::[ProjectName]] =
    catalog.get(ship) match
      case Some(row) => Right(row.members)
      case None      => Left(LocalPublishError.UnknownShip(ship))

  private def choose(
      graph: ProjectGraph,
      names: List[PublishName],
      plans: List[Planned],
  ): Either[LocalPublishError, Picked] =
    PublishSelect.pick(graph, names, writes(plans))

  private def writes(plans: List[Planned]): List[ShipWrite] =
    plans.foldLeft(List.empty[ShipWrite]): (seen, plan) =>
      val snapshot = PublishSelect.snapshotCoordinate(plan.revision.render)
      seen.find(_.ship == plan.ship) match
        case None    => seen :+ ShipWrite(plan.ship, plan.members, snapshot)
        case Some(_) =>
          seen.map: item =>
            if item.ship == plan.ship then item.copy(snapshot = item.snapshot || snapshot)
            else item

  /** Published JVM members of the ships being written. Order follows the catalog. */
  private def jvmMembers(graph: ProjectGraph, ships: List[Ship]): List[ProjectName] =
    ships
      .flatMap(_.members.toList)
      .distinct
      .filter: name =>
        graph.get(name) match
          case Some(module: Module) => module.platforms.all.contains(Platform.Jvm)
          case _                    => false

  /** Check compiles only the members it compares. A write compiles every module it publishes, and any extra JVM member
    * the proof still needs.
    */
  private def compileStages(
      graph: ProjectGraph,
      write: PublishWrite,
      publishing: Set[ProjectName],
      proving: List[ProjectName],
  ): List[CompilationStage] =
    val names = write match
      case PublishWrite.Check                                                 => proving
      case PublishWrite.Local | PublishWrite.Snapshots | PublishWrite.Release =>
        (publishing.toList ++ proving).distinct
    names.flatMap: name =>
      val every = write != PublishWrite.Check && publishing.contains(name)
      mainStages(graph, name, every)

  private def mainStages(graph: ProjectGraph, name: ProjectName, everyPlatform: Boolean): List[CompilationStage] =
    graph.get(name) match
      case Some(module: Module) =>
        for
          version  <- module.versions.all
          platform <- module.platforms.all
          if everyPlatform || platform == Platform.Jvm
        yield CompilationStage(Cell(name, version, platform), SourceSet.Main)
      case _ => Nil

  /** A member outside the scope is named, not described as compatible. An empty selection is not a full proof. */
  private def proofNotes(
      write: PublishWrite,
      scope: ProofScope,
      members: List[ProjectName],
      proving: List[ProjectName],
  ): List[String] =
    write match
      case PublishWrite.Snapshots | PublishWrite.Release => Nil
      case PublishWrite.Check | PublishWrite.Local       =>
        val omitted = members.filterNot(proving.toSet.contains).map(_.value).sorted
        val named   = if omitted.isEmpty then Nil else List(omitted.mkString("not checked: ", ", ", ""))
        val empty   = scope match
          case ProofScope.Selected(_) if proving.isEmpty => List("nothing was selected")
          case ProofScope.All | ProofScope.Selected(_)   => Nil
        empty ++ named

  private def organization(
      graph: ProjectGraph
  ): Either[LocalPublishError, Organization] =
    graph.organization match
      case Some(group) => Right(group)
      case None        => Left(LocalPublishError.NoOrganization)

  /** A release coordinate needs a license, a developer, and an scm. A snapshot does not. */
  private def requireFacts(
      graph: ProjectGraph,
      plans: List[Planned],
      modules: Set[ProjectName],
  ): Either[LocalPublishError, Unit] =
    val releasing = plans.exists: plan =>
      releaseRevision(plan.revision) && plan.members.exists(modules.contains)
    if !releasing then Right(())
    else
      val missing = List(
        Option.when(graph.facts.license.isEmpty)(PomFact.License),
        Option.when(graph.facts.developer.isEmpty)(PomFact.Developer),
        Option.when(graph.facts.scm.isEmpty)(PomFact.Scm),
      ).flatten
      missing match
        case Nil          => Right(())
        case head :: tail => Left(LocalPublishError.Facts(::(head, tail)))

  private def releaseShips(plans: List[Planned], picked: Picked): (Picked, List[String]) =
    val live = picked.modules.filter: module =>
      plans.exists(plan => plan.members.contains(module) && releaseRevision(plan.revision))
    (
      picked.copy(modules = live, roots = picked.roots.intersect(live)),
      snapshotNotes(plans, picked.modules),
    )

  private def snapshotNotes(plans: List[Planned], selected: Set[ProjectName]): List[String] =
    plans
      .map(_.ship)
      .distinct
      .flatMap: ship =>
        val mine     = plans.filter(_.ship == ship)
        val chosen   = mine.exists(_.members.exists(selected.contains))
        val releases = mine.exists(plan => releaseRevision(plan.revision))
        if chosen && !releases then
          mine match
            case plan :: _ => List(s"${ship.value} is ${plan.revision.render}, which is not a release")
            case Nil       => Nil
        else Nil

  private def releaseRevision(revision: Revision): Boolean =
    revision match
      case Revision.Release(_) => true
      case _                   => false

  private[publish] def document(graph: ProjectGraph, ship: ShipName, module: Module): PomDocument =
    val row = graph.catalog.get(ship)
    PomDocument(
      description = module.description.orElse(row.flatMap(_.description)).orElse(graph.facts.description),
      url = row.flatMap(_.url).orElse(graph.facts.url),
      license = graph.facts.license,
      developer = graph.facts.developer,
      scm = graph.facts.scm,
      organizationName = graph.facts.organizationName,
      organizationHomepage = graph.facts.organizationHomepage,
      versionScheme = graph.facts.versionScheme,
      name = Some(histos.PomText.trusted(module.published.fold(module.name.value)(_.value))),
    )

  private def publishedRevision(
      published: List[(ProjectName, Revision)],
      module: ProjectName,
      dependency: ProjectName,
      writing: Revision,
  ): Either[LocalPublishError, String] =
    val wanted = kind(writing)
    published.filter((name, revision) => name == dependency && kind(revision) == wanted) match
      case (_, revision) :: _ => Right(revision.render)
      case Nil                => Left(LocalPublishError.UnpublishedDependency(module, dependency))

  private def kind(revision: Revision): RevisionKind =
    revision match
      case Revision.Ci(_)       => RevisionKind.Ci
      case Revision.Release(_)  => RevisionKind.Release
      case Revision.Snapshot(_) => RevisionKind.Snapshot
      case Revision.Pointer(_)  => RevisionKind.Pointer

  private def jobs(
      plans: List[Planned],
      chosen: Set[ProjectName],
      cells: List[histos.zinc.CompiledCell],
  ): List[Job] =
    plans.flatMap: plan =>
      val names = plan.members.filter(chosen.contains).toSet
      cells
        .filter(cell => names.contains(cell.module))
        .map(cell => Job(plan.ship, plan.revision, cell))

  private def stage(
      repo: Path,
      graph: ProjectGraph,
      group: Organization,
      job: Job,
      jobs: List[Job],
      write: PublishWrite,
      api: ApiDigest,
      generated: GeneratedRoots,
  ): ZIO[PublishEnv, ProjectionError, PublishedModule] =
    val cell     = job.cell
    val artifact = artifactName(graph, cell.module, cell.version, cell.platform)
    stageLibrary(repo, graph, group, job, jobs, artifact, write, api, generated)

  private def stageLibrary(
      repo: Path,
      graph: ProjectGraph,
      group: Organization,
      job: Job,
      jobs: List[Job],
      artifact: String,
      write: PublishWrite,
      api: ApiDigest,
      generated: GeneratedRoots,
  ): ZIO[PublishEnv, ProjectionError, PublishedModule] =
    val cell = job.cell
    for
      name <- artifactId(group, artifact, job.revision.render)
      deps <- ZIO.fromEither(
        dependencies(
          graph,
          group,
          cell.module,
          cell.version,
          cell.platform,
          job.revision,
          jobs.map(item => item.cell.module -> item.revision),
        )
      )
      module <- ZIO.fromEither(moduleOf(graph, cell.module))
      packed <- StageRoots
        .of(graph)
        .read(
          repo,
          CompilationStage(Cell(cell.module, cell.version, cell.platform), SourceSet.Main),
          generated,
        )
      jarPath = scratch(repo, cell, job.revision, Scratch.Classes)
      stamp  <- versionStamp(repo, graph, cell, module, api)
      store  <- ZIO.service[ContentStore]
      remote <- ZIO.service[RemoteCache]
      ready  <- preparedLibrary(repo, cell, module, stamp, deps, group, name, job.revision)
      bytes  <- ProductCache
        .materialize(remote, store, ready.key, jarPath, ready.write(jarPath))
        .mapError(cached)
      stamped <- stampAssembly(repo, graph, cell, module, jarPath, bytes)
      sources <- ClassJar
        .writeNamed(
          packed.sources.map(file => file.entry -> file.path),
          scratch(repo, cell, job.revision, Scratch.SourceJar),
        )
        .mapError(error => LocalPublishError.Jar(cell.module, cell.version, cell.platform, error))
      javadoc     <- documented(repo, cell, job.revision, write)
      sourceBytes <- bytesOf(sources.path)
      published   <- publishOne(
        group,
        name,
        job.revision,
        stamped,
        deps,
        document(graph, job.ship, module),
        Some(sourceBytes),
        javadoc,
        artifact,
        cell.module,
        job.ship,
        ready.key,
      )
    yield published

  /** The class-jar cache stays unstamped. Only the launcher jar carries this commit and the shim digest. */
  private def stampAssembly(
      repo: Path,
      graph: ProjectGraph,
      cell: histos.zinc.CompiledCell,
      module: Module,
      jarPath: Path,
      bytes: Chunk[Byte],
  ): ZIO[Any, LocalPublishError, Chunk[Byte]] =
    module.publication match
      case Publication.Assembly(_) =>
        val jarred = (error: JarError) => LocalPublishError.Jar(cell.module, cell.version, cell.platform, error)
        graph.catalog.ships.find(_.toolchain.isDefined) match
          case None       => ZIO.fail(LocalPublishError.NoToolchain)
          case Some(ship) =>
            for
              found    <- ToolchainIdentity.discover(repo, ship.release)
              identity <- found match
                case Some(value) => ZIO.succeed(value)
                case None        => ZIO.fail(LocalPublishError.NoCommit)
              shim    <- shimDigest(repo)
              stamped <- LauncherStamp.stamp(jarPath, identity, shim).mapError(jarred)
              _       <- writeBytes(jarPath, stamped)
            yield stamped
      case Publication.Library | Publication.SbtPlugin(_) | Publication.Application =>
        ZIO.succeed(bytes)

  private def shimDigest(repo: Path): ZIO[Any, LocalPublishError, Sha256] =
    val source = repo.resolve(PublishedShim.fileName)
    ZIO
      .attemptBlocking {
        if !Files.isRegularFile(source) then Left(LocalPublishError.Read(source))
        else Right(Sha256Digest.of(Chunk.fromArray(Files.readAllBytes(source))))
      }
      .mapError(_ => LocalPublishError.Read(source))
      .flatMap(ZIO.fromEither)

  private final case class PreparedLibrary(
      key: Sha256,
      write: Path => ZIO[Any, LocalPublishError, Chunk[Byte]],
  )

  /** A plugin library is sealed before the cache runs. An ordinary library keeps the semantic product key. */
  private def preparedLibrary(
      repo: Path,
      cell: histos.zinc.CompiledCell,
      module: Module,
      stamp: List[(String, Path)],
      dependencies: List[MavenDependency],
      group: Organization,
      name: ArtifactId,
      revision: Revision,
  ): ZIO[Any, LocalPublishError, PreparedLibrary] =
    module.pluginLibrary match
      case Some(mode) =>
        ZIO
          .fromEither(recipeNode(cell.module))
          .flatMap: node =>
            PluginBundle
              .library(
                mode,
                node,
                cell.capture,
                repo,
                cell.classes,
                stamp,
                dependencies,
                group,
                name,
                revision,
              )
              .mapError(fault => rejected(cell, fault))
              .map: staged =>
                PreparedLibrary(staged.key, path => writeBytes(path, staged.bytes))
      case None =>
        productDigests(List(cell.classes), cell).flatMap: products =>
          val keyed =
            if stamp.isEmpty then ZIO.succeed(PackageKey.of(products))
            else PluginBundle.legacyKey(cell.classes, stamp).mapError(fault => rejected(cell, fault))
          keyed.map: key =>
            PreparedLibrary(
              key,
              jarPath =>
                ClassJar
                  .write(cell.classes, Nil, jarPath, stamp, ClassJar.semanticdb)
                  .mapError(error => LocalPublishError.Jar(cell.module, cell.version, cell.platform, error))
                  .flatMap(_ => bytesOf(jarPath)),
            )

  private def writeBytes(path: Path, bytes: Chunk[Byte]): ZIO[Any, LocalPublishError, Chunk[Byte]] =
    ZIO
      .attemptBlocking {
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        Files.write(path, bytes.toArray)
        bytes
      }
      .mapError(_ => LocalPublishError.Io(path.toString))

  private def recipeNode(module: ProjectName): Either[LocalPublishError, RecipeNodeId] =
    RecipeNodeId
      .from(module.value)
      .left
      .map: _ =>
        LocalPublishError.Recipe(RecipeError.Unsupported(RecipeFeature("node"), RecipeNodeId.trusted("module")))

  private def rejected(cell: histos.zinc.CompiledCell, fault: PluginPublishFault): LocalPublishError =
    val jarred = (error: JarError) => LocalPublishError.Jar(cell.module, cell.version, cell.platform, error)
    fault match
      case PluginPublishFault.Recipe(error) =>
        LocalPublishError.Recipe(error)
      case PluginPublishFault.Changed(path) =>
        LocalPublishError.InputsChanged(path)
      case PluginPublishFault.Seal(error) =>
        LocalPublishError.Recipe(RecipeError.Seal(error))
      case PluginPublishFault.Jar(error) =>
        jarred(error)
      case PluginPublishFault.Dependency(group, name, revision) =>
        LocalPublishError.BadDependency(group, name, revision)
      case PluginPublishFault.Sources(error) =>
        LocalPublishError.SourceRead(cell.module, cell.version, cell.platform, error)
      case PluginPublishFault.Io(failure) =>
        LocalPublishError.Io(failure.path.toString)
      case PluginPublishFault.Entry(name) =>
        jarred(JarError.Escaped(name))
      case PluginPublishFault.Publish(error) =>
        LocalPublishError.Publish(error)

  private def artifactId(
      group: Organization,
      artifact: String,
      revision: String,
  ): ZIO[Any, LocalPublishError, ArtifactId] =
    ZIO
      .fromEither(ArtifactId.from(artifact))
      .mapError(_ => LocalPublishError.BadDependency(group.value, artifact, revision))

  private def publishOne(
      group: Organization,
      name: ArtifactId,
      revision: Revision,
      jar: Chunk[Byte],
      deps: List[MavenDependency],
      document: PomDocument,
      sources: Option[Chunk[Byte]],
      javadoc: Option[Chunk[Byte]],
      artifact: String,
      module: ProjectName,
      ship: ShipName,
      key: Sha256,
  ): ZIO[Publisher, LocalPublishError, PublishedModule] =
    ZIO
      .serviceWithZIO[Publisher](
        _.publish(Artifact(group, name, revision, jar, deps, document, sources, javadoc))
      )
      .mapError(LocalPublishError.Publish(_))
      .map: published =>
        val placedHere = placed(published)
        PublishedModule(
          module,
          group,
          artifact,
          revision.render,
          placedHere.jar,
          placedHere.pom,
          placedHere.sources,
          placedHere.javadoc,
          ship,
          key,
          placedHere.unique,
          deps,
        )

  private def moduleOf(
      graph: ProjectGraph,
      name: ProjectName,
  ): Either[LocalPublishError, Module] =
    graph.get(name) match
      case Some(module: Module) => Right(module)
      case _                    => Left(LocalPublishError.Compile(GraphCompileError.NoRoot(name)))

  private def productDigests(
      dirs: List[Path],
      cell: histos.zinc.CompiledCell,
  ): ZIO[Any, LocalPublishError, List[Sha256]] =
    ZIO.foreach(dirs): classes =>
      val found = GraphCompile.analysisDirOf(classes) match
        case Some(dir) => ProductDigest.read(dir)
        case None      => Left(CompileError.Io(IoFailure(classes, IoFault.Stat)))
      ZIO
        .fromEither(found)
        .mapError(error =>
          LocalPublishError.Compile(GraphCompileError.Failed(cell.module, cell.version, cell.platform, error))
        )

  private final case class Placed(
      jar: Option[Path],
      pom: Option[Path],
      sources: Option[Path],
      javadoc: Option[Path],
      unique: Option[String],
  )

  private def placed(published: Published): Placed =
    published match
      case Published.Local(jar, pom, sources, javadoc, _) =>
        Placed(Some(jar), Some(pom), sources, javadoc, None)
      case Published.Snapshot(unique, _) =>
        Placed(None, None, None, None, Some(unique.value))

  private[publish] def dependencies(
      graph: ProjectGraph,
      group: Organization,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      revision: Revision,
      published: List[(ProjectName, Revision)],
  ): Either[LocalPublishError, List[MavenDependency]] =
    graph.get(module) match
      case Some(found: Module) =>
        val internal = found
          .upstream(platform, SourceSet.Main)
          .foldLeft[Either[LocalPublishError, List[MavenDependency]]](
            Right(Nil)
          ): (acc, dependency) =>
            acc.flatMap: seen =>
              publishedRevision(published, module, dependency, revision).flatMap: at =>
                coordinate(
                  group,
                  artifactName(graph, dependency, version, platform),
                  at,
                ).map(seen :+ _)
        val external = Dep
          .declared(found.dependencies, platform)
          .foldLeft[Either[LocalPublishError, List[MavenDependency]]](
            Right(Nil)
          ): (acc, dependency) =>
            acc.flatMap: seen =>
              externalCoordinate(
                dependency.organization,
                ArtifactName.of(dependency, version, platform),
                dependency.revision.value,
                dependency.exclusions,
                dependency.link,
              ).map(seen :+ _)
        for
          inside  <- internal
          outside <- external
        yield inside ++ outside
      case _ => Right(Nil)

  private def externalCoordinate(
      group: String,
      artifact: String,
      revision: String,
      exclusions: List[DepExclusion],
      link: DepLink,
  ): Either[LocalPublishError, MavenDependency] =
    Organization.from(group) match
      case Left(_) =>
        Left(LocalPublishError.BadDependency(group, artifact, revision))
      case Right(parsed) => coordinate(parsed, artifact, revision, exclusions, scopeOf(link))

  private def scopeOf(link: DepLink): MavenScope =
    link match
      case DepLink.Provided                        => MavenScope.Provided
      case DepLink.Published | DepLink.CompileOnly => MavenScope.Compile

  private def coordinate(
      group: Organization,
      artifact: String,
      revision: String,
      exclusions: List[DepExclusion] = Nil,
      scope: MavenScope = MavenScope.Compile,
  ): Either[LocalPublishError, MavenDependency] =
    for
      name <- ArtifactId
        .from(artifact)
        .left
        .map(_ => LocalPublishError.BadDependency(group.value, artifact, revision))
      version <- DependencyVersion
        .from(revision)
        .left
        .map(_ => LocalPublishError.BadDependency(group.value, artifact, revision))
    yield MavenDependency(group, name, version, exclusions, scope)

  private def artifactName(
      graph: ProjectGraph,
      name: ProjectName,
      version: ScalaVersion,
      platform: Platform,
  ): String =
    graph.get(name) match
      case Some(module: Module) => ArtifactName.module(module, version, platform)
      case _                    => ArtifactName.published(name.value, version, platform)

  private def cached(error: LocalPublishError | RemoteError | CacheError): LocalPublishError =
    error match
      case found: LocalPublishError => found
      case found: RemoteError       => LocalPublishError.Remote(found)
      case found: CacheError        => LocalPublishError.Runtime(CompileError.Stored(found))

  private def bytesOf(path: Path): ZIO[Any, LocalPublishError, Chunk[Byte]] =
    ZIO
      .attemptBlocking(Chunk.fromArray(Files.readAllBytes(path)))
      .mapError(_ => LocalPublishError.Read(path))

  /** A snapshot omits the javadoc jar. A local publish and a release upload the stub page. */
  private def documented(
      repo: Path,
      cell: histos.zinc.CompiledCell,
      revision: Revision,
      write: PublishWrite,
  ): ZIO[Any, LocalPublishError, Option[Chunk[Byte]]] =
    write match
      case PublishWrite.Check | PublishWrite.Snapshots => ZIO.succeed(None)
      case PublishWrite.Local | PublishWrite.Release   =>
        val path = scratch(repo, cell, revision, Scratch.Javadoc)
        JavadocStub
          .write(path)
          .mapError(error => LocalPublishError.Jar(cell.module, cell.version, cell.platform, error))
          .flatMap(_ => bytesOf(path))
          .map(Some(_))

  private enum Scratch:
    case Classes, SourceJar, Javadoc

    def file(platform: String, revision: Revision): String =
      val stem = revision match
        case Revision.Pointer(_) => s"$platform-pointer"
        case _                   => platform
      this match
        case Classes   => s"$stem.jar"
        case SourceJar => s"$stem-sources.jar"
        case Javadoc   => s"$stem-javadoc.jar"
  end Scratch

  /** The binary jar of a plugin library, and of a module the graph names as a plugin. The sources jar is left alone.
    * Catalog coordinates stay on the load graph, so publication does not wait for this graph to name the module.
    */
  private def versionStamp(
      repo: Path,
      graph: ProjectGraph,
      cell: histos.zinc.CompiledCell,
      module: Module,
      api: ApiDigest,
  ): ZIO[Any, LocalPublishError, List[(String, Path)]] =
    if !versioned(graph, module) then ZIO.succeed(Nil)
    else
      val path = repo
        .resolve(".histos")
        .resolve("jars")
        .resolve(cell.module.value)
        .resolve(cell.version.value)
        .resolve(s"${cell.platform.token}-api-digest")
      Workspace
        .writeString(path, s"${api.value}\n")
        .mapError(_ => LocalPublishError.Io(path.toString))
        .as(List(ApiDigest.resource -> path))

  private[histos] def versioned(graph: ProjectGraph, module: Module): Boolean =
    module.pluginLibrary.isDefined || graph.coordinates.exists:
      case PluginCoordinate.Module(name)    => name == module.name
      case PluginCoordinate.Published(_, _) => false

  private def scratch(
      repo: Path,
      cell: histos.zinc.CompiledCell,
      revision: Revision,
      kind: Scratch,
  ): Path =
    repo
      .resolve(".histos")
      .resolve("jars")
      .resolve(cell.module.value)
      .resolve(cell.version.value)
      .resolve(kind.file(cell.platform.token, revision))
end LocalPublish
