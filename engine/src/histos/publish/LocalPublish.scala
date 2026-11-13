package histos.publish

import histos.jdk.{JdkRuntime, Sha256}
import histos.central.{
  Artifact,
  ArtifactId,
  DependencyVersion,
  MavenDependency,
  PomDocument,
  PublishError,
  Published,
  Publisher,
  Revision,
}
import histos.native.NativeOrigin
import histos.doc.DocClasspath
import histos.cache.{ContentStore, ProductCache}
import histos.pack.{Assembly, AssemblyError, AssemblyFile, ClassJar, JarIo, JavadocJar, PackageKey}
import histos.plugin.{ApiDigest, PluginCoordinate}
import histos.resolve.{ArtifactName, BuildResolvers, ClasspathScope, LockedTools, Resolve, ResolveCell, ResolveError}
import histos.version.{Decision, PublishAction, ShipName, VersionError, VersionTask}
import histos.select.SelectError
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.{CompileError, GraphCompile, GraphCompileError, JvmCompile, ProductDigest, Sources}
import histos.{
  Dep,
  DepExclusion,
  DepLink,
  MavenScope,
  MainClass,
  Module,
  Organization,
  Platform,
  PomFact,
  ProjectGraph,
  ProjectName,
  Publication,
  ScalaVersion,
}

import java.nio.file.{Files, Path}

import histos.schedule.{CacheConfig, CacheError, Histos, Outcome, Parallelism, RemoteCache, RemoteError, StepFailure}
import histos.step.{Need, Phase, Step, StepId, StepInfo}

import zio.{Chunk, ZIO, ZLayer}

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
)

object LocalPublish:
  /** Compile the modules a decision releases and write them. A hold is reported and not written. A development session
    * is the snapshot at HEAD for a local or snapshot write. A release write is not settled.
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
  ): ZIO[histos.resolve.Resolve & Publisher & ContentStore, LocalPublishError, PublishOutput] =
    ZIO.scoped:
      for
        process <- CacheConfig.processEnv
        remote  <- CacheConfig.open(process).mapError(LocalPublishError.Remote(_))
        written <- go(repo, graph, names, jdk, env, write, api, report).provideSomeLayer(ZLayer.succeed(remote))
      yield written

  private def go(
      repo: Path,
      graph: ProjectGraph,
      names: List[PublishName],
      jdk: JdkRuntime,
      env: Map[String, String],
      write: PublishWrite,
      api: ApiDigest,
      report: histos.zinc.CompileFeed => zio.UIO[Unit],
  ): ZIO[histos.resolve.Resolve & Publisher & ContentStore & RemoteCache, LocalPublishError, PublishOutput] =
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
            // No release coordinate is compiled. Selection still runs when another ship is a release, so a
            // snapshot dependency can refuse that ship.
            val members = plans.flatMap(_.members).toSet
            ZIO.succeed(PublishOutput(Nil, Nil, chosen.held, snapshotNotes(plans, members)))
          case plans =>
            ZIO
              .fromEither(choose(graph, names, plans))
              .flatMap: picked =>
                // The snapshot ship stays writable so a dependent is refused, then it is not staged.
                val (live, notes) = write match
                  case PublishWrite.Release => releaseShips(plans, picked)
                  case _                    => (picked, Nil)
                if live.modules.isEmpty then
                  if live.refused.nonEmpty || live.skipped.nonEmpty || chosen.held.nonEmpty || notes.nonEmpty then
                    ZIO.succeed(PublishOutput(skipped(live), Nil, chosen.held, refused(live) ++ notes))
                  else ZIO.fail(LocalPublishError.NothingToPublish)
                else
                  for
                    group <- ZIO.fromEither(organization(graph))
                    _     <- ZIO.fromEither(requireFacts(graph, plans, live.modules))
                    cells <- GraphCompile
                      .run(
                        repo,
                        graph,
                        live.modules.toList,
                        jdk,
                        NativeOrigin.read(env),
                        None,
                        report,
                      )
                      .mapError(LocalPublishError.Compile(_))
                    modules <- publishJobs(
                      repo,
                      graph,
                      group,
                      jobs(plans, live.modules, cells),
                      cells,
                      write,
                      api,
                    )
                  yield PublishOutput(
                    skipped(live),
                    modules.sortBy(line => (line.group.value, line.artifact, line.revision)),
                    chosen.held,
                    refused(live) ++ notes,
                  )
    yield written

  private type PublishEnv = Publisher & Resolve & ContentStore & RemoteCache

  private def publishJobs(
      repo: Path,
      graph: ProjectGraph,
      group: Organization,
      jobs: List[Job],
      cells: List[histos.zinc.CompiledCell],
      write: PublishWrite,
      api: ApiDigest,
  ): ZIO[PublishEnv, LocalPublishError, List[PublishedModule]] =
    execute(combine(jobs.map: job =>
      val cell = job.cell
      val info = StepInfo(
        StepId.Cell(cell.module, cell.version, cell.platform, Phase.Publish),
        Chunk.empty,
        Set(Need.Io),
      )
      Step.local[PublishedModule](info)(stage(repo, graph, group, job, jobs, cells, write, api))))

  private def combine(
      steps: List[Step[PublishEnv, LocalPublishError, PublishedModule]]
  ): Step[PublishEnv, LocalPublishError, List[PublishedModule]] =
    steps match
      case Nil =>
        Step.local[List[PublishedModule]](
          StepInfo(StepId.Work("publish"), Chunk.empty, Set(Need.Io))
        )(ZIO.succeed(Nil))
      case head :: tail =>
        tail
          .foldLeft[Step[PublishEnv, LocalPublishError, List[PublishedModule]]](
            head.map(List(_))
          ): (acc, step) =>
            acc.zip(step).map((all, next) => all :+ next)

  private def execute[A](
      goal: Step[PublishEnv, LocalPublishError, A]
  ): ZIO[PublishEnv, LocalPublishError, A] =
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

  private def document(graph: ProjectGraph, ship: ShipName): PomDocument =
    val row = graph.catalog.get(ship)
    PomDocument(
      description = row.flatMap(_.description).orElse(graph.facts.description),
      url = row.flatMap(_.url).orElse(graph.facts.url),
      license = graph.facts.license,
      developer = graph.facts.developer,
      scm = graph.facts.scm,
    )

  private def publication(graph: ProjectGraph, name: ProjectName): Publication =
    graph.get(name) match
      case Some(module: Module) => module.publication
      case _                    => Publication.Library

  private def publishedRevision(
      jobs: List[Job],
      module: ProjectName,
      dependency: ProjectName,
      writing: Revision,
  ): Either[LocalPublishError, String] =
    val wanted = kind(writing)
    jobs.filter(job => job.cell.module == dependency && kind(job.revision) == wanted) match
      case job :: _ => Right(job.revision.render)
      case Nil      => Left(LocalPublishError.UnpublishedDependency(module, dependency))

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
      cells: List[histos.zinc.CompiledCell],
      write: PublishWrite,
      api: ApiDigest,
  ): ZIO[PublishEnv, LocalPublishError, PublishedModule] =
    val cell     = job.cell
    val artifact = artifactName(graph, cell.module, cell.version, cell.platform)
    publication(graph, cell.module) match
      case Publication.Assembly(main) =>
        stageAssembly(repo, graph, group, job, cells, main, artifact, write, api)
      case Publication.Library | Publication.SbtPlugin(_) | Publication.Application =>
        stageLibrary(repo, graph, group, job, jobs, artifact, write, api)

  private def stageLibrary(
      repo: Path,
      graph: ProjectGraph,
      group: Organization,
      job: Job,
      jobs: List[Job],
      artifact: String,
      write: PublishWrite,
      api: ApiDigest,
  ): ZIO[PublishEnv, LocalPublishError, PublishedModule] =
    val cell = job.cell
    for
      products <- productDigests(List(cell.classes), cell)
      name     <- artifactId(group, artifact, job.revision.render)
      deps     <- ZIO.fromEither(dependencies(graph, group, cell, job.revision, jobs))
      module   <- ZIO.fromEither(moduleOf(graph, cell.module))
      packed   <- sourcesOf(repo, graph, cell)
      jarPath = scratch(repo, cell, job.revision, Scratch.Classes)
      stamp  <- versionStamp(repo, graph, cell, api)
      store  <- ZIO.service[ContentStore]
      remote <- ZIO.service[RemoteCache]
      key = PackageKey.of(products)
      bytes <- ProductCache
        .materialize(
          remote,
          store,
          key,
          jarPath,
          ClassJar
            .write(cell.classes, Nil, jarPath, stamp)
            .mapError(error => LocalPublishError.Jar(cell.module, cell.version, cell.platform, error))
            .flatMap(_ => bytesOf(jarPath)),
        )
        .mapError(cached)
      sources <- ClassJar
        .writeNamed(packed, scratch(repo, cell, job.revision, Scratch.SourceJar))
        .mapError(error => LocalPublishError.Jar(cell.module, cell.version, cell.platform, error))
      javadoc     <- documented(repo, graph, cell, job.revision, write)
      sourceBytes <- bytesOf(sources.path)
      published   <- publishOne(
        group,
        name,
        job.revision,
        bytes,
        deps,
        document(graph, job.ship),
        Some(sourceBytes),
        javadoc,
        artifact,
        cell.module,
        job.ship,
        key,
      )
    yield published

  private def stageAssembly(
      repo: Path,
      graph: ProjectGraph,
      group: Organization,
      job: Job,
      cells: List[histos.zinc.CompiledCell],
      main: MainClass,
      artifact: String,
      write: PublishWrite,
      api: ApiDigest,
  ): ZIO[PublishEnv, LocalPublishError, PublishedModule] =
    val cell = job.cell
    for
      name     <- artifactId(group, artifact, job.revision.render)
      module   <- ZIO.fromEither(moduleOf(graph, cell.module))
      upstream <- upstreamOf(repo, graph, module, cell, cells)
      dirs = cell.classes :: upstream.map(_.classes)
      products <- productDigests(dirs, cell)
      jars     <- runtimeJars(
        repo,
        graph,
        (module -> cell.version) :: upstream.map(found => found.module -> found.version),
        cell,
      )
      packed <- sourcesOf(repo, graph, cell)
      jarPath = scratch(repo, cell, job.revision, Scratch.Classes)
      stamp  <- versionStamp(repo, graph, cell, api)
      store  <- ZIO.service[ContentStore]
      remote <- ZIO.service[RemoteCache]
      key = PackageKey.of(products)
      bytes <- ProductCache
        .materialize(
          remote,
          store,
          key,
          jarPath,
          Assembly
            .write(
              dirs,
              jars,
              histos.pack.MainClass.trusted(main.value),
              jarPath,
              replacing = stamp.map((entry, file) => AssemblyFile(entry, file)),
            )
            .mapError(error => LocalPublishError.Pack(cell.module, cell.version, cell.platform, error))
            .flatMap(_ => bytesOf(jarPath)),
        )
        .mapError(cached)
      sources <- ClassJar
        .writeNamed(packed, scratch(repo, cell, job.revision, Scratch.SourceJar))
        .mapError(error => LocalPublishError.Jar(cell.module, cell.version, cell.platform, error))
      javadoc     <- documented(repo, graph, cell, job.revision, write)
      sourceBytes <- bytesOf(sources.path)
      published   <- publishOne(
        group,
        name,
        job.revision,
        bytes,
        Nil,
        document(graph, job.ship),
        Some(sourceBytes),
        javadoc,
        artifact,
        cell.module,
        job.ship,
        key,
      )
    yield published

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

  /** The upstream cells the assembled cell's compile read, from the cells this publish compiled. */
  private def upstreamOf(
      repo: Path,
      graph: ProjectGraph,
      module: Module,
      cell: histos.zinc.CompiledCell,
      cells: List[histos.zinc.CompiledCell],
  ): ZIO[Resolve, LocalPublishError, List[histos.zinc.UpstreamCell]] =
    for
      lock     <- Resolve.readLock(repo).mapError(LocalPublishError.Classpath(_))
      upstream <- histos.zinc.UpstreamCells
        .read(
          module,
          Module.chain(module, modulesOf(graph), cell.platform),
          cell.version,
          cell.platform,
          histos.zinc.UpstreamCells.compiled(cells),
        )
        .mapError(LocalPublishError.Compile(_))
        .provideSome[Resolve](ZLayer.fromZIO(LockedTools.make(lock, graph)))
    yield upstream

  /** The JVM runtime of `cell`, then each module's runtime jars at its own cell's version. */
  private def runtimeJars(
      repo: Path,
      graph: ProjectGraph,
      placed: List[(Module, ScalaVersion)],
      cell: histos.zinc.CompiledCell,
  ): ZIO[Resolve, LocalPublishError, List[Path]] =
    val platform = cell.platform
    for
      lock    <- Resolve.readLock(repo).mapError(LocalPublishError.Classpath(_))
      runtime <- JvmCompile
        .runtime(cell.version)
        .mapError(LocalPublishError.Runtime(_))
        .provideSome[Resolve](ZLayer.fromZIO(LockedTools.make(lock, graph)))
      external <- ZIO.foldLeft(placed)(List.empty[Path]): (acc, entry) =>
        val (item, version) = entry
        val request         = ResolveCell(
          item.name,
          Dep.runtime(item.dependencies, platform),
          version,
          platform,
          BuildResolvers(graph.resolvers),
          ClasspathScope.Compile,
        )
        Resolve
          .materialize(request, lock)
          .mapBoth(
            LocalPublishError.Classpath(_),
            classpath =>
              val fresh = classpath.artifacts.map(_.path).filterNot(path => acc.contains(path))
              acc ++ fresh,
          )
    yield runtime ++ external.filterNot(path => runtime.contains(path))

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

  private def dependencies(
      graph: ProjectGraph,
      group: Organization,
      cell: histos.zinc.CompiledCell,
      revision: Revision,
      jobs: List[Job],
  ): Either[LocalPublishError, List[MavenDependency]] =
    graph.get(cell.module) match
      case Some(found: Module) =>
        val internal = found
          .upstream(cell.platform)
          .foldLeft[Either[LocalPublishError, List[MavenDependency]]](
            Right(Nil)
          ): (acc, dependency) =>
            acc.flatMap: seen =>
              publishedRevision(jobs, cell.module, dependency, revision).flatMap: version =>
                coordinate(
                  group,
                  artifactName(graph, dependency, cell.version, cell.platform),
                  version,
                ).map(seen :+ _)
        val external = Dep
          .declared(found.dependencies, cell.platform)
          .foldLeft[Either[LocalPublishError, List[MavenDependency]]](
            Right(Nil)
          ): (acc, dependency) =>
            acc.flatMap: seen =>
              externalCoordinate(
                dependency.organization,
                ArtifactName.of(dependency, cell.version, cell.platform),
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

  /** Upstream modules on every platform the module builds. */
  private def upstream(module: Module): List[ProjectName] =
    module.platforms.all.flatMap(module.upstream).distinct

  private def sourcesOf(
      repo: Path,
      graph: ProjectGraph,
      cell: histos.zinc.CompiledCell,
  ): ZIO[Any, LocalPublishError, List[(String, Path)]] =
    graph.get(cell.module) match
      case Some(module: Module) =>
        GraphCompile
          .sourceBase(repo, module, modulesOf(graph))
          .mapError(LocalPublishError.Compile(_))
          .flatMap:
            case None =>
              ZIO.fail(LocalPublishError.Compile(GraphCompileError.NoRoot(cell.module)))
            case Some(root) =>
              Sources
                .packaged(root, cell.version, cell.platform)
                .mapError(error => LocalPublishError.SourceRead(cell.module, cell.version, cell.platform, error))
                .flatMap:
                  case Nil =>
                    ZIO.fail(LocalPublishError.NoSources(cell.module, cell.version, cell.platform))
                  case files => ZIO.succeed(files.map(file => file.entry -> file.path))
      case _ =>
        ZIO.fail(LocalPublishError.Compile(GraphCompileError.NoRoot(cell.module)))

  private def modulesOf(graph: ProjectGraph): List[Module] =
    graph.projects.collect { case module: Module => module }

  private def cached(error: LocalPublishError | RemoteError | CacheError): LocalPublishError =
    error match
      case found: LocalPublishError => found
      case found: RemoteError       => LocalPublishError.Remote(found)
      case found: CacheError        => LocalPublishError.Runtime(CompileError.Stored(found))

  private def bytesOf(path: Path): ZIO[Any, LocalPublishError, Chunk[Byte]] =
    ZIO
      .attemptBlocking(Chunk.fromArray(Files.readAllBytes(path)))
      .mapError(_ => LocalPublishError.Read(path))

  /** A snapshot omits the javadoc jar. A local publish and a release pack the scaladoc site. */
  private def documented(
      repo: Path,
      graph: ProjectGraph,
      cell: histos.zinc.CompiledCell,
      revision: Revision,
      write: PublishWrite,
  ): ZIO[Resolve, LocalPublishError, Option[Chunk[Byte]]] =
    write match
      case PublishWrite.Snapshots                    => ZIO.succeed(None)
      case PublishWrite.Local | PublishWrite.Release =>
        for
          module <- ZIO.fromEither(moduleOf(graph, cell.module))
          paths  <- DocClasspath
            .of(repo, graph, module, cell)
            .mapError(LocalPublishError.Compile(_))
          packed <- JavadocJar
            .write(
              cell.classes,
              paths,
              cell.module.value,
              cell.version.value,
              scratch(repo, cell, revision, Scratch.DocSite),
              scratch(repo, cell, revision, Scratch.Javadoc),
            )
            .mapError(error => LocalPublishError.Jar(cell.module, cell.version, cell.platform, error))
          bytes <- bytesOf(packed.path)
        yield Some(bytes)

  private enum Scratch:
    case Classes, SourceJar, Javadoc, DocSite

    def file(platform: String, revision: Revision): String =
      val stem = revision match
        case Revision.Pointer(_) => s"$platform-pointer"
        case _                   => platform
      this match
        case Classes   => s"$stem.jar"
        case SourceJar => s"$stem-sources.jar"
        case Javadoc   => s"$stem-javadoc.jar"
        case DocSite   => s"$stem-doc"
  end Scratch

  /** The binary jar of a module the graph names as a plugin. The sources jar is left alone. */
  private def versionStamp(
      repo: Path,
      graph: ProjectGraph,
      cell: histos.zinc.CompiledCell,
      api: ApiDigest,
  ): ZIO[Any, LocalPublishError, List[(String, Path)]] =
    val named = graph.coordinates.exists:
      case PluginCoordinate.Module(name)    => name == cell.module
      case PluginCoordinate.Published(_, _) => false
    if !named then ZIO.succeed(Nil)
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
