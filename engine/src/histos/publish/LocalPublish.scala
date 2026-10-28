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
import histos.pack.{Assembly, AssemblyError, AssemblyFile, ClassJar, JarError, JavadocJar, PackageKey, Resources}
import histos.resolve.{ArtifactName, BuildResolvers, ClasspathScope, LockedTools, Resolve, ResolveCell, ResolveError}
import histos.version.{CiVersion, Decision, PublishAction, ShipName, VersionError, VersionTask}
import histos.select.SelectError
import histos.workspace.{IoFailure, IoFault}
import histos.zinc.{CompileError, GraphCompile, GraphCompileError, JvmCompile, ProductDigest, Sources}
import histos.{
  Dep,
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

import histos.schedule.{Histos, Outcome, Parallelism, StepFailure}
import histos.step.{Need, Phase, Step, StepId, StepInfo}

import zio.{Chunk, ZIO, ZLayer}

final case class PublishedModule(
    group: Organization,
    artifact: String,
    revision: String,
    jar: Option[Path],
    ship: ShipName,
    key: Sha256,
    unique: Option[String] = None,
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

enum LocalPublishError:
  case Version(error: VersionError)
  case Target(error: SelectError)
  case Compile(error: GraphCompileError)
  case Jar(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      error: JarError,
  )
  case SourceRead(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      error: CompileError,
  )
  case NoSources(module: ProjectName, version: ScalaVersion, platform: Platform)
  case Publish(error: PublishError)
  case Read(path: Path)
  case NoOrganization
  case NothingToPublish
  case Development(ship: ShipName, compile: CiVersion)
  case UnknownShip(ship: ShipName)
  case NotPublished(module: ProjectName)
  case SnapshotDependency(ship: ShipName, dependency: String)
  case Directory
  case TwoDirectories
  case UnpublishedDependency(module: ProjectName, dependency: ProjectName)
  case Facts(missing: ::[PomFact])
  case BadDependency(organization: String, name: String, revision: String)
  case Credentials
  case NoHome
  case NotAPath(raw: String)
  case Classpath(error: ResolveError)
  case Runtime(error: CompileError)
  case Pack(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      error: AssemblyError,
  )
  case MissingClasses(module: ProjectName, version: ScalaVersion, platform: Platform)

  def message: String = this match
    case Version(error)                        => error.message
    case Target(error)                         => error.message
    case Compile(error)                        => error.message
    case Jar(module, version, platform, error) =>
      s"'${module.value}' ${version.value} ${platform.token} failed to jar\n${error.message}"
    case SourceRead(module, version, platform, error) =>
      s"'${module.value}' ${version.value} ${platform.token} failed to read sources\n${error.message}"
    case NoSources(module, version, platform) =>
      s"'${module.value}' ${version.value} ${platform.token} has no sources to publish"
    case Publish(error) => error.message
    case Read(path)     => s"could not read $path"
    case NoOrganization =>
      "a publish needs an organization. Name it with Projects.organization(organization\"com.example\")"
    case NothingToPublish           => "nothing publishes"
    case Development(ship, compile) =>
      s"${ship.value} compiles ${compile.render}, which is not published"
    case UnknownShip(ship)                    => s"ship '${ship.value}' is not in the catalog"
    case NotPublished(module)                 => s"'${module.value}' is not in a ship"
    case SnapshotDependency(ship, dependency) =>
      s"${ship.value} depends on snapshot $dependency, so it is not published"
    case Directory                                 => "a directory belongs on publishLocal"
    case TwoDirectories                            => "publishLocal takes one directory"
    case UnpublishedDependency(module, dependency) =>
      s"'${module.value}' depends on '${dependency.value}', which is not published"
    case Facts(missing) =>
      s"a release needs ${missing.map(_.word).mkString(", ")}"
    case BadDependency(organization, name, revision) =>
      s"$organization:$name:$revision is not a publishable dependency"
    case Credentials                            => "SONATYPE_USERNAME and SONATYPE_PASSWORD are required"
    case NoHome                                 => "user.home is not set"
    case NotAPath(raw)                          => s"'$raw' is not a path"
    case Classpath(error)                       => error.message
    case Runtime(error)                         => error.message
    case Pack(module, version, platform, error) =>
      s"'${module.value}' ${version.value} ${platform.token} failed to assemble\n${error.message}"
    case MissingClasses(module, version, platform) =>
      s"'${module.value}' has no ${version.value} ${platform.token} classes"
end LocalPublishError

object LocalPublish:
  /** Compile the modules a decision releases and write them. A hold is reported and not written. `<ship>-ci` is written
    * only when `write` is local.
    */
  def run(
      repo: Path,
      graph: ProjectGraph,
      names: List[PublishName],
      jdk: JdkRuntime,
      env: Map[String, String],
      write: PublishWrite,
      report: histos.zinc.CompileFeed => zio.UIO[Unit] = _ => zio.ZIO.unit,
  ): ZIO[histos.resolve.Resolve & Publisher, LocalPublishError, PublishOutput] =
    for
      decision <- VersionTask
        .decide(graph, repo, env)
        .mapError(LocalPublishError.Version(_))
      chosen  <- ZIO.fromEither(scheme(decision, graph.catalog, write))
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
                    )
                  yield PublishOutput(
                    skipped(live),
                    modules.sortBy(line => (line.group.value, line.artifact, line.revision)),
                    chosen.held,
                    refused(live) ++ notes,
                  )
    yield written

  private type PublishEnv = Publisher & Resolve

  private def publishJobs(
      repo: Path,
      graph: ProjectGraph,
      group: Organization,
      jobs: List[Job],
      cells: List[histos.zinc.CompiledCell],
  ): ZIO[PublishEnv, LocalPublishError, List[PublishedModule]] =
    execute(combine(jobs.map: job =>
      val cell = job.cell
      val info = StepInfo(
        StepId.Cell(cell.module, cell.version, cell.platform, Phase.Publish),
        Chunk.empty,
        Set(Need.Io),
      )
      Step.local[PublishedModule](info)(write(repo, graph, group, job, jobs, cells))))

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
    case Ci, Release, Snapshot, Pointer, Manual

  private def scheme(
      decision: Decision,
      catalog: histos.version.Catalog,
      write: PublishWrite,
  ): Either[LocalPublishError, Scheme] =
    decision match
      case Decision.NoCatalog         => Left(LocalPublishError.NothingToPublish)
      case Decision.Develop(_, lines) =>
        write match
          case PublishWrite.Snapshots | PublishWrite.Release =>
            lines match
              case line :: _ =>
                Left(LocalPublishError.Development(line.ship, line.compile))
          case PublishWrite.Local =>
            lines.toList
              .foldLeft[Either[LocalPublishError, List[Planned]]](Right(Nil)): (acc, line) =>
                acc.flatMap: seen =>
                  members(catalog, line.ship).map(mods =>
                    seen :+ Planned(line.ship, Revision.Ci(line.compile), mods.toList)
                  )
              .flatMap(present)
              .map(plans => Scheme(plans, Nil))
      case Decision.Refused(refusals) =>
        Left(LocalPublishError.Version(VersionError.Refused(refusals)))
      case Decision.Manual(lines) =>
        lines.toList
          .foldLeft[Either[LocalPublishError, List[Planned]]](Right(Nil)): (acc, line) =>
            acc.flatMap: seen =>
              members(catalog, line.ship).map(mods =>
                seen :+ Planned(line.ship, Revision.Manual(line.version), mods.toList)
              )
          .flatMap(present)
          .map(plans => Scheme(plans, Nil))
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

  private def present(
      plans: List[Planned]
  ): Either[LocalPublishError, List[Planned]] =
    plans match
      case Nil  => Left(LocalPublishError.NothingToPublish)
      case some => Right(some)

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

  /** A release coordinate needs a license, a developer, and an scm. `-ci` and a snapshot do not. */
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
      case Revision.Release(_)      => true
      case Revision.Manual(version) => !PublishSelect.snapshotCoordinate(version.value)
      case _                        => false

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
      case Revision.Manual(_)   => RevisionKind.Manual

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

  private def write(
      repo: Path,
      graph: ProjectGraph,
      group: Organization,
      job: Job,
      jobs: List[Job],
      cells: List[histos.zinc.CompiledCell],
  ): ZIO[PublishEnv, LocalPublishError, PublishedModule] =
    val cell     = job.cell
    val artifact = artifactName(graph, cell.module, cell.version, cell.platform)
    publication(graph, cell.module) match
      case Publication.Assembly(main) =>
        stageAssembly(repo, graph, group, job, cells, main, artifact)
      case Publication.Library | Publication.SbtPlugin(_) | Publication.Application =>
        stageLibrary(repo, graph, group, job, jobs, artifact)

  private def stageLibrary(
      repo: Path,
      graph: ProjectGraph,
      group: Organization,
      job: Job,
      jobs: List[Job],
      artifact: String,
  ): ZIO[PublishEnv, LocalPublishError, PublishedModule] =
    val cell = job.cell
    for
      products <- productDigests(List(cell.classes), cell)
      name     <- artifactId(group, artifact, job.revision.render)
      deps     <- ZIO.fromEither(dependencies(graph, group, cell, job.revision, jobs))
      module   <- ZIO.fromEither(moduleOf(graph, cell.module))
      packed   <- sourcesOf(repo, graph, cell)
      extra    <- resourceEntries(repo, graph, List(module), cell.version, cell.platform)
      classes  <- ClassJar
        .write(cell.classes, extra, scratch(repo, cell, job.revision, Scratch.Classes))
        .mapError(error => LocalPublishError.Jar(cell.module, cell.version, cell.platform, error))
      sources <- ClassJar
        .writeNamed(packed, scratch(repo, cell, job.revision, Scratch.SourceJar))
        .mapError(error => LocalPublishError.Jar(cell.module, cell.version, cell.platform, error))
      javadoc <- JavadocJar
        .write(scratch(repo, cell, job.revision, Scratch.Javadoc))
        .mapError(error => LocalPublishError.Jar(cell.module, cell.version, cell.platform, error))
      bytes        <- bytesOf(classes.path)
      sourceBytes  <- bytesOf(sources.path)
      javadocBytes <- bytesOf(javadoc.path)
      published    <- publishOne(
        group,
        name,
        job.revision,
        bytes,
        deps,
        document(graph, job.ship),
        Some(sourceBytes),
        Some(javadocBytes),
        artifact,
        job.ship,
        PackageKey.of(products),
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
  ): ZIO[PublishEnv, LocalPublishError, PublishedModule] =
    val cell = job.cell
    for
      name   <- artifactId(group, artifact, job.revision.render)
      module <- ZIO.fromEither(moduleOf(graph, cell.module))
      ordered = module :: Module.chain(module, modulesOf(graph), cell.platform)
      dirs     <- ZIO.fromEither(classDirectories(ordered, cell.version, cell.platform, cells))
      products <- productDigests(dirs, cell)
      extra    <- resourceEntries(repo, graph, ordered, cell.version, cell.platform)
      jars     <- runtimeJars(repo, graph, ordered, cell.version, cell.platform)
      packed   <- sourcesOf(repo, graph, cell)
      classes  <- Assembly
        .write(
          dirs,
          jars,
          histos.pack.MainClass.trusted(main.value),
          scratch(repo, cell, job.revision, Scratch.Classes),
          extra.map((entry, file) => AssemblyFile(entry, file)),
        )
        .mapError(error => LocalPublishError.Pack(cell.module, cell.version, cell.platform, error))
      sources <- ClassJar
        .writeNamed(packed, scratch(repo, cell, job.revision, Scratch.SourceJar))
        .mapError(error => LocalPublishError.Jar(cell.module, cell.version, cell.platform, error))
      javadoc <- JavadocJar
        .write(scratch(repo, cell, job.revision, Scratch.Javadoc))
        .mapError(error => LocalPublishError.Jar(cell.module, cell.version, cell.platform, error))
      bytes        <- bytesOf(classes.path)
      sourceBytes  <- bytesOf(sources.path)
      javadocBytes <- bytesOf(javadoc.path)
      published    <- publishOne(
        group,
        name,
        job.revision,
        bytes,
        Nil,
        document(graph, job.ship),
        Some(sourceBytes),
        Some(javadocBytes),
        artifact,
        job.ship,
        PackageKey.of(products),
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
      ship: ShipName,
      key: Sha256,
  ): ZIO[Publisher, LocalPublishError, PublishedModule] =
    ZIO
      .serviceWithZIO[Publisher](
        _.publish(Artifact(group, name, revision, jar, deps, document, sources, javadoc))
      )
      .mapError(LocalPublishError.Publish(_))
      .map: published =>
        val (jarPath, unique) = placed(published)
        PublishedModule(group, artifact, revision.render, jarPath, ship, key, unique)

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

  private def classDirectories(
      ordered: List[Module],
      version: ScalaVersion,
      platform: Platform,
      cells: List[histos.zinc.CompiledCell],
  ): Either[LocalPublishError, List[Path]] =
    ordered.foldLeft[Either[LocalPublishError, List[Path]]](Right(Nil)): (acc, item) =>
      acc.flatMap: paths =>
        cells.find(cell => cell.module == item.name && cell.version == version && cell.platform == platform) match
          case Some(cell) => Right(paths :+ cell.classes)
          case None       => Left(LocalPublishError.MissingClasses(item.name, version, platform))

  private def resourceEntries(
      repo: Path,
      graph: ProjectGraph,
      modules: List[Module],
      version: ScalaVersion,
      platform: Platform,
  ): ZIO[Any, LocalPublishError, List[(String, Path)]] =
    ZIO.foldLeft(modules)(List.empty[(String, Path)]): (acc, module) =>
      GraphCompile
        .sourceBase(repo, module, modulesOf(graph))
        .mapError(LocalPublishError.Compile(_))
        .flatMap:
          case None =>
            ZIO.fail(LocalPublishError.Compile(GraphCompileError.NoRoot(module.name)))
          case Some(root) =>
            Resources
              .files(root)
              .mapError(error => LocalPublishError.Jar(module.name, version, platform, error))
              .map: found =>
                val taken = acc.map(_._1).toSet
                acc ++ found.filterNot((name, _) => taken.contains(name))

  private def runtimeJars(
      repo: Path,
      graph: ProjectGraph,
      ordered: List[Module],
      version: ScalaVersion,
      platform: Platform,
  ): ZIO[Resolve, LocalPublishError, List[Path]] =
    for
      lock    <- Resolve.readLock(repo).mapError(LocalPublishError.Classpath(_))
      runtime <- JvmCompile
        .runtime(version)
        .mapError(LocalPublishError.Runtime(_))
        .provideSome[Resolve](ZLayer.fromZIO(LockedTools.make(lock, graph)))
      external <- ZIO.foldLeft(ordered)(List.empty[Path]): (acc, item) =>
        val request = ResolveCell(
          item.name,
          Dep.select(item.dependencies, platform),
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

  private def placed(published: Published): (Option[Path], Option[String]) =
    published match
      case Published.Local(jar, _, _)    => (Some(jar), None)
      case Published.Snapshot(unique, _) => (None, Some(unique.value))

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
          .select(found.dependencies, cell.platform)
          .foldLeft[Either[LocalPublishError, List[MavenDependency]]](
            Right(Nil)
          ): (acc, dependency) =>
            acc.flatMap: seen =>
              externalCoordinate(
                dependency.organization,
                ArtifactName.of(dependency, cell.version, cell.platform),
                dependency.revision,
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
  ): Either[LocalPublishError, MavenDependency] =
    Organization.from(group) match
      case Left(_) =>
        Left(LocalPublishError.BadDependency(group, artifact, revision))
      case Right(parsed) => coordinate(parsed, artifact, revision)

  private def coordinate(
      group: Organization,
      artifact: String,
      revision: String,
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
    yield MavenDependency(group, name, version)

  private def artifactName(
      graph: ProjectGraph,
      name: ProjectName,
      version: ScalaVersion,
      platform: Platform,
  ): String =
    val base = graph.get(name) match
      case Some(module: Module) =>
        module.published.fold(module.name.value)(_.value)
      case _ => name.value
    publication(graph, name) match
      case Publication.Assembly(_)       => base
      case Publication.SbtPlugin(series) => ArtifactName.plugin(base, version, platform, series)
      case Publication.Library           => ArtifactName.published(base, version, platform)
      case Publication.Application       => ArtifactName.published(base, version, platform)

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

  private def bytesOf(path: Path): ZIO[Any, LocalPublishError, Chunk[Byte]] =
    ZIO
      .attemptBlocking(Chunk.fromArray(Files.readAllBytes(path)))
      .mapError(_ => LocalPublishError.Read(path))

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
