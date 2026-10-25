package histos.publish

import histos.jdk.JdkRuntime
import histos.central.{
  Artifact,
  ArtifactId,
  DependencyVersion,
  MavenDependency,
  PublishError,
  Published,
  Publisher,
  Revision,
}
import histos.native.NativeOrigin
import histos.pack.{ClassJar, JarError}
import histos.resolve.ArtifactName
import histos.version.{CiVersion, Decision, PublishAction, ShipName, VersionError, VersionTask}
import histos.select.Selection
import histos.workflow.CiError
import histos.zinc.{GraphCompile, GraphCompileError}
import histos.{Dep, Module, Organization, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}

import histos.schedule.{Histos, Outcome, Parallelism, StepFailure}
import histos.step.{Need, Phase, Step, StepId, StepInfo}

import zio.{Chunk, ZIO}

final case class PublishedModule(
    group: Organization,
    artifact: String,
    revision: String,
    jar: Option[Path],
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
)

enum LocalPublishError:
  case Version(error: VersionError)
  case Target(error: CiError)
  case Compile(error: GraphCompileError)
  case Jar(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      error: JarError,
  )
  case Publish(error: PublishError)
  case Read(path: Path)
  case NoOrganization
  case NothingToPublish
  case Development(ship: ShipName, compile: CiVersion)
  case UnknownShip(ship: ShipName)
  case NotPublished(module: ProjectName)
  case UnpublishedDependency(module: ProjectName, dependency: ProjectName)
  case BadDependency(organization: String, name: String, revision: String)
  case Credentials
  case NotAPath(raw: String)
  case NoAssembly(path: Path)
  case VersionMismatch(expected: String, found: String)
  case LauncherRevision

  def message: String = this match
    case Version(error)                        => error.message
    case Target(error)                         => error.message
    case Compile(error)                        => error.message
    case Jar(module, version, platform, error) =>
      s"'${module.value}' ${version.value} ${platform.token} failed to jar\n${error.message}"
    case Publish(error) => error.message
    case Read(path)     => s"could not read $path"
    case NoOrganization =>
      "a publish needs an organization. Name it with Projects.organization(organization\"com.example\")"
    case NothingToPublish           => "nothing publishes"
    case Development(ship, compile) =>
      s"${ship.value} compiles ${compile.render}, which is not published"
    case UnknownShip(ship)                         => s"ship '${ship.value}' is not in the catalog"
    case NotPublished(module)                      => s"'${module.value}' is not in a ship"
    case UnpublishedDependency(module, dependency) =>
      s"'${module.value}' depends on '${dependency.value}', which is not published"
    case BadDependency(organization, name, revision) =>
      s"$organization:$name:$revision is not a publishable dependency"
    case Credentials                      => "SONATYPE_USERNAME and SONATYPE_PASSWORD are required"
    case NotAPath(raw)                    => s"'$raw' is not a path"
    case NoAssembly(path)                 => s"no assembly at $path"
    case VersionMismatch(expected, found) =>
      s"the ship is $expected and this publish wrote $found"
    case LauncherRevision => "the launcher publishes one manual version"
end LocalPublishError

object LocalPublish:
  /** Compile the modules a decision releases and write them into a local Maven directory. A hold is reported and not
    * written. `<ship>-ci` is not written.
    */
  def run(
      repo: Path,
      graph: ProjectGraph,
      names: List[String],
      jdk: JdkRuntime,
      env: Map[String, String],
      report: histos.zinc.CompileFeed => zio.UIO[Unit] = _ => zio.ZIO.unit,
  ): ZIO[histos.resolve.Resolve & Publisher, LocalPublishError, PublishOutput] =
    for
      decision <- VersionTask
        .decide(graph, repo, env)
        .mapError(LocalPublishError.Version(_))
      chosen  <- ZIO.fromEither(scheme(decision, graph.catalog))
      written <- chosen.plans match
        case Nil =>
          chosen.held match
            case Nil => ZIO.fail(LocalPublishError.NothingToPublish)
            case _   => ZIO.succeed(PublishOutput(Nil, Nil, chosen.held))
        case plans =>
          for
            picked <- ZIO.fromEither(choose(graph, names, plans))
            _      <- ZIO.fromEither(publishedDependencies(graph, picked.modules))
            group  <- ZIO.fromEither(organization(graph))
            cells  <- GraphCompile
              .run(
                repo,
                graph,
                picked.modules.toList.map(_.value),
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
              jobs(plans, picked.modules, cells),
            )
          yield PublishOutput(
            picked.skipped.map(name => LocalPublishError.NotPublished(name).message),
            modules.sortBy(line => (line.group.value, line.artifact, line.revision)),
            chosen.held,
          )
    yield written

  private def publishJobs(
      repo: Path,
      graph: ProjectGraph,
      group: Organization,
      jobs: List[Job],
  ): ZIO[Publisher, LocalPublishError, List[PublishedModule]] =
    execute(combine(jobs.map: job =>
      val cell = job.cell
      val info = StepInfo(
        StepId.Cell(cell.module, cell.version, cell.platform, Phase.Publish),
        Chunk.empty,
        Set(Need.Io),
      )
      Step.local[PublishedModule](info)(write(repo, graph, group, job))))

  private def combine(
      steps: List[Step[Publisher, LocalPublishError, PublishedModule]]
  ): Step[Publisher, LocalPublishError, List[PublishedModule]] =
    steps match
      case Nil =>
        Step.local[List[PublishedModule]](
          StepInfo(StepId.Work("publish"), Chunk.empty, Set(Need.Io))
        )(ZIO.succeed(Nil))
      case head :: tail =>
        tail
          .foldLeft[Step[Publisher, LocalPublishError, List[PublishedModule]]](
            head.map(List(_))
          ): (acc, step) =>
            acc.zip(step).map((all, next) => all :+ next)

  private def execute[A](
      goal: Step[Publisher, LocalPublishError, A]
  ): ZIO[Publisher, LocalPublishError, A] =
    ZIO.scoped:
      ZIO
        .serviceWithZIO[Histos](_.start(goal))
        .flatMap(_.outcome)
        .provideSome[Publisher & zio.Scope](Parallelism.live >>> Histos.live)
        .flatMap:
          case Outcome.Done(value)      => ZIO.succeed(value)
          case Outcome.Failed(failures) =>
            failures.head match
              case StepFailure.Failed(_, cause) => ZIO.failCause(cause)
              case StepFailure.Skipped(_, _)    =>
                ZIO.dieMessage("publish step was skipped")

  private final case class Planned(
      revision: Revision,
      members: List[ProjectName],
  )

  private final case class Scheme(plans: List[Planned], held: List[String])

  private final case class Job(
      revision: Revision,
      cell: histos.zinc.CompiledCell,
  )

  private def scheme(
      decision: Decision,
      catalog: histos.version.Catalog,
  ): Either[LocalPublishError, Scheme] =
    decision match
      case Decision.NoCatalog             => Left(LocalPublishError.NothingToPublish)
      case Decision.Develop(_, line :: _) =>
        Left(LocalPublishError.Development(line.ship, line.compile))
      case Decision.Refused(refusals) =>
        Left(LocalPublishError.Version(VersionError.Refused(refusals)))
      case Decision.Manual(lines) =>
        lines.toList
          .foldLeft[Either[LocalPublishError, List[Planned]]](Right(Nil)): (acc, line) =>
            acc.flatMap: seen =>
              members(catalog, line.ship).map(mods => seen :+ Planned(Revision.Manual(line.version), mods.toList))
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
                case PublishAction.Release(version, _) =>
                  members(catalog, line.ship).map(mods =>
                    (
                      seen :+ Planned(Revision.Release(version), mods.toList),
                      held,
                    )
                  )
                case PublishAction.Snapshot(_, stored, pointer) =>
                  members(catalog, line.ship).map: mods =>
                    val names      = mods.toList
                    val storedPlan = Planned(Revision.Snapshot(stored), names)
                    val extra      = pointer.toList
                      .map(value => Planned(Revision.Pointer(value), names))
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

  private final case class Chosen(
      modules: Set[ProjectName],
      skipped: List[ProjectName],
  )

  private def choose(
      graph: ProjectGraph,
      names: List[String],
      plans: List[Planned],
  ): Either[LocalPublishError, Chosen] =
    val shipMembers = plans.flatMap(_.members).toSet
    if names.isEmpty then Right(Chosen(shipMembers, Nil))
    else
      Selection.select(graph, names) match
        case Left(error)     => Left(LocalPublishError.Target(error))
        case Right(selected) =>
          val direct = selected.modules.filter(module => !selected.expanded.contains(module.name))
          direct.find(module => !shipMembers.contains(module.name)) match
            case Some(module) =>
              Left(LocalPublishError.NotPublished(module.name))
            case None =>
              val (keep, skip) = selected.modules.partition(module => shipMembers.contains(module.name))
              if keep.isEmpty then Left(LocalPublishError.NothingToPublish)
              else
                Right(
                  Chosen(
                    closure(graph, keep.map(_.name).toSet, shipMembers),
                    skip.map(_.name),
                  )
                )

  private def closure(
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

  /** A published module's `dependsOn` is published in the same run. An external library is a pom dependency.
    */
  private def publishedDependencies(
      graph: ProjectGraph,
      chosen: Set[ProjectName],
  ): Either[LocalPublishError, Unit] =
    val modules = chosen.toList.flatMap: name =>
      graph.get(name) match
        case Some(module: Module) => List(module)
        case _                    => Nil
    modules.foldLeft[Either[LocalPublishError, Unit]](Right(())): (acc, module) =>
      acc.flatMap: _ =>
        upstream(module).find(dependency => !chosen.contains(dependency)) match
          case Some(dependency) =>
            Left(
              LocalPublishError.UnpublishedDependency(module.name, dependency)
            )
          case None => Right(())

  private def organization(
      graph: ProjectGraph
  ): Either[LocalPublishError, Organization] =
    graph.organization match
      case Some(group) => Right(group)
      case None        => Left(LocalPublishError.NoOrganization)

  private def jobs(
      plans: List[Planned],
      chosen: Set[ProjectName],
      cells: List[histos.zinc.CompiledCell],
  ): List[Job] =
    plans.flatMap: plan =>
      val names = plan.members.filter(chosen.contains).toSet
      cells
        .filter(cell => names.contains(cell.module))
        .map(cell => Job(plan.revision, cell))

  private def write(
      repo: Path,
      graph: ProjectGraph,
      group: Organization,
      job: Job,
  ): ZIO[Publisher, LocalPublishError, PublishedModule] =
    val cell     = job.cell
    val artifact = artifactName(graph, cell.module, cell.version, cell.platform)
    for
      name <- ZIO
        .fromEither(ArtifactId.from(artifact))
        .mapError(_ =>
          LocalPublishError
            .BadDependency(group.value, artifact, job.revision.render)
        )
      deps <- ZIO.fromEither(
        dependencies(graph, group, cell, job.revision)
      )
      jar <- ClassJar
        .write(cell.classes, jarFile(repo, cell, job.revision))
        .mapError(error => LocalPublishError.Jar(cell.module, cell.version, cell.platform, error))
      bytes <- ZIO
        .attemptBlocking(Chunk.fromArray(Files.readAllBytes(jar.path)))
        .mapError(_ => LocalPublishError.Read(jar.path))
      published <- ZIO
        .serviceWithZIO[Publisher](
          _.publish(Artifact(group, name, job.revision, bytes, deps))
        )
        .mapError(LocalPublishError.Publish(_))
      (jarPath, unique) = placed(published)
    yield PublishedModule(group, artifact, job.revision.render, jarPath, unique)

  private def placed(published: Published): (Option[Path], Option[String]) =
    published match
      case Published.Local(jar, _, _)    => (Some(jar), None)
      case Published.Snapshot(unique, _) => (None, Some(unique.value))

  private def dependencies(
      graph: ProjectGraph,
      group: Organization,
      cell: histos.zinc.CompiledCell,
      revision: Revision,
  ): Either[LocalPublishError, List[MavenDependency]] =
    graph.get(cell.module) match
      case Some(found: Module) =>
        val internal = found
          .upstream(cell.platform)
          .foldLeft[Either[LocalPublishError, List[MavenDependency]]](
            Right(Nil)
          ): (acc, dependency) =>
            acc.flatMap: seen =>
              coordinate(
                group,
                artifactName(graph, dependency, cell.version, cell.platform),
                revision.render,
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
    ArtifactName.published(base, version, platform)

  /** Upstream modules on every platform the module builds. */
  private def upstream(module: Module): List[ProjectName] =
    module.platforms.all.flatMap(module.upstream).distinct

  private def jarFile(
      repo: Path,
      cell: histos.zinc.CompiledCell,
      revision: Revision,
  ): Path =
    val kind = revision match
      case Revision.Pointer(_) => s"${cell.platform.token}-pointer.jar"
      case _                   => s"${cell.platform.token}.jar"
    repo
      .resolve(".histos")
      .resolve("jars")
      .resolve(cell.module.value)
      .resolve(cell.version.value)
      .resolve(kind)
end LocalPublish
