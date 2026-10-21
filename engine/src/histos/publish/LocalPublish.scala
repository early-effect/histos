package histos.publish

import histos.central.{Artifact, ArtifactId, DependencyVersion, MavenDependency, PublishError, Publisher, Revision}
import histos.pack.{ClassJar, JarError}
import histos.resolve.ArtifactName
import histos.version.{CiVersion, Decision, PublishAction, ShipName, VersionError, VersionTask}
import histos.select.Selection
import histos.workflow.CiError
import histos.zinc.{GraphCompile, GraphCompileError, ResolvedJdk}
import histos.{Dep, Module, Organization, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}

import zio.{Chunk, ZIO}

final case class PublishedModule(group: Organization, artifact: String, revision: String, jar: Path)

/** Skipped aggregate members are named. The modules are what was written. */
final case class PublishOutput(skipped: List[String], modules: List[PublishedModule])

enum LocalPublishError:
  case Version(error: VersionError)
  case Target(error: CiError)
  case Compile(error: GraphCompileError)
  case Jar(module: ProjectName, version: ScalaVersion, error: JarError)
  case Publish(error: PublishError)
  case Read(path: Path)
  case NoOrganization
  case NothingToPublish
  case Development(ship: ShipName, compile: CiVersion)
  case UnknownShip(ship: ShipName)
  case NotPublished(module: ProjectName)
  case UnpublishedDependency(module: ProjectName, dependency: ProjectName)
  case BadDependency(organization: String, name: String, revision: String)

  def message: String = this match
    case Version(error)  => error.message
    case Target(error)   => error.message
    case Compile(error)  => error.message
    case Jar(module, version, error) =>
      s"'${module.value}' ${version.value} failed to jar\n${error.message}"
    case Publish(error)  => error.message
    case Read(path)      => s"could not read $path"
    case NoOrganization  =>
      "a publish needs an organization. Name it with Projects.organization(organization\"com.example\")"
    case NothingToPublish => "nothing publishes"
    case Development(ship, compile) =>
      s"${ship.value} compiles ${compile.render}, which is not published"
    case UnknownShip(ship) => s"ship '${ship.value}' is not in the catalog"
    case NotPublished(module) => s"'${module.value}' is not in a ship"
    case UnpublishedDependency(module, dependency) =>
      s"'${module.value}' depends on '${dependency.value}', which is not published"
    case BadDependency(organization, name, revision) =>
      s"$organization:$name:$revision is not a publishable dependency"

object LocalPublish:
  /** Compile the published modules and write them into a local Maven directory. `<ship>-ci` is not written. */
  def run(
      repo: Path,
      graph: ProjectGraph,
      names: List[String],
      jdk: ResolvedJdk,
      env: Map[String, String],
  ): ZIO[histos.resolve.Resolve & Publisher, LocalPublishError, PublishOutput] =
    for
      decision <- VersionTask.decide(graph, repo, env).mapError(LocalPublishError.Version(_))
      plans    <- ZIO.fromEither(planned(decision, graph.catalog))
      picked   <- ZIO.fromEither(choose(graph, names, plans))
      _        <- ZIO.fromEither(publishedDependencies(graph, picked.modules))
      group    <- ZIO.fromEither(organization(graph))
      cells    <- GraphCompile.run(repo, graph, picked.modules.toList.map(_.value), jdk).mapError(LocalPublishError.Compile(_))
      written  <- ZIO.foreachPar(jobs(plans, picked.modules, cells))(job => write(repo, graph, group, job))
    yield PublishOutput(
      picked.skipped.map(name => LocalPublishError.NotPublished(name).message),
      written.sortBy(line => (line.group.value, line.artifact, line.revision)),
    )

  private final case class Planned(revision: Revision, members: List[ProjectName])

  private final case class Job(revision: Revision, cell: histos.zinc.CompiledCell)

  private def planned(decision: Decision, catalog: histos.version.Catalog): Either[LocalPublishError, List[Planned]] =
    decision match
      case Decision.NoCatalog              => Left(LocalPublishError.NothingToPublish)
      case Decision.Develop(_, line :: _)  => Left(LocalPublishError.Development(line.ship, line.compile))
      case Decision.Refused(refusals)      => Left(LocalPublishError.Version(VersionError.Refused(refusals)))
      case Decision.Manual(lines)          =>
        lines.toList.foldLeft[Either[LocalPublishError, List[Planned]]](Right(Nil)): (acc, line) =>
          acc.flatMap: seen =>
            members(catalog, line.ship).map(mods => seen :+ Planned(Revision.Manual(line.version), mods.toList))
        .flatMap(present)
      case Decision.Publish(_, lines) =>
        lines.toList.foldLeft[Either[LocalPublishError, List[Planned]]](Right(Nil)): (acc, line) =>
          acc.flatMap: seen =>
            line.action match
              case PublishAction.Hold(_, _) => Right(seen)
              case PublishAction.Release(version, _) =>
                members(catalog, line.ship).map(mods => seen :+ Planned(Revision.Release(version), mods.toList))
              case PublishAction.Snapshot(_, stored, pointer) =>
                members(catalog, line.ship).map: mods =>
                  val names = mods.toList
                  val storedPlan = Planned(Revision.Snapshot(stored), names)
                  val extra = pointer.toList.map(value => Planned(Revision.Pointer(value), names))
                  seen ++ (storedPlan :: extra)
        .flatMap(present)

  private def present(plans: List[Planned]): Either[LocalPublishError, List[Planned]] =
    plans match
      case Nil  => Left(LocalPublishError.NothingToPublish)
      case some => Right(some)

  private def members(catalog: histos.version.Catalog, ship: ShipName): Either[LocalPublishError, ::[ProjectName]] =
    catalog.get(ship) match
      case Some(row) => Right(row.members)
      case None      => Left(LocalPublishError.UnknownShip(ship))

  private final case class Chosen(modules: Set[ProjectName], skipped: List[ProjectName])

  private def choose(graph: ProjectGraph, names: List[String], plans: List[Planned]): Either[LocalPublishError, Chosen] =
    val shipMembers = plans.flatMap(_.members).toSet
    if names.isEmpty then Right(Chosen(shipMembers, Nil))
    else
      Selection.select(graph, names) match
        case Left(error) => Left(LocalPublishError.Target(error))
        case Right(selected) =>
          val direct = selected.modules.filter(module => !selected.expanded.contains(module.name))
          direct.find(module => !shipMembers.contains(module.name)) match
            case Some(module) => Left(LocalPublishError.NotPublished(module.name))
            case None =>
              val (keep, skip) = selected.modules.partition(module => shipMembers.contains(module.name))
              if keep.isEmpty then Left(LocalPublishError.NothingToPublish)
              else Right(Chosen(closure(graph, keep.map(_.name).toSet, shipMembers), skip.map(_.name)))

  private def closure(graph: ProjectGraph, asked: Set[ProjectName], members: Set[ProjectName]): Set[ProjectName] =
    def step(seen: Set[ProjectName], more: Set[ProjectName]): Set[ProjectName] =
      if more.isEmpty then seen
      else
        val next = more.flatMap: name =>
          graph.get(name) match
            case Some(module: Module) => module.upstream(Platform.Jvm).filter(members.contains).toSet
            case _                    => Set.empty
        step(seen ++ more, next -- seen)
    step(Set.empty, asked)

  /** A published module's `dependsOn` is published in the same run. An external library is a pom dependency. */
  private def publishedDependencies(graph: ProjectGraph, chosen: Set[ProjectName]): Either[LocalPublishError, Unit] =
    val modules = chosen.toList.flatMap: name =>
      graph.get(name) match
        case Some(module: Module) => List(module)
        case _                    => Nil
    modules.foldLeft[Either[LocalPublishError, Unit]](Right(())): (acc, module) =>
      acc.flatMap: _ =>
        module.upstream(Platform.Jvm).find(dependency => !chosen.contains(dependency)) match
          case Some(dependency) => Left(LocalPublishError.UnpublishedDependency(module.name, dependency))
          case None             => Right(())

  private def organization(graph: ProjectGraph): Either[LocalPublishError, Organization] =
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
      cells.filter(cell => names.contains(cell.module)).map(cell => Job(plan.revision, cell))

  private def write(
      repo: Path,
      graph: ProjectGraph,
      group: Organization,
      job: Job,
  ): ZIO[Publisher, LocalPublishError, PublishedModule] =
    val cell = job.cell
    val artifact = ArtifactName.published(cell.module, cell.version, Platform.Jvm)
    for
      name <- ZIO.fromEither(ArtifactId.from(artifact)).mapError(_ => LocalPublishError.BadDependency(group.value, artifact, job.revision.render))
      deps <- ZIO.fromEither(dependencies(graph, group, cell.module, cell.version, job.revision))
      jar  <- ClassJar.write(cell.classes, jarFile(repo, cell, job.revision)).mapError(error => LocalPublishError.Jar(cell.module, cell.version, error))
      bytes <- ZIO.attemptBlocking(Chunk.fromArray(Files.readAllBytes(jar.path))).mapError(_ => LocalPublishError.Read(jar.path))
      published <- ZIO.serviceWithZIO[Publisher](_.publish(Artifact(group, name, job.revision, bytes, deps))).mapError(LocalPublishError.Publish(_))
    yield PublishedModule(group, artifact, job.revision.render, published.jar)

  private def dependencies(
      graph: ProjectGraph,
      group: Organization,
      module: ProjectName,
      version: ScalaVersion,
      revision: Revision,
  ): Either[LocalPublishError, List[MavenDependency]] =
    graph.get(module) match
      case Some(found: Module) =>
        val internal = found.upstream(Platform.Jvm).foldLeft[Either[LocalPublishError, List[MavenDependency]]](Right(Nil)): (acc, dependency) =>
          acc.flatMap: seen =>
            coordinate(group, ArtifactName.published(dependency, version, Platform.Jvm), revision.render).map(seen :+ _)
        val external = Dep.select(found.dependencies, Platform.Jvm).foldLeft[Either[LocalPublishError, List[MavenDependency]]](Right(Nil)): (acc, dependency) =>
          acc.flatMap: seen =>
            externalCoordinate(dependency.organization, ArtifactName.of(dependency, version, Platform.Jvm), dependency.revision).map(seen :+ _)
        for
          inside <- internal
          outside <- external
        yield inside ++ outside
      case _ => Right(Nil)

  private def externalCoordinate(group: String, artifact: String, revision: String): Either[LocalPublishError, MavenDependency] =
    Organization.from(group) match
      case Left(_)          => Left(LocalPublishError.BadDependency(group, artifact, revision))
      case Right(parsed)    => coordinate(parsed, artifact, revision)

  private def coordinate(group: Organization, artifact: String, revision: String): Either[LocalPublishError, MavenDependency] =
    for
      name    <- ArtifactId.from(artifact).left.map(_ => LocalPublishError.BadDependency(group.value, artifact, revision))
      version <- DependencyVersion.from(revision).left.map(_ => LocalPublishError.BadDependency(group.value, artifact, revision))
    yield MavenDependency(group, name, version)

  private def jarFile(repo: Path, cell: histos.zinc.CompiledCell, revision: Revision): Path =
    val kind = revision match
      case Revision.Pointer(_) => "pointer.jar"
      case _                   => "jvm.jar"
    repo.resolve(".histos").resolve("jars").resolve(cell.module.value).resolve(cell.version.value).resolve(kind)
