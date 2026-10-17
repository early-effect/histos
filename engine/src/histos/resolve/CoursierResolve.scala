package histos.resolve

import histos.jdk.Sha256

import java.nio.file.{Files, Path}

import zio.{Chunk, IO, ZIO}

/**
 * Coursier is linked as a library. `coursier.util.Task` runs on `Future`, and `eitherResult`
 * is that library's runner. This is the only place it executes. Callers see [[ResolveError]].
 */
private[resolve] final class CoursierResolve(cache: Path) extends Resolve:
  def resolve(cell: ResolveCell): IO[ResolveError, Classpath] =
    Resolve.prepare(cell) *> {
      if cell.dependencies.isEmpty then ZIO.succeed(Classpath(Nil, Nil))
      else solve(cell)
    }

  def fetch(artifact: LockedArtifact): IO[ResolveError, ResolvedArtifact] =
    for
      base   <- session
      single  = dependency(artifact.organization, artifact.name, artifact.revision).copy(transitive = false)
      result <- execute(base.withRepositories(Seq(toCoursier(artifact.repository))).withDependencies(Seq(single)))
      path   <- ZIO.fromEither(jar(artifact, result))
      sha    <- digest(path)
      out    <-
        if sha == artifact.sha256 then
          ZIO.succeed(ResolvedArtifact(artifact.organization, artifact.name, artifact.revision, path, sha, artifact.repository))
        else ZIO.fail(ResolveError.Checksum(path, artifact.sha256, sha))
    yield out

  private def solve(cell: ResolveCell): IO[ResolveError, Classpath] =
    val dependencies = cell.dependencies.map(dep =>
      dependency(dep.organization, ArtifactName.of(dep, cell.scalaVersion, cell.platform), dep.revision),
    )
    val repositories = Repositories.ordered(cell.repositories).map(toCoursier)
    for
      base   <- session
      result <- execute(base.withRepositories(repositories).withDependencies(dependencies))
      rows   <- ZIO.fromEither(classpathRows(cell, result))
      hashed <- ZIO.foreach(rows)(row => place(cell, row))
    yield Classpath(hashed, evictions(result.resolution))

  private def session: IO[ResolveError, coursier.Fetch[coursier.util.Task]] =
    ZIO.attempt {
      val files = coursier.cache.FileCache().withLocation(cache.toString).copy(allowCacheSubstitution = false)
      coursier.Fetch(files)(using coursier.util.Task.sync).noMirrors.withGradleModuleSupport(false)
    }.mapError(_ => ResolveError.Repository(RepositoryIo.Fetch))

  private def execute(fetch: coursier.Fetch[coursier.util.Task]): IO[ResolveError, coursier.Fetch.Result] =
    ZIO.attemptBlockingInterrupt(fetch.eitherResult()).mapError(_ => ResolveError.Repository(RepositoryIo.Fetch)).flatMap:
      case Left(error)   => ZIO.fail(lift(error))
      case Right(result) => ZIO.succeed(result)

  private def place(cell: ResolveCell, row: Row): IO[ResolveError, ResolvedArtifact] =
    Repository.recorded(row.url, Repositories.ordered(cell.repositories)) match
      case None => ZIO.fail(ResolveError.Repository(RepositoryIo.Metadata))
      case Some(repository) =>
        digest(row.path).map(sha => ResolvedArtifact(row.organization, row.name, row.revision, row.path, sha, repository))

  private def classpathRows(cell: ResolveCell, result: coursier.Fetch.Result): Either[ResolveError, List[Row]] =
    rows(result).flatMap: found =>
      if found.nonEmpty then Right(found)
      else
        cell.dependencies match
          case dependency :: _ =>
            Left(ResolveError.NotFound(
              dependency.organization,
              ArtifactName.of(dependency, cell.scalaVersion, cell.platform),
              dependency.revision,
            ))
          case Nil => Right(Nil)

  private def jar(artifact: LockedArtifact, result: coursier.Fetch.Result): Either[ResolveError, Path] =
    rows(result).flatMap: found =>
      found.find(row => row.organization == artifact.organization && row.name == artifact.name && row.revision == artifact.revision) match
        case Some(row) => Right(row.path)
        case None      => Left(ResolveError.NotFound(artifact.organization, artifact.name, artifact.revision))

  private def rows(result: coursier.Fetch.Result): Either[ResolveError, List[Row]] =
    result.detailedArtifacts0.toList.foldLeft[Either[ResolveError, List[Row]]](Right(Nil)):
      case (Left(error), _) => Left(error)
      case (Right(seen), (dependency, _, artifact, file)) =>
        val path = file.toPath
        val fileName = Option(path.getFileName).map(_.toString).getOrElse("")
        if !fileName.endsWith(".jar") then Right(seen)
        else
          result.resolution.reconciledVersions.get(dependency.module) match
            case None => Left(ResolveError.Repository(RepositoryIo.Metadata))
            case Some(version) =>
              val row = Row(
                dependency.module.organization.value,
                dependency.module.name.value,
                version.asString,
                path,
                artifact.url,
              )
              if seen.exists(prior => prior.organization == row.organization && prior.name == row.name) then Right(seen)
              else Right(seen :+ row)

  /** Project metadata is a map, so this order is sorted. The classpath order is not. */
  private def evictions(resolution: coursier.core.Resolution): List[Eviction] =
    val selected = resolution.reconciledVersions
    def one(dependency: coursier.core.Dependency): Option[Eviction] =
      selected.get(dependency.module).flatMap: chosen =>
        val requested = dependency.versionConstraint.asString
        val got       = chosen.asString
        if requested == got then None
        else Some(Eviction(dependency.module.organization.value, dependency.module.name.value, requested, got))
    val roots = resolution.rootDependencies.toList.flatMap(one)
    val transitive = resolution.projectCache0.toList.flatMap { case (_, (_, project)) =>
      project.dependencies0.toList.flatMap((_, dependency) => one(dependency))
    }
    distinct(roots ++ transitive).sortBy(item => (item.organization, item.name, item.requested, item.selected))

  private def distinct(items: List[Eviction]): List[Eviction] =
    items.foldLeft(List.empty[Eviction])((seen, item) => if seen.contains(item) then seen else seen :+ item)

  private def dependency(organization: String, name: String, revision: String): coursier.core.Dependency =
    coursier.Dependency(
      coursier.Module(coursier.Organization(organization), coursier.ModuleName(name)),
      coursier.version.VersionConstraint(revision),
    )

  private def toCoursier(repository: Repository): coursier.core.Repository =
    repository match
      case Repository.Central    => coursier.Repositories.central
      case Repository.IvyLocal   => coursier.LocalRepositories.ivy2Local
      case Repository.Maven(url) => coursier.MavenRepository(url.value)

  private def digest(path: Path): IO[ResolveError, Sha256] =
    ZIO.attemptBlocking(Sha256.of(Chunk.fromArray(Files.readAllBytes(path))))
      .mapError(_ => ResolveError.Repository(RepositoryIo.Read))

  private def lift(error: coursier.error.CoursierError): ResolveError =
    error match
      case resolution: coursier.error.ResolutionError =>
        resolution.errors.headOption match
          case Some(missing: coursier.error.ResolutionError.CantDownloadModule) =>
            ResolveError.NotFound(
              missing.module.organization.value,
              missing.module.name.value,
              missing.versionConstraint.asString,
            )
          case _ => ResolveError.Repository(RepositoryIo.Metadata)
      case _ => ResolveError.Repository(RepositoryIo.Fetch)

  private final case class Row(organization: String, name: String, revision: String, path: Path, url: String)

end CoursierResolve
