package histos.resolve

import histos.jdk.{Sha256, Sha256Digest}
import histos.workspace.{IoFailure, IoFault}
import histos.{Dep, Platform, ScalaVersion}

import java.nio.file.{Files, Path}

import zio.{Chunk, IO, Ref, ULayer, ZIO, ZLayer}

/** Coursier is linked as a library. `coursier.util.Task` runs on `Future`, and `eitherResult` is that library's runner.
  * Callers see [[ResolveError]]. The Scala 2.13 toolchain is fetched separately and is not a user dependency.
  */
private[resolve] enum CacheMode:
  case Revalidate
  case Retain
  case Offline

private[resolve] final case class FetchKey(
    organization: String,
    name: String,
    revision: String,
    classifier: String,
    sha256: String,
    repository: String,
)

private[resolve] final class CoursierResolve(
    cache: Path,
    mode: CacheMode,
    fetched: Ref[Map[FetchKey, ResolvedArtifact]],
) extends Resolve:
  def resolve(cell: ResolveCell): IO[ResolveError, Classpath] =
    Resolve.prepare(cell) *> {
      if cell.dependencies.isEmpty then ZIO.succeed(Classpath(Nil, Nil))
      else
        solve(
          cell.dependencies,
          cell.scalaVersion,
          cell.platform,
          cell.repositories,
        )
    }

  def resolvePlugin(
      dependency: Dep,
      repositories: Repositories,
  ): IO[ResolveError, Classpath] =
    if Resolve.ranged(dependency) then ZIO.fail(ResolveError.Range(dependency))
    else Resolve.pluginScala.flatMap(version => solve(List(dependency), version, Platform.Jvm, repositories))

  override def resolveTool(request: ToolRequest): IO[ResolveError, Classpath] =
    val dependencies = request.roots.map(root =>
      dependency(root.organization, root.name, root.revision)
        .copy(transitive = root.transitivity == Transitivity.Closure)
    )
    val origins      = request.roots.flatMap(_.origin).distinct
    val repositories = Repositories.ordered(request.repositories)
    val ordered      = (origins ++ repositories).distinct
    val policy       = Repositories(
      ordered.headOption.getOrElse(request.repositories.first),
      ordered.drop(1),
    )
    for
      _ <- ZIO.foreachDiscard(request.roots)(root =>
        ZIO
          .fromEither(histos.ToolRevision.from(root.revision))
          .mapError(_ => ResolveError.Range(root.dependency))
      )
      _ <- ZIO.foreachDiscard(request.roots): root =>
        root.origin match
          case None         => ZIO.unit
          case Some(origin) =>
            for
              base   <- session()
              result <- execute(
                base
                  .withRepositories(Seq(toCoursier(origin)))
                  .withDependencies(
                    Seq(
                      dependency(root.organization, root.name, root.revision)
                        .copy(transitive = false)
                    )
                  )
              )
              _ <- ZIO.fromEither(
                located(root.organization, root.name, root.revision, result)
              )
            yield ()
      base   <- session()
      result <- execute(
        base
          .withRepositories(ordered.map(toCoursier))
          .withDependencies(dependencies)
      )
      found     <- ZIO.fromEither(rows(result))
      artifacts <- ZIO.foreach(found)(row => place(policy, row))
    yield Classpath(artifacts, evictions(result.resolution))

  def fetch(artifact: LockedArtifact): IO[ResolveError, ResolvedArtifact] =
    val key = FetchKey(
      artifact.organization,
      artifact.name,
      artifact.revision,
      artifact.classifier.fold("")(_.value),
      artifact.sha256.value,
      Repository.token(artifact.repository),
    )
    fetched.get.flatMap { have =>
      have.get(key) match
        case Some(found) => ZIO.succeed(found)
        case None        => fetchNew(artifact).tap(found => fetched.update(_.updated(key, found)))
    }

  private def fetchNew(artifact: LockedArtifact): IO[ResolveError, ResolvedArtifact] =
    val policy =
      if mode == CacheMode.Offline then CacheUse.Offline else CacheUse.Locked
    for
      base <- session(policy)
      single = dependency(
        artifact.organization,
        artifact.name,
        artifact.revision,
      ).copy(transitive = false)
        .withAttributes(
          coursier.core.Attributes(
            coursier.core.Type.jar,
            coursier.core.Classifier(artifact.classifier.fold("")(_.value)),
          )
        )
      result <- execute(
        base
          .withRepositories(Seq(toCoursier(artifact.repository)))
          .withDependencies(Seq(single))
      )
      path <- ZIO.fromEither(
        located(
          artifact.organization,
          artifact.name,
          artifact.revision,
          result,
          artifact.classifier,
        )
      )
      sha <- digest(path)
      out <-
        if sha == artifact.sha256 then
          ZIO.succeed(
            ResolvedArtifact(
              artifact.organization,
              artifact.name,
              artifact.revision,
              path,
              sha,
              artifact.repository,
              artifact.classifier,
            )
          )
        else ZIO.fail(ResolveError.Checksum(path, artifact.sha256, sha))
    yield out

  def sources(
      organization: String,
      name: String,
      revision: String,
      repository: Repository,
  ): IO[ResolveError, ResolvedArtifact] =
    val single = dependency(organization, name, revision)
      .copy(transitive = false)
      .withAttributes(
        coursier.core
          .Attributes(coursier.core.Type.jar, coursier.core.Classifier.sources)
      )
    for
      base   <- session()
      result <- execute(
        base
          .withRepositories(Seq(toCoursier(repository)))
          .withDependencies(Seq(single))
      )
      path <- ZIO.fromEither(
        located(
          organization,
          name,
          revision,
          result,
          Some(JarClassifier("sources")),
        )
      )
      sha <- digest(path)
    yield ResolvedArtifact(organization, name, revision, path, sha, repository)

  private def solve(
      dependencies: List[Dep],
      version: ScalaVersion,
      platform: Platform,
      repositories: Repositories,
  ): IO[ResolveError, Classpath] =
    val requested = dependencies.map(dep =>
      dependency(
        dep.organization,
        ArtifactName.of(dep, version, platform),
        dep.revision.value,
        exclusionsOf(dep),
      )
    )
    val coursierRepositories =
      Repositories.ordered(repositories).map(toCoursier)
    for
      base   <- session()
      result <- execute(
        nativeExclusions(version, platform)(
          base.withRepositories(coursierRepositories).withDependencies(requested)
        )
      )
      rows <- ZIO.fromEither(
        classpathRows(dependencies, version, platform, result)
      )
      hashed <- ZIO.foreach(rows)(row => place(repositories, row))
    yield Classpath(hashed, evictions(result.resolution))

  private enum CacheUse:
    case Solve, Locked, Offline

  /** Retain uses a file already in the cache and fetches only a missing one. Offline never fetches. Revalidate is
    * Coursier's default, which also rechecks changing artifacts.
    */
  private val opened: CacheUse = mode match
    case CacheMode.Revalidate => CacheUse.Solve
    case CacheMode.Retain     => CacheUse.Locked
    case CacheMode.Offline    => CacheUse.Offline

  private def session(
      use: CacheUse = opened
  ): IO[ResolveError, coursier.Fetch[coursier.util.Task]] =
    ZIO
      .attemptBlocking {
        val configured = coursier.cache.FileCache
          .apply()
          .withLocation(cache.toString)
          .copy(allowCacheSubstitution = false)
        val files = use match
          case CacheUse.Solve  => configured
          case CacheUse.Locked =>
            configured.copy(cachePolicies =
              Seq(
                coursier.cache.CachePolicy.LocalOnly,
                coursier.cache.CachePolicy.FetchMissing,
              )
            )
          case CacheUse.Offline =>
            configured.copy(cachePolicies = Seq(coursier.cache.CachePolicy.LocalOnly))
        coursier
          .Fetch(files)(using coursier.util.Task.sync)
          .noMirrors
          .withGradleModuleSupport(false)
      }
      .mapError(_ => ResolveError.Io(IoFailure(cache, IoFault.Create)))

  private def execute(
      fetch: coursier.Fetch[coursier.util.Task]
  ): IO[ResolveError, coursier.Fetch.Result] =
    ZIO
      .attemptBlockingInterrupt(fetch.eitherResult())
      .mapError(_ => ResolveError.Repository(RepositoryIo.Fetch))
      .flatMap:
        case Left(error)   => ZIO.fail(lift(error))
        case Right(result) => ZIO.succeed(result)

  private def place(
      repositories: Repositories,
      row: Row,
  ): IO[ResolveError, ResolvedArtifact] =
    Repository.recorded(row.url, Repositories.ordered(repositories)) match
      case None             => ZIO.fail(ResolveError.Repository(RepositoryIo.Metadata))
      case Some(repository) =>
        digest(row.path).map(sha =>
          ResolvedArtifact(
            row.organization,
            row.name,
            row.revision,
            row.path,
            sha,
            repository,
            row.classifier,
          )
        )

  private def classpathRows(
      dependencies: List[Dep],
      version: ScalaVersion,
      platform: Platform,
      result: coursier.Fetch.Result,
  ): Either[ResolveError, List[Row]] =
    rows(result).flatMap: found =>
      if found.nonEmpty then Right(found)
      else
        dependencies match
          case dependency :: _ =>
            Left(
              ResolveError.NotFound(
                dependency.organization,
                ArtifactName.of(dependency, version, platform),
                dependency.revision.value,
              )
            )
          case Nil => Right(Nil)

  private def located(
      organization: String,
      name: String,
      revision: String,
      result: coursier.Fetch.Result,
      classifier: Option[JarClassifier] = None,
  ): Either[ResolveError, Path] =
    rows(result).flatMap: found =>
      found.find(row =>
        row.organization == organization && row.name == name && row.revision == revision && row.classifier == classifier
      ) match
        case Some(row) => Right(row.path)
        case None      => Left(ResolveError.NotFound(organization, name, revision))

  private def rows(
      result: coursier.Fetch.Result
  ): Either[ResolveError, List[Row]] =
    result.detailedArtifacts0.toList
      .foldLeft[Either[ResolveError, List[Row]]](Right(Nil)):
        case (Left(error), _)                                         => Left(error)
        case (Right(seen), (dependency, publication, artifact, file)) =>
          val path = file.toPath
          if !file.getName.endsWith(".jar") then Right(seen)
          else
            val raw = publication match
              case Right(published) => published.classifier.value
              case Left(variant)    => variant.classifier.fold("")(_.value)
            for
              version <- result.resolution.reconciledVersions
                .get(dependency.module)
                .toRight(ResolveError.Repository(RepositoryIo.Metadata))
              classifier <-
                if raw.isEmpty then Right(None)
                else
                  JarClassifier
                    .from(raw)
                    .left
                    .map(error => ResolveError.Lock(LockError.Classifier(error)))
                    .map(Some(_))
              row = Row(
                dependency.module.organization.value,
                dependency.module.name.value,
                version.asString,
                path,
                artifact.url,
                classifier,
              )
            yield
              if seen.exists(prior =>
                  prior.organization == row.organization && prior.name == row.name && prior.classifier == row.classifier
                )
              then seen
              else seen :+ row

  /** Project metadata is a map, so this order is sorted. The classpath order is not.
    */
  private def evictions(resolution: coursier.core.Resolution): List[Eviction] =
    val selected                                                    = resolution.reconciledVersions
    def one(dependency: coursier.core.Dependency): Option[Eviction] =
      selected
        .get(dependency.module)
        .flatMap: chosen =>
          val requested = dependency.versionConstraint.asString
          val got       = chosen.asString
          val eviction  = Eviction(
            dependency.module.organization.value,
            dependency.module.name.value,
            requested,
            got,
          )
          if requested == got || !Eviction.printable(eviction) then None
          else Some(eviction)
    val roots      = resolution.rootDependencies.toList.flatMap(one)
    val transitive = resolution.projectCache0.toList.flatMap { case (_, (_, project)) =>
      project.dependencies0.toList.flatMap((_, dependency) => one(dependency))
    }
    distinct(roots ++ transitive).sortBy(item => (item.organization, item.name, item.requested, item.selected))

  private def distinct(items: List[Eviction]): List[Eviction] =
    items.foldLeft(List.empty[Eviction])((seen, item) => if seen.contains(item) then seen else seen :+ item)

  /** Scala Native publishes one standard library for Scala 2.13 and another for Scala 3. A native resolve keeps the
    * binary version it is compiling. Scala 3.8 and later also has its own `scalalib`, so the 2.13 one is excluded too.
    */
  private def nativeExclusions(
      version: ScalaVersion,
      platform: Platform,
  )(
      fetch: coursier.Fetch[coursier.util.Task]
  ): coursier.Fetch[coursier.util.Task] =
    val names = excludedNativeLibraries(version, platform)
    if names.isEmpty then fetch
    else
      val rules =
        names.map(name => (coursier.Organization("org.scala-native"), coursier.ModuleName(name)))
      fetch.mapResolutionParams(_.addExclusions(rules*))

  private def excludedNativeLibraries(version: ScalaVersion, platform: Platform): List[String] =
    if platform != Platform.Native then Nil
    else
      val libraries = List("nativelib", "clib", "posixlib", "windowslib", "javalib", "auxlib")
      val chosen    = version.value.split("\\.", 3) match
        case Array("2", "13", _)  => Some(("_3", Nil))
        case Array("3", minor, _) =>
          val scalalib = minor.toIntOption.filter(_ >= 8).toList.map(_ => "scalalib")
          Some(("_2.13", scalalib))
        case _ => None
      chosen match
        case None                  => Nil
        case Some((suffix, extra)) =>
          (libraries ++ extra).map(name => s"$name${PlatformSuffix.Native}$suffix")

  private def dependency(
      organization: String,
      name: String,
      revision: String,
      exclusions: Set[(coursier.Organization, coursier.ModuleName)] = Set.empty,
  ): coursier.core.Dependency =
    coursier
      .Dependency(
        coursier
          .Module(coursier.Organization(organization), coursier.ModuleName(name)),
        coursier.version.VersionConstraint(revision),
      )
      .copy(minimizedExclusions = coursier.core.MinimizedExclusions(exclusions))

  /** The published artifact id, as written. The parent coordinate's suffix is not applied. */
  private def exclusionsOf(dep: Dep): Set[(coursier.Organization, coursier.ModuleName)] =
    dep.exclusions
      .map(item => (coursier.Organization(item.organization.value), coursier.ModuleName(item.artifact.value)))
      .toSet

  private def toCoursier(repository: Repository): coursier.core.Repository =
    repository match
      case Repository.Central    => coursier.Repositories.central
      case Repository.IvyLocal   => coursier.LocalRepositories.ivy2Local
      case Repository.Maven(url) => coursier.MavenRepository(url.value)

  private def digest(path: Path): IO[ResolveError, Sha256] =
    ZIO
      .attemptBlocking(Sha256Digest.of(Chunk.fromArray(Files.readAllBytes(path))))
      .mapError(_ => ResolveError.Io(IoFailure(path, IoFault.Read)))

  private def lift(error: coursier.error.CoursierError): ResolveError =
    error match
      case resolution: coursier.error.ResolutionError =>
        resolution.errors.headOption match
          case Some(
                missing: coursier.error.ResolutionError.CantDownloadModule
              ) =>
            ResolveError.NotFound(
              missing.module.organization.value,
              missing.module.name.value,
              missing.versionConstraint.asString,
            )
          case _ => ResolveError.Repository(RepositoryIo.Metadata)
      case download: coursier.error.FetchError.DownloadingArtifacts =>
        download.errors.toList.map((artifact, failed) =>
          ArtifactMiss(artifact.url, DownloadFaults.of(failed), failed.describe)
        ) match
          case first :: rest => ResolveError.Download(::(first, rest))
          case Nil           => ResolveError.Repository(RepositoryIo.Fetch)
      case _ => ResolveError.Repository(RepositoryIo.Fetch)

  private final case class Row(
      organization: String,
      name: String,
      revision: String,
      path: Path,
      url: String,
      classifier: Option[JarClassifier],
  )

end CoursierResolve

private[resolve] object CoursierResolve:
  def layer(cache: Path, mode: CacheMode): ULayer[Resolve] =
    ZLayer.fromZIO(
      Ref.make(Map.empty[FetchKey, ResolvedArtifact]).map(ref => new CoursierResolve(cache, mode, ref))
    )
