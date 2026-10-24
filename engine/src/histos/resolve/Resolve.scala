package histos.resolve

import histos.jdk.Sha256
import histos.workspace.{IoFailure, Workspace}
import histos.plugin.{LinkStrategy, PluginCoordinate, PluginLoadError}
import histos.{Aggregate, Dep, DepKind, Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}

import zio.{IO, ULayer, ZIO, ZLayer}

trait Resolve:
  def resolve(cell: ResolveCell): IO[ResolveError, Classpath]
  /** Download one locked artifact from the repository the lock recorded. No transitive solve. */
  def fetch(artifact: LockedArtifact): IO[ResolveError, ResolvedArtifact]
  /** Resolve one plugin coordinate as a Scala 3 JVM binary. Plugins are not module cells. */
  def resolvePlugin(dependency: Dep, repositories: Repositories): IO[ResolveError, Classpath]
  /** Download the sources jar of one locked coordinate. The caller compares the checksum. */
  def sources(organization: String, name: String, revision: String, repository: Repository): IO[ResolveError, ResolvedArtifact]

object Resolve:
  def live(cache: Path): ULayer[Resolve] =
    ZLayer.succeed(CoursierResolve(cache))

  def lockPath(repo: Path): Path =
    repo.resolve(".histos").resolve("dependencies.lock")

  /** Aggregates have no cell. Order is graph order, then platform, then Scala version. A test cell follows its compile cell. */
  def cells(graph: ProjectGraph, repositories: Repositories): List[ResolveCell] =
    graph.projects.flatMap:
      case Aggregate(_, _, _) => Nil
      case Module(name, platforms, versions, dependencies, _, _, _, _, testDependencies, _, _, _, _, _, _, _, _, _) =>
        platforms.all.flatMap: platform =>
          versions.all.flatMap: version =>
            val compileDeps = Dep.select(dependencies, platform)
            val testDeps = Dep.select(testDependencies, platform)
            val compile = ResolveCell(name, compileDeps, version, platform, repositories, ClasspathScope.Compile)
            val tests =
              if testDeps.isEmpty then Nil
              else
                List(ResolveCell(
                  name,
                  compileDeps ++ testDeps,
                  version,
                  platform,
                  repositories,
                  ClasspathScope.Test,
                ))
            compile :: tests

  def verify(repo: Path, graph: ProjectGraph, repositories: Repositories): ZIO[Resolve, ResolveError, String] =
    val planned = cells(graph, repositories)
    if planned.isEmpty then ZIO.succeed("")
    else
      for
        _     <- ZIO.foreachDiscard(planned)(prepare)
        lock  <- read(lockPath(repo))
        lines <- ZIO.foreach(planned)(cell => materialize(cell, lock).as(describe(cell)))
      yield lines.mkString("\n")

  def update(repo: Path, graph: ProjectGraph, repositories: Repositories): ZIO[Resolve, ResolveError, String] =
    val planned = cells(graph, repositories)
    if planned.isEmpty then ZIO.succeed("")
    else
      for
        _      <- ZIO.foreachDiscard(planned)(prepare)
        solved <- ZIO.foreach(planned): cell =>
          ZIO.serviceWithZIO[Resolve](_.resolve(cell)).map(classpath => lockCell(cell, classpath))
        path = lockPath(repo)
        prior <- read(path).foldZIO(
          {
            case ResolveError.NoLock(_) => ZIO.succeed(DependencyLock(Nil))
            case other                   => ZIO.fail(other)
          },
          ZIO.succeed,
        )
        _ <- write(path, DependencyLock(solved, prior.plugins))
      yield path.toString

  def check(repo: Path, graph: ProjectGraph, repositories: Repositories): ZIO[Resolve, ResolveError, String] =
    val planned = cells(graph, repositories)
    if planned.isEmpty then ZIO.succeed("")
    else
      for
        _     <- ZIO.foreachDiscard(planned)(prepare)
        lock  <- read(lockPath(repo))
        lines <- ZIO.foreach(planned): cell =>
          for
            locked <- ZIO.fromEither(locate(cell, lock))
            fresh  <- ZIO.serviceWithZIO[Resolve](_.resolve(cell))
            _      <- ZIO.fromEither(agree(cell, fresh, locked))
          yield describe(cell)
      yield lines.mkString("\n")

  /**
   * Resolve each header coordinate and write those plugin cells, keeping module cells already there.
   * The write happens only after every coordinate resolves. A failure leaves the previous file.
   */
  def updatePlugins(repo: Path, requests: List[PluginRequest], repositories: Repositories): ZIO[Resolve, ResolveError, String] =
    val requested = requests.collect { case PluginRequest.Dep(dependency, link) => (dependency, link) }
    if requested.isEmpty then ZIO.succeed("")
    else
      for
        solved <- ZIO.foreach(requested): (dependency, link) =>
          ZIO.serviceWithZIO[Resolve](_.resolvePlugin(dependency, repositories)).flatMap: classpath =>
            lockPlugin(dependency, link, classpath)
        path = lockPath(repo)
        prior <- read(path).foldZIO(
          {
            case ResolveError.NoLock(_) => ZIO.succeed(DependencyLock(Nil))
            case other                   => ZIO.fail(other)
          },
          ZIO.succeed,
        )
        _ <- write(path, DependencyLock(prior.cells, solved))
      yield path.toString

  /** A missing plugin cell is `Unlocked` and does not compile. A path plugin has no cell. */
  def requirePlugins(repo: Path, requests: List[PluginRequest]): ZIO[Resolve, ResolveError, Unit] =
    val dependencies = requests.collect { case PluginRequest.Dep(dependency, _) => dependency }
    if dependencies.isEmpty then ZIO.unit
    else
      val path = lockPath(repo)
      read(path).foldZIO(
        {
          case ResolveError.NoLock(_) =>
            dependencies match
              case head :: _ =>
                ZIO.fail(ResolveError.Plugin(PluginLoadError.Unlocked(PluginCoordinate.Published(head).render)))
              case Nil => ZIO.fail(ResolveError.NoLock(path))
          case other => ZIO.fail(other)
        },
        lock =>
          ZIO.foreachDiscard(dependencies): dependency =>
            ZIO.fromEither(PluginCells.locate(lock, dependency)).mapError(error => ResolveError.Plugin(error)),
      )

  /** Re-resolve plugin cells and fail when the fresh classpath disagrees with the lock. */
  def checkPlugins(repo: Path, requests: List[PluginRequest], repositories: Repositories): ZIO[Resolve, ResolveError, Unit] =
    val dependencies = requests.collect { case PluginRequest.Dep(dependency, _) => dependency }
    if dependencies.isEmpty then ZIO.unit
    else
      for
        lock <- read(lockPath(repo))
        _ <- ZIO.foreachDiscard(dependencies): dependency =>
          lock.plugins.find(plugin =>
            plugin.organization == dependency.organization &&
              plugin.name == dependency.name &&
              plugin.revision == dependency.revision
          ) match
            case None =>
              ZIO.fail(ResolveError.NotFound(
                dependency.organization,
                ArtifactName.of(dependency, pluginScala, Platform.Jvm),
                dependency.revision,
              ))
            case Some(locked) =>
              for
                fresh <- ZIO.serviceWithZIO[Resolve](_.resolvePlugin(dependency, repositories))
                _     <- ZIO.fromEither(agreePlugin(dependency, fresh, locked))
                _     <- checkSource(locked)
              yield ()
      yield ()

  /** Compare directs, then download each locked jar. Does not re-solve. */
  def materialize(cell: ResolveCell, lock: DependencyLock): ZIO[Resolve, ResolveError, Classpath] =
    for
      _         <- prepare(cell)
      locked    <- ZIO.fromEither(locate(cell, lock))
      artifacts <- ZIO.foreach(locked.artifacts)(artifact => ZIO.serviceWithZIO[Resolve](_.fetch(artifact)))
    yield Classpath(artifacts, locked.evictions)

  private[resolve] def prepare(cell: ResolveCell): IO[ResolveError, Unit] =
    cell.platform match
      case Platform.Jvm | Platform.Js | Platform.Native =>
        cell.dependencies.find(ranged) match
          case Some(dependency) => ZIO.fail(ResolveError.Range(dependency))
          case None             => ZIO.unit

  private def locate(cell: ResolveCell, lock: DependencyLock): Either[ResolveError, LockedCell] =
    val drift = drifted(cell)
    lock.cells.find(same(cell)) match
      case None                                   => Left(drift)
      case Some(locked) if locked.directs != directs(cell) => Left(drift)
      case Some(locked)                           => Right(locked)

  private def agree(cell: ResolveCell, fresh: Classpath, locked: LockedCell): Either[ResolveError, Unit] =
    val drift = drifted(cell)
    val got   = fresh.artifacts.map(artifact => (artifact.organization, artifact.name, artifact.revision, artifact.repository))
    val want  = locked.artifacts.map(artifact => (artifact.organization, artifact.name, artifact.revision, artifact.repository))
    if got != want || fresh.evictions != locked.evictions then Left(drift)
    else
      fresh.artifacts.zip(locked.artifacts).collectFirst:
        case (artifact, lockedArtifact) if artifact.sha256 != lockedArtifact.sha256 =>
          ResolveError.Checksum(artifact.path, lockedArtifact.sha256, artifact.sha256)
      match
        case Some(error) => Left(error)
        case None        => Right(())

  private def lockCell(cell: ResolveCell, classpath: Classpath): LockedCell =
    LockedCell(
      cell.module,
      cell.scalaVersion,
      cell.platform,
      directs(cell),
      classpath.artifacts.map(artifact =>
        LockedArtifact(artifact.organization, artifact.name, artifact.revision, artifact.sha256, artifact.repository),
      ),
      classpath.evictions,
      cell.scope,
    )

  private def directs(cell: ResolveCell): List[LockedDirect] =
    cell.dependencies.map(dependency =>
      LockedDirect(dependency.organization, dependency.name, dependency.revision, dependency.kind),
    )

  private def same(cell: ResolveCell)(locked: LockedCell): Boolean =
    locked.module == cell.module &&
      locked.scalaVersion == cell.scalaVersion &&
      locked.platform == cell.platform &&
      locked.scope == cell.scope

  private def drifted(cell: ResolveCell): ResolveError =
    ResolveError.LockDrift(cell.module, cell.scalaVersion, cell.platform, cell.scope)

  private def describe(cell: ResolveCell): String =
    val base = s"${cell.module.value} ${cell.scalaVersion.value} ${PlatformToken.render(cell.platform)}"
    cell.scope match
      case ClasspathScope.Compile => base
      case ClasspathScope.Test    => s"$base test"

  /** The one reader of `.histos/dependencies.lock`. */
  private[histos] def readLock(repo: Path): IO[ResolveError, DependencyLock] =
    read(lockPath(repo))

  private def read(path: Path): IO[ResolveError, DependencyLock] =
    Workspace.readIfPresent(path).mapError(failure => ResolveError.Io(failure)).flatMap:
      case None       => ZIO.fail(ResolveError.NoLock(path))
      case Some(text) => ZIO.fromEither(DependencyLock.parse(text).left.map(ResolveError.Lock(_)))

  private def lockPlugin(
      dependency: Dep,
      link: LinkStrategy,
      classpath: Classpath,
  ): ZIO[Resolve, ResolveError, LockedPlugin] =
    sourceOf(dependency, link, classpath).map: source =>
      LockedPlugin(
        dependency.organization,
        dependency.name,
        dependency.revision,
        link,
        List(LockedDirect(dependency.organization, dependency.name, dependency.revision, dependency.kind)),
        classpath.artifacts.map(artifact =>
          LockedArtifact(artifact.organization, artifact.name, artifact.revision, artifact.sha256, artifact.repository),
        ),
        source,
      )

  /** `Source` and `BinaryOrSource` record the sources classifier. `ExactBinary` omits the line. */
  private def sourceOf(
      dependency: Dep,
      link: LinkStrategy,
      classpath: Classpath,
  ): ZIO[Resolve, ResolveError, Option[LockedArtifact]] =
    link match
      case LinkStrategy.Source | LinkStrategy.BinaryOrSource =>
        val published = ArtifactName.of(dependency, pluginScala, Platform.Jvm)
        classpath.artifacts.find(artifact =>
          artifact.organization == dependency.organization &&
            artifact.name == published &&
            artifact.revision == dependency.revision
        ) match
          case None => ZIO.fail(ResolveError.NotFound(dependency.organization, published, dependency.revision))
          case Some(direct) =>
            ZIO.serviceWithZIO[Resolve](_.sources(direct.organization, direct.name, direct.revision, direct.repository)).map: fetched =>
              Some(LockedArtifact(fetched.organization, fetched.name, fetched.revision, fetched.sha256, fetched.repository))
      case LinkStrategy.ExactBinary | LinkStrategy.Api => ZIO.succeed(None)

  /** Re-fetch a recorded sources line. A cell with no line does not touch the network. */
  private def checkSource(locked: LockedPlugin): ZIO[Resolve, ResolveError, Unit] =
    locked.source match
      case None => ZIO.unit
      case Some(source) =>
        ZIO.serviceWithZIO[Resolve](_.sources(source.organization, source.name, source.revision, source.repository)).flatMap: fetched =>
          if fetched.sha256 == source.sha256 then ZIO.unit
          else ZIO.fail(ResolveError.Checksum(fetched.path, source.sha256, fetched.sha256))

  private def agreePlugin(dependency: Dep, fresh: Classpath, locked: LockedPlugin): Either[ResolveError, Unit] =
    val got = fresh.artifacts.map(artifact => (artifact.organization, artifact.name, artifact.revision, artifact.repository))
    val want = locked.artifacts.map(artifact => (artifact.organization, artifact.name, artifact.revision, artifact.repository))
    if got != want then Left(ResolveError.PluginDrift(dependency))
    else
      fresh.artifacts.zip(locked.artifacts).collectFirst:
        case (artifact, lockedArtifact) if artifact.sha256 != lockedArtifact.sha256 =>
          ResolveError.Checksum(artifact.path, lockedArtifact.sha256, artifact.sha256)
      match
        case Some(error) => Left(error)
        case None        => Right(())

  private[resolve] val pluginScala: ScalaVersion = ScalaVersion.trusted("3.10.0")

  private[histos] def ranged(dependency: Dep): Boolean =
    val revision = dependency.revision
    revision.startsWith("latest") || revision.exists(marked)

  private def marked(character: Char): Boolean =
    character == '[' || character == ']' || character == '(' || character == ')' || character == '+' || character == ','

  private def write(path: Path, lock: DependencyLock): IO[ResolveError, Unit] =
    Option(path.getParent) match
      case None => ZIO.fail(ResolveError.Repository(RepositoryIo.Write))
      case Some(parent) =>
        ZIO.attemptBlocking(replace(parent, path, DependencyLock.render(lock)))
          .mapError(_ => ResolveError.Repository(RepositoryIo.Write))

  private def replace(parent: Path, path: Path, text: String): Unit =
    Files.createDirectories(parent)
    val name = Option(path.getFileName).map(_.toString).getOrElse("dependencies.lock")
    val temp = parent.resolve(s"$name.tmp")
    Files.writeString(temp, text)
    try Files.move(temp, path, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
    catch
      case _: AtomicMoveNotSupportedException =>
        Files.move(temp, path, StandardCopyOption.REPLACE_EXISTING)
