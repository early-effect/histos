package histos.resolve

import histos.jdk.Sha256
import histos.{Aggregate, Dep, DepKind, Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}

import zio.{IO, ULayer, ZIO, ZLayer}

/** A `file:` or `https:` repository root. An empty string is not a url. */
opaque type RepoUrl = String

object RepoUrl:
  given CanEqual[RepoUrl, RepoUrl] = CanEqual.derived

  def from(raw: String): Either[LockError, RepoUrl] =
    histos.RepositoryUrl.from(raw) match
      case Right(url) => Right(url.value)
      case Left(_)    => Left(LockError.BadRepository(raw))

  def file(path: Path): RepoUrl =
    path.toAbsolutePath.normalize.toUri.toASCIIString

  /** The api already checked the text. */
  private[resolve] def trusted(raw: String): RepoUrl = raw

  extension (url: RepoUrl) def value: String = url

enum Repository:
  case Central
  case IvyLocal
  case Maven(url: RepoUrl)

object Repository:
  def token(repository: Repository): String =
    repository match
      case Central    => "central"
      case IvyLocal   => "ivy-local"
      case Maven(url) => s"maven:${url.value}"

  def parse(raw: String): Either[LockError, Repository] =
    raw match
      case "central"  => Right(Central)
      case "ivy-local" => Right(IvyLocal)
      case other if other.startsWith("maven:") =>
        RepoUrl.from(other.drop("maven:".length)).map(Maven(_))
      case other => Left(LockError.BadRepository(other))

  /** The repository whose root is a prefix of this artifact url. */
  def serves(repository: Repository, url: String): Boolean =
    repository match
      case Central =>
        canonical(url).startsWith("https://repo1.maven.org/maven2/")
      case IvyLocal =>
        canonical(url).contains("/.ivy2/local/")
      case Maven(root) =>
        val artifact = canonical(url)
        val base     = canonical(root.value)
        artifact == base || artifact.startsWith(base + "/")

  def recorded(url: String, repositories: List[Repository]): Option[Repository] =
    repositories.find(repository => serves(repository, url))

  private def canonical(raw: String): String =
    val trimmed = raw.stripSuffix("/")
    if trimmed.startsWith("file:///") then "file:/" + trimmed.drop("file:///".length)
    else if trimmed.startsWith("file://") then "file:/" + trimmed.drop("file://".length)
    else trimmed

/** Declared repositories first. `Central` is last when it is present. Empty is not representable. */
final case class Repositories(first: Repository, rest: List[Repository]):
  def all: List[Repository] = first :: rest

object BuildResolvers:
  /** An empty declaration is Central. A listed `central` is moved last. It is not added when absent. */
  def apply(declared: List[histos.Resolver]): Repositories =
    declared match
      case Nil => Repositories.central
      case first :: rest =>
        val mapped = mapOne(first)
        val tail = rest.map(mapOne)
        pack(Repositories.ordered(Repositories(mapped, tail)))

  private def mapOne(resolver: histos.Resolver): Repository =
    resolver match
      case histos.Resolver.Central    => Repository.Central
      case histos.Resolver.Maven(url) => Repository.Maven(RepoUrl.trusted(url.value))

  private def pack(ordered: List[Repository]): Repositories =
    ordered match
      case first :: rest => Repositories(first, rest)
      case Nil           => Repositories.central

object Repositories:
  val central: Repositories = Repositories(Repository.Central, Nil)

  def ordered(repositories: Repositories): List[Repository] =
    val all      = repositories.all
    val declared = all.filter(_ != Repository.Central)
    val central  = if all.exists(_ == Repository.Central) then List(Repository.Central) else Nil
    declared ++ central

final case class ResolveCell(
    module: ProjectName,
    dependencies: List[Dep],
    scalaVersion: ScalaVersion,
    platform: Platform,
    repositories: Repositories,
)

final case class ResolvedArtifact(
    organization: String,
    name: String,
    revision: String,
    path: Path,
    sha256: Sha256,
    repository: Repository,
)

final case class Eviction(organization: String, name: String, requested: String, selected: String)

final case class Classpath(artifacts: List[ResolvedArtifact], evictions: List[Eviction])

enum RepositoryIo:
  case Read, Fetch, Metadata, Write

enum LockField:
  case Cell, Direct, Artifact, Evict

object LockField:
  def from(raw: String): Option[LockField] =
    raw match
      case "cell"     => Some(LockField.Cell)
      case "direct"   => Some(LockField.Direct)
      case "artifact" => Some(LockField.Artifact)
      case "evict"    => Some(LockField.Evict)
      case _          => None

  def token(field: LockField): String =
    field match
      case LockField.Cell     => "cell"
      case LockField.Direct   => "direct"
      case LockField.Artifact => "artifact"
      case LockField.Evict    => "evict"

enum LockError:
  case Empty
  case BadLine(line: String)
  case DuplicateCell(module: ProjectName, scalaVersion: ScalaVersion, platform: Platform)
  case Missing(field: LockField)
  case BadSha256(raw: String)
  case BadRepository(raw: String)

  def message: String = this match
    case Empty => "dependency lock is empty"
    case BadLine(line) => s"dependency lock line is not a field: $line"
    case DuplicateCell(module, version, platform) =>
      s"dependency lock repeats ${module.value} ${version.value} ${PlatformToken.render(platform)}"
    case Missing(field)       => s"dependency lock is missing ${LockField.token(field)}"
    case BadSha256(raw)       => s"dependency lock sha256 is not 64 hex digits: $raw"
    case BadRepository(raw)   => s"dependency lock repository is not central, ivy-local, or a maven url: $raw"

enum ResolveError:
  case Range(dependency: Dep)
  case NotFound(organization: String, name: String, revision: String)
  case Checksum(path: Path, expected: Sha256, actual: Sha256)
  case LockDrift(module: ProjectName, scalaVersion: ScalaVersion, platform: Platform)
  case NoLock(path: Path)
  case UnsupportedPlatform(platform: Platform)
  case Lock(error: LockError)
  case Repository(op: RepositoryIo)

  def message: String = this match
    case ResolveError.Range(dependency) => s"${shown(dependency)} is not an exact revision"
    case ResolveError.NotFound(organization, name, revision) => s"$organization:$name:$revision was not found"
    case ResolveError.Checksum(path, _, _) => s"checksum of $path does not match the lock"
    case ResolveError.LockDrift(module, version, platform) =>
      s"${module.value} ${version.value} ${PlatformToken.render(platform)} does not match the dependency lock"
    case ResolveError.NoLock(path)                  => s"no dependency lock at $path"
    case ResolveError.UnsupportedPlatform(platform) => s"resolve does not fetch ${PlatformToken.render(platform)}"
    case ResolveError.Lock(error)                   => error.message
    case ResolveError.Repository(op)                => s"resolve failed while ${op.toString.toLowerCase}"

  private def shown(dependency: Dep): String =
    val separator = dependency.kind match
      case DepKind.Java   => ":"
      case DepKind.Binary => "::"
      case DepKind.Full   => ":::"
    s"${dependency.organization}$separator${dependency.name}:${dependency.revision}"

/** The resolution seam. Tests substitute a fixture. The live value is the Coursier adapter. */
trait Resolve:
  def resolve(cell: ResolveCell): IO[ResolveError, Classpath]
  /** Download one locked artifact from the repository the lock recorded. No transitive solve. */
  def fetch(artifact: LockedArtifact): IO[ResolveError, ResolvedArtifact]

object Resolve:
  def live(cache: Path): ULayer[Resolve] =
    ZLayer.succeed(CoursierResolve(cache))

  def lockPath(repo: Path): Path =
    repo.resolve(".histos").resolve("dependencies.lock")

  /** Aggregates have no cell. Order is graph order, then platform, then Scala version. */
  def cells(graph: ProjectGraph, repositories: Repositories): List[ResolveCell] =
    graph.projects.flatMap:
      case Aggregate(_, _, _) => Nil
      case Module(name, platforms, versions, dependencies, _, _, _) =>
        platforms.all.flatMap: platform =>
          versions.all.map: version =>
            ResolveCell(name, dependencies, version, platform, repositories)

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
        _ <- write(path, DependencyLock(solved))
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
    val drift = ResolveError.LockDrift(cell.module, cell.scalaVersion, cell.platform)
    lock.cells.find(same(cell)) match
      case None                                   => Left(drift)
      case Some(locked) if locked.directs != directs(cell) => Left(drift)
      case Some(locked)                           => Right(locked)

  private def agree(cell: ResolveCell, fresh: Classpath, locked: LockedCell): Either[ResolveError, Unit] =
    val drift = ResolveError.LockDrift(cell.module, cell.scalaVersion, cell.platform)
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
    )

  private def directs(cell: ResolveCell): List[LockedDirect] =
    cell.dependencies.map(dependency =>
      LockedDirect(dependency.organization, dependency.name, dependency.revision, dependency.kind),
    )

  private def same(cell: ResolveCell)(locked: LockedCell): Boolean =
    locked.module == cell.module && locked.scalaVersion == cell.scalaVersion && locked.platform == cell.platform

  private def describe(cell: ResolveCell): String =
    s"${cell.module.value} ${cell.scalaVersion.value} ${PlatformToken.render(cell.platform)}"

  private def ranged(dependency: Dep): Boolean =
    val revision = dependency.revision
    revision.startsWith("latest") || revision.exists(marked)

  private def marked(character: Char): Boolean =
    character == '[' || character == ']' || character == '(' || character == ')' || character == '+' || character == ','

  private def read(path: Path): IO[ResolveError, DependencyLock] =
    ZIO.attemptBlocking {
      if Files.exists(path) then Right(Files.readString(path)) else Left(ResolveError.NoLock(path))
    }.mapError(_ => ResolveError.Repository(RepositoryIo.Read)).flatMap:
      case Left(error) => ZIO.fail(error)
      case Right(text) => ZIO.fromEither(DependencyLock.parse(text).left.map(ResolveError.Lock(_)))

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
