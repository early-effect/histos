package histos.resolve

import histos.jdk.Sha256
import histos.workspace.{IoFailure, Workspace}
import histos.plugin.{LinkStrategy, PluginCoordinate, PluginLoadError}
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

