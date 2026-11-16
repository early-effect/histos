package histos.resolve

import histos.jdk.Sha256
import histos.workspace.{IoFailure, Workspace}
import histos.plugin.{LinkStrategy, PluginCoordinate, PluginLoadError}
import histos.{Aggregate, Dep, DepKind, Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}

import zio.{IO, ULayer, ZIO, ZLayer}

/** The repositories a resolve searches, first to last. Empty is not representable.
  */
final case class Repositories(first: Repository, rest: List[Repository]):
  def all: List[Repository] = first :: rest

object BuildResolvers:
  /** Central's snapshot repository. A trailing slash is the same repository.
    */
  private[histos] val snapshots: Repository =
    Repository.Maven(RepoUrl.trusted("https://central.sonatype.com/repository/maven-snapshots/"))

  /** Ivy local, then the resolvers this build listed, then Sonatype snapshots, then Central. sbt searches
    * `~/.ivy2/local`, then `resolvers`, then Maven Central. Snapshots is always in that list. A repository already
    * present keeps the earlier place.
    */
  def apply(declared: List[histos.Resolver]): Repositories =
    pack(distinct(Repository.IvyLocal :: declared.map(mapOne) ::: List(snapshots, Repository.Central)))

  private[histos] def same(left: Repository, right: Repository): Boolean =
    keyOf(left) == keyOf(right)

  private def mapOne(resolver: histos.Resolver): Repository =
    resolver match
      case histos.Resolver.Central    => Repository.Central
      case histos.Resolver.Maven(url) =>
        Repository.Maven(RepoUrl.trusted(url.value))

  private def distinct(repositories: List[Repository]): List[Repository] =
    def go(left: List[Repository], seen: Set[String], kept: List[Repository]): List[Repository] =
      left match
        case Nil          => kept.reverse
        case head :: tail =>
          val key = keyOf(head)
          if seen.contains(key) then go(tail, seen, kept)
          else go(tail, seen + key, head :: kept)
    go(repositories, Set.empty, Nil)

  private def keyOf(repository: Repository): String =
    repository match
      case Repository.Central    => "central"
      case Repository.IvyLocal   => "ivy"
      case Repository.Maven(url) => url.value.stripSuffix("/")

  private def pack(ordered: List[Repository]): Repositories =
    ordered match
      case first :: rest => Repositories(first, rest)
      case Nil           => Repositories(Repository.IvyLocal, List(snapshots, Repository.Central))
end BuildResolvers

object Repositories:
  val central: Repositories = Repositories(Repository.Central, Nil)

  /** [[BuildResolvers]] already chose the order. Callers search from the front.
    */
  def ordered(repositories: Repositories): List[Repository] =
    repositories.all
end Repositories
