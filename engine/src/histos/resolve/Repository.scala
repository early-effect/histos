package histos.resolve

import histos.jdk.Sha256
import histos.workspace.{IoFailure, Workspace}
import histos.plugin.{LinkStrategy, PluginCoordinate, PluginLoadError}
import histos.{Aggregate, Dep, DepKind, Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}

import zio.{IO, ULayer, ZIO, ZLayer}

/** Declared repositories first. `Central` is last when it is present. Empty is not representable.
  */
final case class Repositories(first: Repository, rest: List[Repository]):
  def all: List[Repository] = first :: rest

object BuildResolvers:
  /** An empty declaration is Central. A listed `central` is moved last. It is not added when absent.
    */
  def apply(declared: List[histos.Resolver]): Repositories =
    declared match
      case Nil           => Repositories.central
      case first :: rest =>
        val mapped = mapOne(first)
        val tail   = rest.map(mapOne)
        pack(Repositories.ordered(Repositories(mapped, tail)))

  private def mapOne(resolver: histos.Resolver): Repository =
    resolver match
      case histos.Resolver.Central    => Repository.Central
      case histos.Resolver.Maven(url) =>
        Repository.Maven(RepoUrl.trusted(url.value))

  private def pack(ordered: List[Repository]): Repositories =
    ordered match
      case first :: rest => Repositories(first, rest)
      case Nil           => Repositories.central
end BuildResolvers

object Repositories:
  val central: Repositories = Repositories(Repository.Central, Nil)

  def ordered(repositories: Repositories): List[Repository] =
    val all      = repositories.all
    val declared = all.filter(_ != Repository.Central)
    val central  = if all.exists(_ == Repository.Central) then List(Repository.Central)
    else Nil
    declared ++ central
end Repositories
