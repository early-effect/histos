package histos.load

import histos.plugin.PluginCoordinate
import histos.resolve.ArtifactName
import histos.{Dep, Module, ProjectGraph, ProjectName}

import java.nio.file.Path

/** A direct `histos-api` coordinate. A plugin's artifact for that name is this process's API jar. */
private[histos] object ApiDep:
  val Artifact: String = "histos-api"

  def direct(module: Module): Option[Dep] =
    (module.dependencies ++ module.testDependencies).find(isApi(module, _))

  /** Modules the graph names as plugins. A `.pluginLibrary` module is a plugin even when this set is empty. */
  def modules(graph: ProjectGraph): Set[ProjectName] =
    coordinates(graph)

  def plugin(module: Module, names: Set[ProjectName]): Boolean =
    module.pluginLibrary.isDefined || names.contains(module.name)

  def plugin(module: Module, graph: ProjectGraph): Boolean =
    plugin(module, coordinates(graph))

  def resolvedName(name: String): Boolean =
    name.startsWith(Artifact)

  def resolvedJar(path: Path): Boolean =
    Option(path.getFileName).exists(_.toString.startsWith(Artifact))

  private def isApi(module: Module, dep: Dep): Boolean =
    dep.name == Artifact || published(module, dep).startsWith(Artifact)

  private def published(module: Module, dep: Dep): String =
    (module.versions.all, module.platforms.all) match
      case (version :: _, platform :: _) => ArtifactName.of(dep, version, platform)
      case _                             => dep.name

  private def coordinates(graph: ProjectGraph): Set[ProjectName] =
    graph.coordinates.collect { case PluginCoordinate.Module(name) => name }.toSet
end ApiDep
