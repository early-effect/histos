package histos.resolve

import histos.plugin.{LinkStrategy, PluginCoordinate, PluginLoadError}
import histos.Dep

/** A header coordinate matched to the plugin cell that pins it. */
object PluginCells:
  def locate(lock: DependencyLock, dependency: Dep): Either[PluginLoadError, LockedPlugin] =
    lock.plugins.find(plugin =>
      plugin.organization == dependency.organization &&
        plugin.name == dependency.name &&
        plugin.revision == dependency.revision
    ) match
      case None => Left(PluginLoadError.Unlocked(PluginCoordinate.Published(dependency).render))
      case Some(plugin) =>
        plugin.link match
          case LinkStrategy.Api => Left(PluginLoadError.ApiLink(dependency))
          case _                => Right(plugin)
