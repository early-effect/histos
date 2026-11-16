package histos.plugin

import histos.lock.LockDefaults
import histos.{PluginRef, Resolution, Resolver, ShipPlugin}

/** Resolvers for catalog and seed inputs. Main-only resolvers are too late for those inputs. */
enum PluginResolvers:
  case Central
  case Listed(resolvers: List[Resolver])

/** Where early strategy services are wired. [[histos.plugin.BootstrapSource.Core]] means there is no strategy file. */
enum BootstrapSource:
  case Core
  case File(path: RepoPath, required: PluginCatalog)

/** Sources available to the build. A coordinate does not activate a service or a task. */
final case class PluginCatalog(entries: List[PluginRef])

object PluginCatalog:
  val empty: PluginCatalog = PluginCatalog(Nil)

  def apply(first: PluginRef | ShipPlugin, rest: (PluginRef | ShipPlugin)*): PluginCatalog =
    PluginCatalog((first +: rest).toList.map(PluginRef.normalize))
end PluginCatalog

/** Core-safe catalog. The main build inherits [[resolution]] unless it selects another intent. */
trait HistosPlugins:
  def resolution: Resolution = Resolution.Solve

  def resolvers: PluginResolvers = PluginResolvers.Central

  def bootstrapLocks: LockDefaults = LockDefaults.core

  def bootstrap: BootstrapSource = BootstrapSource.Core

  def sources: PluginCatalog
end HistosPlugins
