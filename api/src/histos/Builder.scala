package histos

import histos.plugin.HistosPlugin

/** Plugin instances [[Projects]] copies onto the graph. Constructing this does not call `contribute`. */
final case class Uses(plugins: List[HistosPlugin])

/** Register these plugins. The list is the first argument of [[Projects]], before the projects. */
def use(plugin: HistosPlugin, rest: HistosPlugin*): Uses =
  Uses(plugin :: rest.toList)

/** Starts a compiling project. One platform and one Scala version are required, so omitting either does not compile. */
inline def module(name: String, platform: Platform, platforms: Platform*)(
    version: ScalaVersion,
    versions: ScalaVersion*,
): ModuleDef =
  ${ BuilderMacro.module('name, 'platform, 'platforms, 'version, 'versions) }

/** A root that does not compile. It names the modules it is given. A nested aggregate does not compile. */
inline def aggregate(name: String, project: ModuleDef, projects: ModuleDef*): AggregateDef =
  ${ BuilderMacro.aggregate('name, 'project, 'projects) }

