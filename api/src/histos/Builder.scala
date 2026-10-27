package histos

import histos.plugin.HistosPlugin

/** Plugin instances [[Projects]] copies onto the graph. Constructing this does not call `contribute`. */
final case class Uses(plugins: List[HistosPlugin])

/** Register these plugins. The list is the first argument of [[Projects]], before the projects. */
def use(plugin: HistosPlugin, rest: HistosPlugin*): Uses =
  Uses(plugin :: rest.toList)

/** Starts a compiling project. One platform and one Scala version are required, so omitting either does not compile.
  *
  * The platforms are the cases `Jvm`, `Js`, and `Native`. The result type keeps that union, and only those settings
  * exist.
  */
transparent inline def module(
    name: String,
    inline platform: Platform,
    inline platforms: Platform*
)(
    version: ScalaVersion,
    versions: ScalaVersion*
): ModuleDef[?] =
  ${ BuilderMacro.module('name, 'platform, 'platforms, 'version, 'versions) }

/** A root that does not compile. It names the modules it is given. A nested aggregate does not compile.
  *
  * Any module is accepted. The illegal platform setting has already failed on the module.
  */
inline def aggregate(name: String, project: ModuleDef[?], projects: ModuleDef[?]*): AggregateDef =
  ${ BuilderMacro.aggregate('name, 'project, 'projects) }
