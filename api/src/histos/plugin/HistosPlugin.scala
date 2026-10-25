package histos.plugin

import zio.{Scope, ZIO}

/** One object in the plugin jar. `contribute` is data and is not a `ZIO`. `run` is the effect the engine calls later,
  * inside a `Scope`.
  */
trait HistosPlugin:
  def id: PluginId
  def contribute: Contribution
  def run(request: RunRequest): ZIO[Scope, PluginRunError, PluginOutput]

object HistosPlugin:
  /** One line in the direct jar: the entry object's binary name. Transitive jars are not read. */
  val entryResource: String = "META-INF/histos/plugin"
