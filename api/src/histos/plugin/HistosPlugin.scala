package histos.plugin

import histos.doctor.DoctorCheck

import zio.{Scope, ZIO}

/** One object in the plugin jar. `contribute` is data and is not a `ZIO`. `run` is the effect the engine calls later.
  * The environment `Scope` ends when `run` returns, including on interrupt. [[PluginScopes]] names that scope and the
  * generation scope, so one acquire can outlive the invocation.
  */
trait HistosPlugin:
  def id: PluginId
  def contribute: Contribution
  def checks: List[DoctorCheck]    = Nil
  def preflight: List[DoctorCheck] = Nil
  def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput]

object HistosPlugin:
  /** One line in the direct jar: the entry object's binary name. Transitive jars are not read. */
  val EntryResourceName: String = "META-INF/histos/plugin"
