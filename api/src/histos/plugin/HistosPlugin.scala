package histos.plugin

import histos.doctor.DoctorCheck

import zio.{Scope, ZIO}

/** The value `use` receives. `contribute` is data and is not a `ZIO`. `run` is the effect the engine calls later, on
  * this same value. The environment `Scope` ends when `run` returns, including on interrupt. [[PluginScopes]] names
  * that scope and the generation scope, so one acquire can outlive the invocation.
  *
  * The jar names one entry object in `META-INF/histos/plugin`. An object plugin is its own value, and `entryClass` is
  * its class. A plugin configured at `use` returns a value of another class, which closes over that configuration and
  * overrides `entryClass` with the object's class. The loader still loads the entry, checks its id, and runs this
  * value.
  */
trait HistosPlugin:
  def id: PluginId
  def entryClass: Class[?] = getClass
  def contribute: Contribution
  def checks: List[DoctorCheck]    = Nil
  def preflight: List[DoctorCheck] = Nil
  def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput]

object HistosPlugin:
  /** One line in the direct jar: the entry object's binary name. Transitive jars are not read. */
  val EntryResourceName: String = "META-INF/histos/plugin"
