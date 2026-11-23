package histos.load

import histos.plugin.PluginRunError
import histos.resolve.ResolveError
import histos.schedule.ScheduleError
import histos.testrun.ModuleTestError
import histos.zinc.GraphCompileError

/** A plugin run failed. The inner error stays the error that happened. */
enum PluginFault:
  case Task(error: PluginRunError)
  case Products(error: ProductError)
  case MissingProduct(id: histos.plugin.PluginTaskId)
  case Compiled(error: GraphCompileError)
  case Resolved(error: ResolveError)
  case Tested(error: ModuleTestError)
  case Scheduled(error: ScheduleError[PluginFault])

  def message: String = this match
    case Task(error)        => error.message
    case Products(error)    => error.message
    case MissingProduct(id) => s"no committed products for ${id.render}"
    case Compiled(error)    => error.message
    case Resolved(error)    => error.message
    case Tested(error)      => error.message
    case Scheduled(error)   => error.message
end PluginFault
