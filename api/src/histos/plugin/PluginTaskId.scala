package histos.plugin

/** `pluginId/localName`. `inspect` and `versions` do not parse, so they cannot occupy a plugin key. */
final case class PluginTaskId(plugin: PluginId, name: TaskName):
  def render: String = s"${plugin.value}/${name.value}"

enum PluginTaskError:
  case Shape(raw: String)
  case Plugin(error: PluginIdError)
  case Task(error: TaskNameError)

  def message: String = this match
    case Shape(raw)    => s"'$raw' is not a plugin task id"
    case Plugin(error) => error.message
    case Task(error)   => error.message
end PluginTaskError

object PluginTaskId:
  def parse(raw: String): Either[PluginTaskError, PluginTaskId] =
    raw.split("/", -1).toList match
      case plugin :: name :: Nil =>
        for
          id   <- PluginId.from(plugin).left.map(PluginTaskError.Plugin(_))
          task <- TaskName.from(name).left.map(PluginTaskError.Task(_))
        yield PluginTaskId(id, task)
      case _ => Left(PluginTaskError.Shape(raw))
