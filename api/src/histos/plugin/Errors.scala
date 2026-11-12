package histos.plugin

import histos.{Dep, ProjectName}

import java.nio.file.Path

/** What fails while loading a plugin. The throwable is not a payload. `binary` is an exception class name. */
enum PluginLoadError:
  case Unlocked(coordinate: String)
  case Unversioned(coordinate: String)
  case VersionMismatch(dependency: Dep, pluginApi: ApiDigest, launcher: ApiDigest)
  case ApiLink(dependency: Dep)
  case NoSource(coordinate: PluginCoordinate)
  case ReservedPackage(binary: String)
  case EngineLeak(coordinate: PluginCoordinate, binary: String)
  case BuildLeak(binary: String)
  case IdMismatch(declared: PluginId, found: PluginId)
  case Defect(coordinate: PluginCoordinate, binary: String)
  case Io(op: PluginIo)
  case Construction(detail: String)

  def message: String = this match
    case Unlocked(coordinate)    => s"plugin $coordinate is not in the lock"
    case Unversioned(coordinate) => s"plugin $coordinate does not contain ${ApiDigest.resource}"
    case VersionMismatch(dependency, pluginApi, launcher) =>
      val coordinate = PluginCoordinate.Published(dependency).render
      s"plugin $coordinate was built against api ${pluginApi.value} and this process loads ${launcher.value}"
    case ApiLink(dependency) =>
      s"plugin ${PluginCoordinate.Published(dependency).render} uses LinkStrategy.Api, which is refused"
    case NoSource(coordinate) =>
      s"plugin ${coordinate.render} needs sources, and this loader does not have them"
    case ReservedPackage(binary)        => s"'$binary' is in a reserved package"
    case EngineLeak(coordinate, binary) => s"plugin ${coordinate.render} links '$binary'"
    case BuildLeak(binary)              => s"the build links '$binary', and no plugin class asked for it"
    case IdMismatch(declared, found)    => s"plugin ${declared.value} loaded as ${found.value}"
    case Defect(coordinate, binary)     => s"plugin ${coordinate.render} failed while loading ($binary)"
    case Io(op)                         => s"plugin ${op.word} failed at ${op.location}"
    case Construction(detail)           => s"plugin construction failed ($detail)"
end PluginLoadError

enum PluginIo:
  case Read(path: Path)
  case Resolve(path: Path)
  case Compile(path: Path)
  case Open(path: Path)

  def word: String = this match
    case Read(_)    => "read"
    case Resolve(_) => "resolve"
    case Compile(_) => "compile"
    case Open(_)    => "open"

  def location: Path = this match
    case Read(path)    => path
    case Resolve(path) => path
    case Compile(path) => path
    case Open(path)    => path
end PluginIo

/** What `run` fails with. Interruption is not one of these. */
enum PluginRunError:
  case Failed(detail: PluginDetail)
  case MissingOutput(name: OutputName)
  case ExtraOutput(path: String)
  case MissingArg(name: ParamName)
  case MissingNote(module: ProjectName, key: NoteKey)
  case Unknown(id: PluginTaskId)
  case Cycle(path: ::[PluginTaskId])
  case Defect(binary: String)
  case EngineLeak(binary: String)
  case Linkage(binary: String)

  def message: String = this match
    case Failed(detail)           => detail.value
    case MissingOutput(name)      => s"plugin output ${name.value} was not written"
    case ExtraOutput(path)        => s"plugin wrote '$path', which was not declared"
    case MissingArg(name)         => s"plugin argument ${name.value} is missing"
    case MissingNote(module, key) => s"module ${module.value} has no note ${key.value}"
    case Unknown(id)              => s"plugin task ${id.render} is not in the graph"
    case Cycle(path)              => s"plugin task cycle ${path.map(_.render).mkString(" -> ")}"
    case Defect(binary)           => s"plugin task failed ($binary)"
    case EngineLeak(binary)       => s"plugin task links '$binary'"
    case Linkage(binary)          => s"plugin task is missing class $binary"
end PluginRunError
