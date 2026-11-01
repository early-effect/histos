package histos.workflow

import histos.workspace.{IoFailure}
import histos.{ProjectName}
import java.nio.file.{Path}

enum CiError:
  case BadName(raw: String)
  case Unknown(name: ProjectName)
  case NotModule(name: ProjectName)
  case Duplicate(name: ProjectName)
  case Missing(path: Path)
  case Drift
  case Io(failure: IoFailure)

  def message: String = this match
    case BadName(raw)    => s"'$raw' is not a project name"
    case Unknown(name)   => s"'${name.value}' is not a project"
    case NotModule(name) => s"'${name.value}' is not a module"
    case Duplicate(name) => s"'${name.value}' is listed twice"
    case Missing(path)   => s"no workflow at $path"
    case Drift           => "workflow does not match the graph"
    case Io(failure)     => failure.message
end CiError
