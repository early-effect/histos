package histos.publish

import histos.workspace.IoFailure
import java.nio.file.Path

enum LauncherError:
  case Line(raw: String)
  case Unknown(raw: String)
  case Duplicate
  case Missing
  case Version(raw: String)
  case Release(raw: String)
  case Repository(raw: String)
  case Resolve(detail: String)
  case Io(error: IoFailure)
  case InvalidPath(path: Path)
  case Absent(path: Path)

  def message: String = this match
    case Line(raw)    => s"launcher properties line is not a field: $raw"
    case Unknown(raw) => s"launcher properties field is unknown: $raw"
    case Duplicate    => "launcher properties repeat histos.version"
    case Missing      => "launcher properties are missing histos.version"
    case Version(raw) =>
      s"launcher version is not a release, a snapshot pointer, or a timestamped revision: $raw"
    case Release(raw)      => s"a release pin does not move: $raw"
    case Repository(raw)   => s"launcher repository is not a supported base: $raw"
    case Resolve(detail)   => detail
    case Io(error)         => error.message
    case InvalidPath(path) =>
      s"launcher path is not a readable regular file: $path"
    case Absent(path) => s"launcher pin is absent: $path"
end LauncherError
