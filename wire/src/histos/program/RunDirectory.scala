package histos.program

import java.nio.file.Path

/** Why a tool directory cannot be the program's working directory. */
enum RunDirectoryFault:
  case Missing(path: Path)
  case NotDirectory(path: Path)
  case Outside(path: Path)
  case Unreadable(path: Path)

  def message: String = this match
    case Missing(path)      => s"run directory does not exist: $path"
    case NotDirectory(path) => s"run directory is not a directory: $path"
    case Outside(path)      => s"run directory is outside the repo: $path"
    case Unreadable(path)   => s"run directory could not be read: $path"
end RunDirectoryFault
