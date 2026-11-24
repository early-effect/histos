package histos.watch

import java.nio.file.Path

enum WatchError:
  case Read(path: Path)
  case Link(path: Path)

  def message: String = this match
    case Read(path) => s"could not read sources under $path"
    case Link(path) => s"symbolic link in watched inputs at $path"
end WatchError
