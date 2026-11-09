package histos.watch

import java.nio.file.Path

enum WatchError:
  case Read(path: Path)

  def message: String = this match
    case Read(path) => s"could not read sources under $path"
end WatchError
