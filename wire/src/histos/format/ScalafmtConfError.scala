package histos.format

import histos.workspace.IoFailure

import java.nio.file.Path

/** Why the declared scalafmt configuration does not name a scalafmt Histos can lock. */
enum ScalafmtConfError:
  case Missing(config: Path)
  case Unreadable(failure: IoFailure)
  case NoVersion(config: Path)
  case BadVersion(config: Path, raw: String)
  case Unsupported(config: Path, version: String)

  def message: String = this match
    case Missing(config)              => s"the scalafmt configuration $config does not exist"
    case Unreadable(failure)          => failure.message
    case NoVersion(config)            => s"$config has no top-level version line"
    case BadVersion(config, raw)      => s"$config names version '$raw', which is not a scalafmt version"
    case Unsupported(config, version) => s"$config names scalafmt $version. Histos formats with 3.0.0 or newer"

  /** What to change so the configuration names a lockable scalafmt. */
  def fix: String = this match
    case Missing(_)        => "Add the configuration, or point .scalafmt(...) at it."
    case Unreadable(_)     => "Make the configuration readable."
    case NoVersion(_)      => "Add a top-level line such as version = \"3.11.5\"."
    case BadVersion(_, _)  => "Write the version as major.minor.patch, such as version = \"3.11.5\"."
    case Unsupported(_, _) => "Move the version line to 3.0.0 or newer."
end ScalafmtConfError
