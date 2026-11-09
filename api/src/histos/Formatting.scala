package histos

import histos.plugin.RepoPath

/** Where the scalafmt configuration lives. `Root` is `.scalafmt.conf`, which a [[RepoPath]] cannot spell. */
enum ScalafmtConf:
  case Root
  case At(path: RepoPath)

  def relative: String = this match
    case Root     => ".scalafmt.conf"
    case At(path) => path.value
end ScalafmtConf

/** Whether this build formats its sources. `Scalafmt` locks the formatter, renders the CI check, and turns on the
  * doctor checks. The scalafmt version is the `version` line in that configuration.
  */
enum Formatting:
  case Off
  case Scalafmt(conf: ScalafmtConf)
