package histos.load

import histos.BuildError

enum LoadError:
  case MissingFile(path: String)
  case CompileFailed(diagnostics: List[String])
  case NoEntry
  case AmbiguousEntry(names: List[String])
  case BrokenGraph(error: BuildError)
  case Failed(detail: String)

  def message: String = this match
    case MissingFile(path)            => s"no build file at $path"
    case CompileFailed(diagnostics)   => diagnostics.mkString("\n")
    case NoEntry                      => "histos.scala does not define one object that extends HistosBuild"
    case AmbiguousEntry(names)        => s"histos.scala defines more than one HistosBuild: ${names.mkString(", ")}"
    case BrokenGraph(error)           => error.message
    case Failed(detail)               => detail
