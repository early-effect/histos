package histos.pack

enum MainClassError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "a main class is not empty"
    case Bad(raw) => s"'$raw' is not a main class"
end MainClassError
