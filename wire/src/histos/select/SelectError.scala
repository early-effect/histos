package histos.select

import histos.ProjectName

enum SelectError:
  case Duplicate(name: ProjectName)
  case Unknown(name: ProjectName)

  def message: String = this match
    case Duplicate(name) => s"'${name.value}' is listed twice"
    case Unknown(name)   => s"'${name.value}' is not a project"
end SelectError
