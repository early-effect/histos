package histos.select

import histos.doctor.{Diagnostic, Noted}
import histos.ProjectName

enum SelectError:
  case Duplicate(name: ProjectName)

  /** `near` is the projects whose names are close to `name`, nearest first. */
  case Unknown(name: ProjectName, near: List[ProjectName])

  def message: String = this match
    case Duplicate(name)  => s"'${name.value}' is listed twice"
    case Unknown(name, _) => s"'${name.value}' is not a project"

  def diagnose: Diagnostic = this match
    case Duplicate(name) =>
      Noted.carry("project.duplicate", message, s"${name.value} is already in the request.", "histos graph")
    case Unknown(_, near) =>
      val explain = near match
        case head :: _ => s"did you mean ${head.value}?"
        case Nil       => "The name is not a project in this graph."
      val fix = near.headOption match
        case Some(guess) => Noted.command(s"histos compile ${guess.value}")
        case None        => Noted.command("histos graph")
      Noted.error("project.unknown", message, explain, fix)
end SelectError
