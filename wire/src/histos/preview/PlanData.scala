package histos.preview

import histos.doctor.{Diagnostic, Noted}
import histos.publish.{LocalPublishError}
import histos.select.{SelectError}
import histos.testrun.{SuitePatternError}
import histos.version.{VersionError}
import histos.workspace.IoFailure
import histos.{Aggregate, Platform, ProjectName}

enum RunMiss:
  case Absent(name: String, near: List[String] = Nil)
  case Aggregate(name: ProjectName)
  case NoMain(name: ProjectName)
  case Choose(name: ProjectName, platforms: List[Platform])
  case Unbuilt(name: ProjectName, platform: Platform, built: List[Platform])
  case UnknownTask(id: String)
  case BadId(detail: String)

  def message: String = this match
    case Absent(name, _)        => s"'$name' is not a module"
    case Aggregate(name)         => s"'${name.value}' is an aggregate"
    case NoMain(name)            => s"'${name.value}' does not name a main"
    case Choose(name, platforms) =>
      val tokens = platforms.map(_.token).mkString(", ")
      s"'${name.value}' builds $tokens. Name one."
    case Unbuilt(name, platform, built) =>
      val tokens = built.map(_.token).mkString(", ")
      s"'${name.value}' does not build ${platform.token}. It builds $tokens"
    case UnknownTask(id) => s"'$id' is not a task"
    case BadId(detail) => detail

  def diagnose: Diagnostic = this match
    case Absent(_, near) =>
      val explain = near match
        case head :: _ => s"did you mean $head?"
        case Nil       => "The name is not a module in this graph."
      val fix = near.headOption match
        case Some(guess) => Noted.command(s"histos run $guess")
        case None        => Noted.command("histos graph")
      Noted.error("run.absent", message, explain, fix)
    case UnknownTask(id) =>
      Noted.carry("run.unknown-task", message, s"'$id' is not a plugin task.", "histos help run")
    case other =>
      Noted.carry("run.failed", other.message, other.message, "histos help run")
end RunMiss

enum PreviewError:
  case Target(error: SelectError)
  case Cycle(module: ProjectName)
  case Sources(failure: IoFailure)
  case Version(error: VersionError)
  case Publish(error: LocalPublishError)
  case Run(miss: RunMiss)
  case Pattern(error: SuitePatternError)

  def message: String = this match
    case Target(error)    => error.message
    case Cycle(module)    => s"'${module.value}' is in a compile cycle"
    case Sources(failure) => failure.message
    case Version(error)   => error.message
    case Publish(error)   => error.message
    case Run(miss)        => miss.message
    case Pattern(error) => error.message

  def diagnose: Diagnostic = this match
    case Target(error)  => error.diagnose
    case Publish(error) => error.diagnose
    case Run(miss)      => miss.diagnose
    case other          => Noted.carry("preview.failed", other.message, other.message, "histos help preview")
end PreviewError
