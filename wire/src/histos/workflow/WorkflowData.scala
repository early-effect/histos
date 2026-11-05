package histos.workflow

import histos.doctor.{Diagnostic, Noted}
import histos.plugin.{ActionName, JobName, RunnerName, SecretName, TargetName}
import histos.workspace.IoFailure
import histos.ProjectName
import java.nio.file.Path

enum CiError:
  case BadName(raw: String)
  case Unknown(name: ProjectName, near: List[String] = Nil)
  case NotModule(name: ProjectName)
  case Duplicate(name: ProjectName)
  case Missing(path: Path)
  case Drift(file: String, hint: String)
  case Io(failure: IoFailure)
  case DuplicateSite(first: JobName, second: JobName)
  case IllegalSite(name: JobName)
  case UnknownNeed(job: JobName, need: JobName)
  case DuplicateNeed(job: JobName, need: JobName)
  case UnknownModule(job: JobName, module: ProjectName)
  case DuplicateTarget(job: JobName, target: TargetName)
  case UnpinnedAction(job: JobName, action: ActionName)
  case GrantMissing(job: JobName, action: ActionName)
  case UnknownRunner(job: JobName, runner: RunnerName)
  case UnknownSecret(secret: SecretName)
  case UnreachableCond(job: JobName)
  case DispatchInputConflict(name: String)
  case ConcurrencyConflict(file: String)
  case UnpinnedWorkflowRun(name: String)
  case Pins(text: String)
  case NoDefaultBranch
  case JobTimeout(count: Int)
  case LlvmChecksum(archive: String)

  def message: String = this match
    case BadName(raw)                 => s"'$raw' is not a project name"
    case Unknown(name, _)             => s"'${name.value}' is not a project"
    case NotModule(name)              => s"'${name.value}' is not a module"
    case Duplicate(name)              => s"'${name.value}' is listed twice"
    case Missing(path)                => s"no workflow at $path"
    case Drift(file, _)               => s"$file does not match the graph"
    case Io(failure)                  => failure.message
    case DuplicateSite(first, second) =>
      s"job ${first.value} and job ${second.value} both name a site"
    case IllegalSite(name)      => s"job ${name.value} does not render one site"
    case UnknownNeed(job, need) =>
      s"job ${job.value} needs ${need.value}, which is not a job on this graph"
    case DuplicateNeed(job, need)   => s"job ${job.value} needs ${need.value} twice"
    case UnknownModule(job, module) =>
      s"job ${job.value} names module ${module.value}, which is not a module"
    case DuplicateTarget(job, target) => s"job ${job.value} names target ${target.value} twice"
    case UnpinnedAction(job, action)  => s"job ${job.value} uses ${action.value}, which has no pin"
    case GrantMissing(job, action)    =>
      s"job ${job.value} uses ${action.value} without its grant"
    case UnknownRunner(job, runner) =>
      s"job ${job.value} names runner ${runner.value}, which is not in the catalog"
    case UnknownSecret(secret)       => s"secret ${secret.value} is not declared"
    case UnreachableCond(job)        => s"job ${job.value} has a condition its events do not reach"
    case DispatchInputConflict(name) =>
      s"workflow_dispatch input $name is declared twice with different types"
    case ConcurrencyConflict(file) => s"jobs in $file do not share a concurrency group"
    case UnpinnedWorkflowRun(name) => s"workflow $name is not rendered by this graph"
    case Pins(text)                => text
    case NoDefaultBranch =>
      "the repository default branch is not set. refs/remotes/origin/HEAD is absent, and origin did not answer HEAD"
    case JobTimeout(count) =>
      s"a job timeout is between 1 and 360 minutes, not $count"
    case LlvmChecksum(archive) =>
      s"no sha256 is pinned for $archive"

  def diagnose: Diagnostic = this match
    case Drift(file, hint) =>
      val line = hint match
        case s"line $raw\n$_" => raw.toIntOption
        case _                => None
      Noted.at(
        Noted.carry(
          "workflow.drift",
          message,
          if hint.isEmpty then "The rendered workflow does not match the file." else hint,
          "histos workflow --update",
        ),
        file,
        line,
      )
    case Unknown(_, near) =>
      val explain = near match
        case head :: _ => s"did you mean $head?"
        case Nil       => "The name is not a project in this graph."
      val fix = near.headOption match
        case Some(guess) => Noted.command(s"histos test $guess")
        case None        => Noted.command("histos graph")
      Noted.error("workflow.unknown-project", message, explain, fix)
    case Missing(_) =>
      Noted.carry("workflow.missing", message, "The generated file is not on disk.", "histos workflow --update")
    case other =>
      Noted.carry("workflow.failed", other.message, other.message, "histos workflow --update")
end CiError
