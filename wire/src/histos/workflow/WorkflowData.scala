package histos.workflow

import histos.doctor.{Diagnostic, Noted}
import histos.plugin.JobName
import histos.workspace.IoFailure
import histos.{CiFault, ProjectName}
import histos.version.ShipName
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

  /** A job the graph arranges that no workflow can render. */
  case Fault(fault: CiFault)
  case Pins(text: String)
  case NoDefaultBranch
  case JobTimeout(count: Int)
  case LlvmChecksum(archive: String)
  case SnapshotClients(ship: ShipName)

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
    case Fault(fault)    => fault.message
    case Pins(text)      => text
    case NoDefaultBranch =>
      "the repository default branch is not set. refs/remotes/origin/HEAD is absent, and origin did not answer HEAD"
    case JobTimeout(count) =>
      s"a job timeout is between 1 and 360 minutes, not $count"
    case LlvmChecksum(archive) =>
      s"no sha256 is pinned for $archive"
    case SnapshotClients(ship) =>
      s"ship ${ship.value} builds clients on snapshots, and it is not the one asset ship with a native client and an LLVM pin"

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
