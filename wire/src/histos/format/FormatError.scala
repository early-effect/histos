package histos.format

import histos.doctor.{Diagnostic, Noted}
import histos.resolve.{ResolveError, ToolRoot}
import histos.select.SelectError
import histos.workspace.IoFailure

import java.nio.file.Path

/** A package scalafmt-dynamic asked its downloader for. */
final case class ScalafmtPackage(group: String, artifact: String, version: String):
  def shown: String = s"$group:$artifact:$version"

object ScalafmtPackage:
  def of(root: ToolRoot): ScalafmtPackage = ScalafmtPackage(root.organization, root.name, root.revision)

/** Why `histos fmt` stopped before it formatted a file. */
enum FormatError:
  case NotDeclared
  case Conf(error: ScalafmtConfError)
  case Resolved(error: ResolveError)
  case CoreDrift(asked: List[ScalafmtPackage], locked: List[ScalafmtPackage])
  case Load(reason: String)
  case Config(config: Path, reason: String)
  case Tracked(reason: String)
  case Selected(error: SelectError)
  case Io(failure: IoFailure)

  def message: String = this match
    case NotDeclared     => "this build does not format. Add .scalafmt to the Projects settings in histos.scala"
    case Conf(error)     => error.message
    case Resolved(error) => error.message
    case CoreDrift(asked, locked) =>
      s"scalafmt asked for ${asked.map(_.shown).mkString(", ")}, and the lock holds ${locked.map(_.shown).mkString(", ")}"
    case Load(reason)           => s"scalafmt-dynamic did not load: $reason"
    case Config(config, reason) => s"$config: $reason"
    case Tracked(reason)        => s"the git-tracked files could not be listed: $reason"
    case Selected(error)        => error.message
    case Io(failure)            => failure.message

  def diagnose: Diagnostic = this match
    case NotDeclared =>
      Noted.carry("format.declared", message, "histos fmt formats a build that asks for it.", "histos help fmt")
    case Conf(error)     => Noted.carry("format.config", message, error.fix, "histos fmt")
    case Resolved(error) => error.diagnose
    case Selected(error) => error.diagnose
    case CoreDrift(_, _) =>
      Noted.carry(
        "format.lock",
        message,
        "The locked scalafmt-core is not the one scalafmt asked for.",
        "histos resolve --update",
      )
    case other => Noted.carry("format.failed", other.message, other.message, "histos fmt")
end FormatError
