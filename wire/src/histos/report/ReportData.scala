package histos.report

import histos.testrun.TestedCell

import zio.Chunk
import zio.json.*

import WireJson.given

/** One module that entered a compile. An unknown source set has no class count.
  */
final case class ModuleTally(
    module: String,
    version: String,
    platform: String,
    compiled: Option[Int],
    skipped: Option[Int],
    classes: Option[Int],
) derives JsonCodec

/** The compile tally as data. Totals are absent when a cell poisoned them. */
final case class Observation(
    modules: Chunk[ModuleTally],
    compiled: Option[Int],
    skipped: Option[Int],
    classes: Option[Int],
    moduleCount: Int,
) derives JsonCodec

/** Wire token is `error`, `warn`, or `info`, not a derived object. */
enum Severity:
  case Error, Warn, Info

  def token: String = this match
    case Error => "error"
    case Warn  => "warn"
    case Info  => "info"
end Severity

object Severity:
  given JsonCodec[Severity] =
    JsonCodec.string.transformOrFail(
      {
        case "error" => Right(Severity.Error)
        case "warn"  => Right(Severity.Warn)
        case "info"  => Right(Severity.Info)
        case other   => Left(s"unknown severity $other")
      },
      _.token,
    )

/** The compiler message the CLI line was rendered from. Positions are displayed and 1-based, or absent. */
final case class Diagnostic(
    path: Option[String],
    line: Option[Int],
    column: Option[Int],
    endLine: Option[Int],
    endColumn: Option[Int],
    severity: Severity,
    message: String,
) derives JsonCodec

/** A finished call. Transport failures are not a report, and Histos's own exit is not stored here. */
final case class Report(
    out: Chunk[String],
    err: Chunk[String],
    run: Option[String],
    observation: Option[Observation],
    diagnostics: Chunk[Diagnostic],
    tests: Chunk[TestedCell],
    program: Option[Int],
) derives JsonCodec

object Report:
  def empty: Report =
    Report(Chunk.empty, Chunk.empty, None, None, Chunk.empty, Chunk.empty, None)

enum ObserveError derives JsonCodec:
  case Missing(path: String)
  case Unreadable(path: String)
  case Malformed(path: String)

  def message: String = this match
    case Missing(path)    => s"no observation at $path"
    case Unreadable(path) => s"could not read $path"
    case Malformed(path)  => s"observation at $path is not a tally"
end ObserveError
