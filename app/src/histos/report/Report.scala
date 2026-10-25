package histos.report

import heddle.endpoint.{Schema, SchemaDoc}
import histos.clean.Products
import histos.workspace.Workspace
import histos.zinc.{GraphCompileError, SourceCount}
import histos.{Platform, ProjectName, ScalaVersion}

import java.nio.file.Path

import zio.json.*
import zio.{Chunk, IO, ZIO}

/** One module that entered a compile. An unknown source set has no class count.
  */
final case class ModuleTally(
    module: String,
    version: String,
    platform: String,
    compiled: Option[Int],
    skipped: Option[Int],
    classes: Option[Int],
) derives JsonCodec,
      Schema

/** The compile tally as data. Totals are absent when a cell poisoned them. */
final case class Observation(
    modules: Chunk[ModuleTally],
    compiled: Option[Int],
    skipped: Option[Int],
    classes: Option[Int],
    moduleCount: Int,
) derives JsonCodec,
      Schema

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

  given Schema[Severity] =
    Schema.of(SchemaDoc.Enum(Some("Severity"), List("error", "warn", "info")))
end Severity

/** The compiler message the CLI line was rendered from. Positions are displayed and 1-based, or absent. */
final case class Diagnostic(
    path: Option[String],
    line: Option[Int],
    column: Option[Int],
    endLine: Option[Int],
    endColumn: Option[Int],
    severity: Severity,
    message: String,
) derives JsonCodec,
      Schema

/** A finished call. Transport failures are not a report. */
final case class Report(
    exit: Int,
    out: Chunk[String],
    err: Chunk[String],
    run: Option[String],
    observation: Option[Observation],
    diagnostics: Chunk[Diagnostic],
) derives JsonCodec,
      Schema

enum ObserveError derives Schema, JsonCodec:
  case Missing(path: String)
  case Unreadable(path: String)
  case Malformed(path: String)

  def message: String = this match
    case Missing(path)    => s"no observation at $path"
    case Unreadable(path) => s"could not read $path"
    case Malformed(path)  => s"observation at $path is not a tally"
end ObserveError

object Observation:
  def recorded(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      sources: Option[SourceCount],
      classesWritten: Option[Int],
  ): ModuleTally =
    val written             = sources.flatMap(_ => classesWritten)
    val (compiled, skipped) = sources match
      case Some(count) => (Some(count.compiled), Some(count.skipped))
      case None        => (None, None)
    ModuleTally(
      module.value,
      version.value,
      platform.token,
      compiled,
      skipped,
      written,
    )

  /** A failure with a full cell identity. Other failures only poison totals. */
  def failed(error: GraphCompileError): Option[ModuleTally] =
    error match
      case GraphCompileError.Failed(module, version, platform, _) =>
        Some(blank(module, version, platform))
      case GraphCompileError.MissingUpstream(module, _, version, platform) =>
        Some(blank(module, version, platform))
      case GraphCompileError.Runtime(_) | GraphCompileError.Target(_) | GraphCompileError.UnknownCell(_, _) |
          GraphCompileError.NoRoot(_) | GraphCompileError.NoSources(_, _) | GraphCompileError.Cycle(_) |
          GraphCompileError.Lock(_) | GraphCompileError.LockRead(_) | GraphCompileError.RootRead(_) |
          GraphCompileError.Output(_) | GraphCompileError.Cells(_) =>
        None

  def merge(
      left: Option[Observation],
      right: Option[Observation],
  ): Option[Observation] =
    (left, right) match
      case (None, other)          => other
      case (other, None)          => other
      case (Some(one), Some(two)) => Some(merged(one, two))

  def combine(parts: List[Option[Observation]]): Option[Observation] =
    parts.foldLeft(Option.empty[Observation])(merge)

  private def merged(left: Observation, right: Observation): Observation =
    Observation(
      left.modules ++ right.modules,
      sum(left.compiled, right.compiled),
      sum(left.skipped, right.skipped),
      sum(left.classes, right.classes),
      left.moduleCount + right.moduleCount,
    )

  private def sum(left: Option[Int], right: Option[Int]): Option[Int] =
    (left, right) match
      case (Some(one), Some(two)) => Some(one + two)
      case _                      => None

  private def blank(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
  ): ModuleTally =
    ModuleTally(module.value, version.value, platform.token, None, None, None)
end Observation

/** `.histos/observation.json`. One file, replaced, not a log. */
object Stored:
  def read(repo: Path): IO[ObserveError, Observation] =
    val path  = Products.observation(repo)
    val shown = path.toString
    Workspace
      .readIfPresent(path)
      .mapError(_ => ObserveError.Unreadable(shown))
      .flatMap:
        case None       => ZIO.fail(ObserveError.Missing(shown))
        case Some(text) =>
          text.fromJson[Observation] match
            case Right(found) => ZIO.succeed(found)
            case Left(_)      => ZIO.fail(ObserveError.Malformed(shown))

  def keep(
      repo: Path,
      observation: Option[Observation],
  ): IO[histos.workspace.IoFailure, Unit] =
    val path = Products.observation(repo)
    observation match
      case None        => Workspace.delete(path)
      case Some(found) => Workspace.writeString(path, found.toJson)
end Stored
