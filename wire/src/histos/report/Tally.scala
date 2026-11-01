package histos.report

import histos.zinc.{GraphCompileError, SourceCount}
import histos.{Platform, ProjectName, ScalaVersion}

object Tally:
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
end Tally
