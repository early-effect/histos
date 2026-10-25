package histos.cli

import histos.testrun.TestedCell
import histos.zinc.{CompileProblem, CompileSeverity, PositionBase, SourceCount}
import histos.{Platform, ProjectName, ScalaVersion}

import java.nio.file.Path

/** The lines a compile or a test prints. The daemon and the local command use
  * the same text.
  */
object Lines:
  def compiling(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform
  ): String =
    s"compiling ${module.value} ${version.value} ${token(platform)}"

  /** Unchanged names the reason. A changed cell prints only counts it has. */
  def outcome(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      changed: Boolean,
      sources: Option[SourceCount],
      classesWritten: Option[Int]
  ): Option[String] =
    val id = s"${module.value} ${version.value} ${token(platform)}"
    val detail = counts(sources, classesWritten)
    if changed then detail.map(text => s"$id $text")
    else
      Some(
        detail match
          case Some(text) => s"$id skipped unchanged $text"
          case None       => s"$id skipped unchanged"
      )

  /** An unchanged test compile is not tests skipped. The suites still run. */
  def testOutcome(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      changed: Boolean,
      sources: Option[SourceCount],
      classesWritten: Option[Int]
  ): String =
    val id = s"${module.value} ${version.value} ${token(platform)} test"
    val detail = counts(sources, classesWritten)
    if changed then
      detail match
        case Some(text) => s"$id $text"
        case None       => s"$id compiled"
    else
      detail match
        case Some(text) => s"$id compile unchanged $text"
        case None       => s"$id compile unchanged"

  /** File and class numbers are omitted when they are unknown. An empty run
    * spells nothing. `failed` is named only when one happened.
    */
  def tally(
      modules: Int,
      failed: Int,
      files: Option[SourceCount],
      classesWritten: Option[Int]
  ): Option[String] =
    if modules == 0 then None
    else
      val parts =
        files
          .map(count =>
            s"files compiled: ${count.compiled} skipped: ${count.skipped}"
          )
          .toList ++
          List(s"modules: $modules") ++
          classesWritten.map(count => s"classes: $count").toList
      val body = parts.mkString("; ")
      Some(if failed == 0 then body else s"$body failed: $failed")

  def testTally(
      modules: Int,
      failed: Int,
      files: Option[SourceCount],
      classesWritten: Option[Int]
  ): Option[String] =
    tally(modules, failed, files, classesWritten).map(line => s"test $line")

  def diagnostic(repo: Path, problem: CompileProblem): String =
    val severity = problem.severity match
      case CompileSeverity.Error => "error"
      case CompileSeverity.Warn  => "warn"
      case CompileSeverity.Info  => "info"
    val where = problem.path.map: path =>
      val shown = relative(repo, path)
      (problem.startLine, problem.startColumn) match
        case (Some(line), Some(column)) =>
          s"$shown:${place(line, problem.base)}:${place(column, problem.base)}"
        case (Some(line), None) =>
          s"$shown:${place(line, problem.base)}"
        case (None, Some(column)) =>
          s"$shown column ${place(column, problem.base)}"
        case (None, None) => shown
    where match
      case Some(at) => s"$at: $severity: ${problem.message}"
      case None     => s"$severity: ${problem.message}"

  def watching(path: String): String =
    s"watching $path"

  private def counts(
      sources: Option[SourceCount],
      classesWritten: Option[Int]
  ): Option[String] =
    val written = sources match
      case Some(_) => classesWritten
      case None    => None
    val parts =
      sources
        .map(count =>
          s"files compiled: ${count.compiled} skipped: ${count.skipped}"
        )
        .toList ++ written.map(count => s"classes: $count").toList
    if parts.isEmpty then None else Some(parts.mkString(" "))

  private def place(value: Int, base: PositionBase): Int =
    base match
      case PositionBase.Zinc  => value
      case PositionBase.Dotty => value + 1

  def begin(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform
  ): String =
    s"${module.value} ${version.value} ${token(platform)}"

  def tested(cell: TestedCell): String =
    s"${cell.module.value} ${cell.version.value} ${token(cell.platform)} ${cell.status.label}"

  def cached(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      suite: String
  ): String =
    s"${module.value} ${version.value} ${token(platform)} $suite cached"

  private def token(platform: Platform): String =
    platform match
      case Platform.Jvm    => "jvm"
      case Platform.Js     => "js"
      case Platform.Native => "native"

  def relative(repo: Path, path: Path): String =
    val base = repo.toAbsolutePath.normalize
    val full = path.toAbsolutePath.normalize
    (if full.startsWith(base) then base.relativize(full) else full).toString
      .replace('\\', '/')
end Lines
