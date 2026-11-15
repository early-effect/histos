package histos.cli

import histos.report.{Diagnostic, Severity}
import histos.testrun.{TestStatus, TestedCell}
import histos.theme.Role
import histos.zinc.{CompileProblem, CompileSeverity, PositionBase, SourceCount}
import histos.{Platform, ProjectName, ScalaVersion}

import java.nio.file.Path

import zio.Chunk

/** The lines a compile or a test prints. The daemon and the local command use the same text.
  */
object Lines:
  def compiling(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      column: Int = 0,
  ): String =
    Part.text(compilingParts(module, version, platform, column))

  def compilingParts(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      column: Int = 0,
  ): Chunk[Part] =
    marked(module, version, platform, "compiling", Role.Compiling, column)

  /** Unchanged names the reason. A changed cell prints only counts it has. */
  def outcome(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      changed: Boolean,
      sources: Option[SourceCount],
      classesWritten: Option[Int],
      column: Int = 0,
  ): Option[String] =
    outcomeParts(module, version, platform, changed, sources, classesWritten, column).map(Part.text)

  def outcomeParts(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      changed: Boolean,
      sources: Option[SourceCount],
      classesWritten: Option[Int],
      column: Int = 0,
  ): Option[Chunk[Part]] =
    val id     = s"${named(module, column)} ${version.value} ${token(platform)}"
    val detail = counts(sources, classesWritten)
    if changed then detail.map(text => Chunk(Part.Plain(s"$id $text")))
    else
      val mark = Part.Mark("skipped unchanged", Role.Unchanged)
      Some(
        detail match
          case Some(text) => Chunk(Part.Plain(s"$id "), mark, Part.Plain(s" $text"))
          case None       => Chunk(Part.Plain(s"$id "), mark)
      )

  /** An unchanged test compile is not tests skipped. The suites still run. */
  def testOutcome(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      changed: Boolean,
      sources: Option[SourceCount],
      classesWritten: Option[Int],
      column: Int = 0,
  ): String =
    val id     = s"${named(module, column)} ${version.value} ${token(platform)} test"
    val detail = counts(sources, classesWritten)
    if changed then
      detail match
        case Some(text) => s"$id $text"
        case None       => s"$id compiled"
    else
      detail match
        case Some(text) => s"$id compile unchanged $text"
        case None       => s"$id compile unchanged"

  /** File and class numbers are omitted when they are unknown. An empty run spells nothing. `failed` is named only when
    * one happened.
    */
  def tally(
      modules: Int,
      failed: Int,
      files: Option[SourceCount],
      classesWritten: Option[Int],
  ): Option[String] =
    if modules == 0 then None
    else
      val parts =
        files
          .map(count => s"files compiled: ${count.compiled} skipped: ${count.skipped}")
          .toList ++
          List(s"modules: $modules") ++
          classesWritten.map(count => s"classes: $count").toList
      val body = parts.mkString("; ")
      Some(if failed == 0 then body else s"$body failed: $failed")

  def testTally(
      modules: Int,
      failed: Int,
      files: Option[SourceCount],
      classesWritten: Option[Int],
  ): Option[String] =
    tally(modules, failed, files, classesWritten).map(line => s"test $line")

  /** The CLI line and the structured diagnostic. Callers use both so the two cannot drift. */
  def heard(repo: Path, problem: CompileProblem): (Diagnostic, String) =
    val found = located(problem, problem.path.map(path => relative(repo, path)))
    (found, render(found))

  /** `path:line:column: severity: message`, with the path as the compiler gave it. A failure message carries this. */
  def located(problem: CompileProblem): String =
    render(located(problem, problem.path.map(_.toString)))

  private def located(problem: CompileProblem, shown: Option[String]): Diagnostic =
    Diagnostic(
      shown,
      problem.startLine.map(line => place(line, problem.base)),
      problem.startColumn.map(column => place(column, problem.base)),
      problem.endLine.map(line => place(line, problem.base)),
      problem.endColumn.map(column => place(column, problem.base)),
      severity(problem.severity),
      problem.message,
    )

  def diagnostic(repo: Path, problem: CompileProblem): String =
    Part.text(diagnosticParts(repo, problem))

  def diagnosticParts(repo: Path, problem: CompileProblem): Chunk[Part] =
    val found = located(problem, problem.path.map(path => relative(repo, path)))
    partsOf(found)

  private def render(found: Diagnostic): String =
    Part.text(partsOf(found))

  private def partsOf(found: Diagnostic): Chunk[Part] =
    val token = found.severity.token
    val mark  = found.severity match
      case Severity.Error => Part.Mark(token, Role.Error)
      case Severity.Warn  => Part.Mark(token, Role.Warn)
      case Severity.Info  => Part.Plain(token)
    val where = found.path.map: shown =>
      (found.line, found.column) match
        case (Some(line), Some(column)) => s"$shown:$line:$column"
        case (Some(line), None)         => s"$shown:$line"
        case (None, Some(column))       => s"$shown column $column"
        case (None, None)               => shown
    where match
      case Some(at) => Chunk(Part.Plain(s"$at: "), mark, Part.Plain(s": ${found.message}"))
      case None     => Chunk(mark, Part.Plain(s": ${found.message}"))

  private def severity(level: CompileSeverity): Severity =
    level match
      case CompileSeverity.Error => Severity.Error
      case CompileSeverity.Warn  => Severity.Warn
      case CompileSeverity.Info  => Severity.Info

  def watching(path: String): String =
    s"watching $path"

  private def counts(
      sources: Option[SourceCount],
      classesWritten: Option[Int],
  ): Option[String] =
    val written = sources match
      case Some(_) => classesWritten
      case None    => None
    val parts =
      sources
        .map(count => s"files compiled: ${count.compiled} skipped: ${count.skipped}")
        .toList ++ written.map(count => s"classes: $count").toList
    if parts.isEmpty then None else Some(parts.mkString(" "))

  private def place(value: Int, base: PositionBase): Int =
    base match
      case PositionBase.Zinc  => value
      case PositionBase.Dotty => value + 1

  def begin(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      column: Int = 0,
  ): String =
    s"${named(module, column)} ${version.value} ${token(platform)}"

  /** The cell, then the spans. An empty body is just the cell. */
  def lead(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      rest: Chunk[Part],
      column: Int = 0,
  ): Chunk[Part] =
    val id = begin(module, version, platform, column)
    if Part.text(rest).isEmpty then Chunk(Part.Plain(id))
    else Chunk(Part.Plain(s"$id ")) ++ rest

  /** The cell, then the text. An empty text is just the cell.
    */
  def cell(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      text: String,
      column: Int = 0,
  ): String =
    val id = begin(module, version, platform, column)
    if text.isEmpty then id else s"$id $text"

  /** Suite counts. Absent when this run did not test.
    */
  def suites(passed: Int, failed: Int, cached: Int, modules: Int): Option[String] =
    if passed + failed + cached + modules == 0 then None
    else Some(s"tests passed: $passed failed: $failed cached: $cached modules: $modules")

  def tested(cell: TestedCell, column: Int = 0): String =
    Part.text(testedParts(cell, column))

  def testedParts(cell: TestedCell, column: Int = 0): Chunk[Part] =
    val role = cell.status match
      case TestStatus.NoTests   => Role.NoTests
      case TestStatus.Unmatched => Role.Unmatched
      case TestStatus.Passed    => Role.Passed
      case TestStatus.Failed(_) => Role.Failed
    Chunk(
      Part.Plain(s"${named(cell.module, column)} ${cell.version.value} ${token(cell.platform)} "),
      Part.Mark(cell.status.label, role),
    )

  def cached(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      suite: String,
      column: Int = 0,
  ): String =
    Part.text(cachedParts(module, version, platform, suite, column))

  def cachedParts(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      suite: String,
      column: Int = 0,
  ): Chunk[Part] =
    Chunk(
      Part.Plain(s"${named(module, column)} ${version.value} ${token(platform)} $suite "),
      Part.Mark("cached", Role.Cached),
    )

  /** `column` of 0 leaves the name as it is. A wider column pads on the right. */
  private def named(name: ProjectName, column: Int): String =
    val raw   = name.value
    val extra = column - raw.length
    if extra > 0 then raw + (" " * extra) else raw

  private def marked(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      text: String,
      role: Role,
      column: Int,
  ): Chunk[Part] =
    val id = begin(module, version, platform, column)
    if text.isEmpty then Chunk(Part.Plain(id))
    else Chunk(Part.Plain(s"$id "), Part.Mark(text, role))

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
