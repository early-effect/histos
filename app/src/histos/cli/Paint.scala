package histos.cli

import zio.UIO

/** Whether status words take colour. */
enum Ink:
  case Colour
  case Plain

/** Where standard output goes. A pipe and a file are both redirected. */
enum Stdout:
  case Terminal
  case Redirected

/** Color on status words. Plain text stays plain. A pipe, a file, and a non-empty `NO_COLOR` stay plain.
  */
object Paint:
  private val Reset  = "\u001b[0m"
  private val Green  = "\u001b[32m"
  private val Red    = "\u001b[31m"
  private val Yellow = "\u001b[33m"
  private val Dim    = "\u001b[2m"
  private val Bold   = "\u001b[1m"

  def paint(line: String, ink: Ink): String =
    ink match
      case Ink.Plain                 => line
      case Ink.Colour if total(line) => bold(line)
      case Ink.Colour                => words(line)

  /** no-color.org: a present, non-empty `NO_COLOR` turns colour off. An empty one does not. */
  def ink(stdout: Stdout, noColor: Option[String]): Ink =
    (stdout, noColor) match
      case (Stdout.Redirected, _)                       => Ink.Plain
      case (Stdout.Terminal, Some(set)) if set.nonEmpty => Ink.Plain
      case (Stdout.Terminal, _)                         => Ink.Colour

  /** `NO_COLOR` is read through `zio.System`. */
  def chosen(stdout: Stdout): UIO[Ink] =
    zio.System.env("NO_COLOR").orElseSucceed(None).map(ink(stdout, _))

  def render(line: String): UIO[String] =
    chosen(terminal).map(paint(line, _))

  private def words(line: String): String =
    val painted = List(
      (": error:", s": ${Red}error${Reset}:"),
      (": warn:", s": ${Yellow}warn${Reset}:"),
      ("skipped unchanged", s"${Dim}skipped unchanged${Reset}"),
      (" passed", s" ${Green}passed${Reset}"),
      (" failed", s" ${Red}failed${Reset}"),
    )
    painted.foldLeft(line) { case (text, (from, to)) => text.replace(from, to) }

  private def bold(line: String): String =
    s"$Bold$line$Reset"

  private def total(line: String): Boolean =
    line.startsWith("files compiled:") ||
      line.startsWith("test files compiled:") ||
      line.startsWith("tests passed:") ||
      line.startsWith("modules:")

  private lazy val terminal: Stdout =
    try
      val probe = new ProcessBuilder("/bin/sh", "-c", "test -t 1")
        .redirectOutput(ProcessBuilder.Redirect.INHERIT)
        .redirectError(ProcessBuilder.Redirect.INHERIT)
        .start()
      if probe.waitFor() == 0 then Stdout.Terminal else Stdout.Redirected
    catch case _: java.io.IOException => Stdout.Redirected
end Paint
