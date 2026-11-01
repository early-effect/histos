package histos.cli

import zio.{UIO, ZIO}

/** Color on status words. Plain text stays plain. A pipe, a file, and `NO_COLOR` stay plain.
  */
object Paint:
  private val Reset  = "\u001b[0m"
  private val Green  = "\u001b[32m"
  private val Red    = "\u001b[31m"
  private val Yellow = "\u001b[33m"
  private val Dim    = "\u001b[2m"
  private val Bold   = "\u001b[1m"

  def paint(line: String, tty: Boolean, noColor: Option[String]): String =
    if !tty || noColor.isDefined then line
    else if total(line) then bold(line)
    else words(line)

  def render(line: String): UIO[String] =
    ZIO.succeed(paint(line, terminal, Option(System.getenv("NO_COLOR"))))

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

  private lazy val terminal: Boolean =
    try
      val probe = new ProcessBuilder("/bin/sh", "-c", "test -t 1")
        .redirectOutput(ProcessBuilder.Redirect.INHERIT)
        .redirectError(ProcessBuilder.Redirect.INHERIT)
        .start()
      probe.waitFor() == 0
    catch case _: java.io.IOException => false
end Paint
