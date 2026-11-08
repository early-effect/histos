package histos.cli

import zio.test.*

object PaintLawSpec extends ZIOSpecDefault:
  private val Bold   = "\u001b[1m"
  private val Reset  = "\u001b[0m"
  private val Red    = "\u001b[31m"
  private val Yellow = "\u001b[33m"
  private val Green  = "\u001b[32m"
  private val Dim    = "\u001b[2m"

  def spec = suite("paint laws")(
    test("a pipe or NO_COLOR leaves the line unchanged"):
      val line = "files compiled: 3"
      assertTrue(
        Paint.paint(line, tty = false, None) == line,
        Paint.paint(line, tty = true, Some("")) == line,
        Paint.paint("hello", tty = true, None) == "hello",
      )
    ,
    test("a total line is bold and a status word takes its color"):
      val totals = List("files compiled: 1", "test files compiled: 1", "tests passed: 1", "modules: 1")
      assertTrue(
        totals.forall(line => Paint.paint(line, tty = true, None) == s"$Bold$line$Reset"),
        Paint.paint("api: error: boom", tty = true, None) == s"api: ${Red}error${Reset}: boom",
        Paint.paint("api: warn: boom", tty = true, None) == s"api: ${Yellow}warn${Reset}: boom",
        Paint.paint("skipped unchanged", tty = true, None) == s"${Dim}skipped unchanged${Reset}",
        Paint.paint("1 passed", tty = true, None) == s"1 ${Green}passed${Reset}",
        Paint.paint("1 failed", tty = true, None) == s"1 ${Red}failed${Reset}",
      ),
  )
end PaintLawSpec
