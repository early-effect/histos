package histos.cli

import zio.ZLayer
import zio.test.*

object PaintLawSpec extends ZIOSpecDefault:
  private val Bold   = "\u001b[1m"
  private val Reset  = "\u001b[0m"
  private val Red    = "\u001b[31m"
  private val Yellow = "\u001b[33m"
  private val Green  = "\u001b[32m"
  private val Dim    = "\u001b[2m"

  def spec = suite("paint laws")(
    test("a redirect or a non-empty NO_COLOR is plain, and an empty NO_COLOR keeps colour"):
      val rows = List(
        (Attached.Terminal, None)        -> Ink.Colour,
        (Attached.Terminal, Some(""))    -> Ink.Colour,
        (Attached.Terminal, Some("1"))   -> Ink.Plain,
        (Attached.Terminal, Some(" "))   -> Ink.Plain,
        (Attached.Redirected, None)      -> Ink.Plain,
        (Attached.Redirected, Some(""))  -> Ink.Plain,
        (Attached.Redirected, Some("1")) -> Ink.Plain,
      )
      val misses = rows.collect {
        case ((output, noColor), want) if Paint.ink(output, noColor) != want => (output, noColor)
      }
      assertTrue(misses == Nil)
    ,
    test("NO_COLOR is read through zio.System"):
      for
        unset <- Paint.chosen(Attached.Terminal)
        _     <- TestSystem.putEnv("NO_COLOR", "")
        empty <- Paint.chosen(Attached.Terminal)
        _     <- TestSystem.putEnv("NO_COLOR", "1")
        set   <- Paint.chosen(Attached.Terminal)
      yield assertTrue(unset == Ink.Colour, empty == Ink.Colour, set == Ink.Plain)
    ,
    test("render paints for the output of the Stdio it is given, whatever the input is"):
      val line = "1 passed"
      for
        shown <- Paint.render(line).provideLayer(ZLayer.succeed(Stdio(Attached.Redirected, Attached.Terminal)))
        piped <- Paint.render(line).provideLayer(ZLayer.succeed(Stdio(Attached.Terminal, Attached.Redirected)))
      yield assertTrue(shown == s"1 ${Green}passed${Reset}", piped == line)
    ,
    test("plain leaves the line, a total line is bold, and a status word takes its color"):
      val totals = List("files compiled: 1", "test files compiled: 1", "tests passed: 1", "modules: 1")
      val misses = totals.filter(line => Paint.paint(line, Ink.Colour) != s"$Bold$line$Reset")
      assertTrue(
        misses == Nil,
        Paint.paint("files compiled: 3", Ink.Plain) == "files compiled: 3",
        Paint.paint("hello", Ink.Colour) == "hello",
        Paint.paint("api: error: boom", Ink.Colour) == s"api: ${Red}error${Reset}: boom",
        Paint.paint("api: warn: boom", Ink.Colour) == s"api: ${Yellow}warn${Reset}: boom",
        Paint.paint("skipped unchanged", Ink.Colour) == s"${Dim}skipped unchanged${Reset}",
        Paint.paint("1 passed", Ink.Colour) == s"1 ${Green}passed${Reset}",
        Paint.paint("1 failed", Ink.Colour) == s"1 ${Red}failed${Reset}",
      ),
  )
end PaintLawSpec
