package histos.cli

import histos.build.BuildEvent
import histos.report.WireJson.given
import histos.suite.HistosSpec
import histos.theme.{Color, Emphasis, Role, Style, Theme}

import java.nio.file.Path

import zio.Chunk
import zio.json.*
import zio.test.*

object PaintSpec extends HistosSpec:
  private val repo  = Path.of("/repo")
  private val blue  = Style(Color.Blue, Emphasis.Plain)
  private val loud  = Role.values.foldLeft(Theme.empty)((theme, role) => theme.set(role, blue))
  private val color = Appearance(Ink.Colour, Depth.Indexed)
  private val plain = Appearance.plain

  def spec = suite("paint")(
    test("a theme replaces the role and a plain ink writes no escape") {
      val theme = Theme.standard.set(Role.Passed, blue)
      val parts = Chunk(Part.Plain("api "), Part.Mark("passed", Role.Passed))
      val shown = Paint.paint(parts, theme, color)
      assertTrue(
        shown == "api \u001b[34mpassed\u001b[0m",
        !shown.contains("\u001b[32m"),
        Paint.paint(parts, theme, plain) == "api passed",
        Paint.paint(
          Chunk(Part.Mark("x", Role.Error)),
          Theme.empty.set(Role.Error, Style(Color.Index(999), Emphasis.Plain)),
          Appearance(Ink.Colour, Depth.Truecolor),
        ) == "x",
      )
    },
    test("rgb follows the depth and a named color does not") {
      val mark  = Chunk(Part.Mark("failed", Role.Failed))
      val rgb   = Theme.empty.set(Role.Failed, Style(Color.rgb(255, 0, 0), Emphasis.Plain))
      val named = Theme.empty.set(Role.Failed, Style(Color.Red, Emphasis.Bold))
      val dim   = Theme.empty.set(Role.Unchanged, Style(Color.Cyan, Emphasis.Dim))
      assertTrue(
        Paint.paint(mark, rgb, Appearance(Ink.Colour, Depth.Truecolor)) == "\u001b[38;2;255;0;0mfailed\u001b[0m",
        Paint.paint(mark, rgb, color) == "\u001b[38;5;196mfailed\u001b[0m",
        Paint.paint(mark, named, Appearance(Ink.Colour, Depth.Truecolor)) == "\u001b[1;31mfailed\u001b[0m",
        Paint.paint(mark, named, color) == "\u001b[1;31mfailed\u001b[0m",
        Paint.paint(Chunk(Part.Mark("same", Role.Unchanged)), dim, color) == "\u001b[2;36msame\u001b[0m",
        Paint.indexOf(255, 0, 0) == 196,
        Paint.indexOf(0, 0, 0) == 16,
        Paint.indexOf(8, 8, 8) == 232,
      )
    },
    test("depth follows the flag, then COLORTERM") {
      assertTrue(
        Paint.depth(TruecolorChoice.On, None) == Depth.Truecolor,
        Paint.depth(TruecolorChoice.On, Some("dumb")) == Depth.Truecolor,
        Paint.depth(TruecolorChoice.Off, Some("truecolor")) == Depth.Indexed,
        Paint.depth(TruecolorChoice.Default, Some("truecolor")) == Depth.Truecolor,
        Paint.depth(TruecolorChoice.Default, Some("24bit")) == Depth.Truecolor,
        Paint.depth(TruecolorChoice.Default, Some("")) == Depth.Indexed,
        Paint.depth(TruecolorChoice.Default, Some("yes")) == Depth.Indexed,
        Paint.depth(TruecolorChoice.Default, None) == Depth.Indexed,
      )
    },
    test("a flag beats NO_COLOR and NO_COLOR beats a terminal") {
      assertTrue(
        Paint.decide(ColorChoice.On, false, Some("1")) == Ink.Colour,
        Paint.decide(ColorChoice.Default, true, Some("1")) == Ink.Plain,
        Paint.decide(ColorChoice.Default, false, None) == Ink.Plain,
        Paint.decide(ColorChoice.On, false, None) == Ink.Colour,
        Paint.decide(ColorChoice.Default, true, Some("")) == Ink.Colour,
        Paint.decide(ColorChoice.Default, true, None) == Ink.Colour,
        Paint.decide(ColorChoice.Off, true, None) == Ink.Plain,
      )
    },
    test("color flags peel off and the last one wins") {
      val after  = ColorFlag.peel(List("compile", "--no-color"))
      val before = ColorFlag.peel(List("--color", "test", "--no-color"))
      val watch  = ColorFlag.peel(List("~", "--no-color", "test"))
      val depth  = ColorFlag.peel(List("--truecolor", "compile", "--no-truecolor"))
      val phases = ColorFlag.peel(List("--verbose", "compile", "app"))
      val arg    = ColorFlag.peel(List("run", "app", "--", "--verbose"))
      assertTrue(
        after == Peeled(List("compile"), ColorChoice.Off, TruecolorChoice.Default, CompileLog.Units),
        before == Peeled(List("test"), ColorChoice.Off, TruecolorChoice.Default, CompileLog.Units),
        watch == Peeled(List("~", "test"), ColorChoice.Off, TruecolorChoice.Default, CompileLog.Units),
        depth == Peeled(List("compile"), ColorChoice.Default, TruecolorChoice.Off, CompileLog.Units),
        phases == Peeled(List("compile", "app"), ColorChoice.Default, TruecolorChoice.Default, CompileLog.Phases),
        arg == Peeled(
          List("run", "app", "--", "--verbose"),
          ColorChoice.Default,
          TruecolorChoice.Default,
          CompileLog.Units,
        ),
      )
    },
    test("a status takes its role and an out line stays plain") {
      val tally = Show.colored(repo, Chunk(BuildEvent.Status("files compiled: 1", Role.TallyOk)), Depth.Indexed)
      val bad   = Show.colored(repo, Chunk(BuildEvent.Status("failed: 1", Role.TallyBad)), Depth.Indexed)
      val out   = Show.colored(
        repo,
        Chunk(BuildEvent.Theme(loud), BuildEvent.Out("hello")),
        Depth.Truecolor,
      )
      val told = Show.transcript(repo, Chunk(BuildEvent.Status("files compiled: 1", Role.TallyOk)))
      assertTrue(
        tally._1 == Chunk("\u001b[1;32mfiles compiled: 1\u001b[0m"),
        bad._1 == Chunk("\u001b[1;31mfailed: 1\u001b[0m"),
        out._1 == Chunk("hello"),
        !out._1.mkString.contains("\u001b"),
        told._1 == Chunk("files compiled: 1"),
      )
    },
    test("a theme event and a status round trip") {
      val theme  = BuildEvent.Theme(Theme.standard)
      val status = BuildEvent.Status("files compiled: 1", Role.TallyOk)
      assertTrue(
        theme.toJson.fromJson[BuildEvent] == Right(theme),
        status.toJson.fromJson[BuildEvent] == Right(status),
      )
    },
  )
end PaintSpec
