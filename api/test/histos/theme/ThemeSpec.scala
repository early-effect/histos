package histos.theme

import histos.*
import histos.suite.HistosSpec

import _root_.scala.compiletime.testing.typeChecks

import zio.test.*

object ThemeSpec extends HistosSpec:
  private val blue = Style(Color.Blue, Emphasis.Plain)

  def spec = suite("a theme")(
    test("a set role wins and a gap keeps the base") {
      val passed = Theme.standard.set(Role.Passed, blue)
      val warn   = Theme.empty.set(Role.Warn, Style(Color.Yellow, Emphasis.Plain))
      assertTrue(
        passed(Role.Passed) == Some(blue),
        passed(Role.Failed) == Theme.standard(Role.Failed),
        Theme.empty.orElse(Theme.standard) == Theme.standard,
        warn.orElse(Theme.standard)(Role.Warn) == Some(Style(Color.Yellow, Emphasis.Plain)),
        warn.orElse(Theme.standard)(Role.Passed) == Theme.standard(Role.Passed),
      )
    },
    test("an index or a channel outside 0 to 255 is rejected") {
      assertTrue(
        typeChecks("histos.theme.Color.indexed(0)"),
        typeChecks("histos.theme.Color.indexed(255)"),
        !typeChecks("histos.theme.Color.indexed(256)"),
        !typeChecks("histos.theme.Color.indexed(-1)"),
        typeChecks("histos.theme.Color.rgb(255, 0, 0)"),
        !typeChecks("histos.theme.Color.rgb(256, 0, 0)"),
        !typeChecks("histos.theme.Color.rgb(0, -1, 0)"),
        !typeChecks("histos.theme.Color.rgb(0, 0, 256)"),
        Color.fromIndex(256) == Left(ColorError.Index(256)),
        Color.fromIndex(-1) == Left(ColorError.Index(-1)),
        Color.fromIndex(196) == Right(Color.Index(196)),
        Color.fromRgb(256, 0, 0) == Left(ColorError.Channel(256)),
        Color.fromRgb(0, -1, 0) == Left(ColorError.Channel(-1)),
        Color.fromRgb(255, 0, 0) == Right(Color.Rgb(255, 0, 0)),
      )
    },
    test("a build opts into the theme it names") {
      val custom = Theme.standard.set(Role.Passed, blue)
      val built  = Projects.theme(custom)(module("api", Jvm)(scala"3.10.1"))
      assertTrue(built.map(_.theme) == Right(custom))
    },
  )
end ThemeSpec
