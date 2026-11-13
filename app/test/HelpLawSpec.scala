package histos.cli

import histos.doctor.Suggest

import zio.test.*

object HelpLawSpec extends ZIOSpecDefault:
  def spec = suite("help laws")(
    test("every command name renders text that contains that name"):
      val bad = Help.names.flatMap: name =>
        Help.render(List(name)) match
          case Right(text) if text.contains(name) => Nil
          case other                              => List(s"$name -> $other")
      val release = Help.render(List("publish", "release"))
      val grouped = Help.grouped
      val roots   = Help.entries.map: entry =>
        entry.name.split(" ").toList match
          case word :: _ => word
          case Nil       => ""
      assertTrue(
        bad == Nil,
        release match
          case Right(text) => text.contains("publish release")
          case Left(_)     => false
        ,
        Help.names.forall(grouped.contains),
        roots.forall(word => word == "~" || Help.names.contains(word)),
      )
    ,
    test("an unknown command names the near commands and an empty token does not"):
      assertTrue(
        Help.render(Nil) == Right(Help.grouped),
        Help.unknown("", Help.names) == "unknown command 'command'\nhistos help\nhistos help <command>",
        Help.unknown("compil", List("compile")) ==
          "unknown command 'compil'\ndid you mean compile?\nhistos help\nhistos help compile",
        Help.unknown("compil", List("compile", "compiles")) ==
          "unknown command 'compil'\ndid you mean compile or compiles?\nhistos help\nhistos help compile",
        Help.render(List("nope")) == Left(HelpError.Unknown("nope")),
        textOf(Help.render(List("nope"))) == "unknown command 'nope'\nhistos help\nhistos help <command>",
        Help.render(List("compil")) == Left(HelpError.Unknown("compil")),
        textOf(Help.render(List("compil"))) ==
          "unknown command 'compil'\ndid you mean compile?\nhistos help\nhistos help compile",
        Help.render(List("server")) == Left(HelpError.Unknown("server")),
      )
    ,
    test("a flag names the closest flag, and an exact name is not a suggestion"):
      assertTrue(
        Help.flag("compile", "--chek") == "compile does not take --chek\ndid you mean --check?",
        Help.flag("compile", "--bogus") == "compile does not take --bogus\nhistos help compile",
        Suggest.near("", List("compile")) == Nil,
        Suggest.near("compile", List("compile")) == Nil,
        Suggest.near("compil", List("compile", "clean")) == List("compile"),
        Suggest.near("aa", List("ab", "ac", "ad", "ae", "aa")) == List("ab", "ac", "ad"),
        Suggest.closest("compil", List("compile")) == Some("compile"),
        Suggest.closest("zzzz", List("compile")) == None,
      ),
  )

  private def textOf(found: Either[HelpError, String]): String =
    found.fold(_.message, identity)
end HelpLawSpec
