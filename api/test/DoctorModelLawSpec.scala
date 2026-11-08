package histos.doctor

import histos.plugin.RepoPath

import zio.test.*

object DoctorModelLawSpec extends ZIOSpecDefault:
  def spec = suite("doctor model laws")(
    test("a check id is two or more lowercase segments"):
      assertTrue(
        CheckId.from("demo.check-id").map(_.value) == Right("demo.check-id"),
        CheckId.from("a.b.c") == Right(CheckId.trusted("a.b.c")),
        CheckId.from("") == Left(CheckIdError.Empty),
        CheckId.from("demo") == Left(CheckIdError.Bad("demo")),
        CheckId.from("Demo.check") == Left(CheckIdError.Bad("Demo.check")),
        CheckId.from("demo.") == Left(CheckIdError.Bad("demo.")),
        CheckId.from(".demo") == Left(CheckIdError.Bad(".demo")),
      )
    ,
    test("a shell line is one non-empty command"):
      assertTrue(
        ShellLine.from("histos help").map(_.value) == Right("histos help"),
        ShellLine.from("") == Left(ShellLineError.Bad("")),
        ShellLine.from("a\nb") == Left(ShellLineError.Bad("a\nb")),
      )
    ,
    test("fix mode prints unless fix is set, and yes drops the github setting"):
      val modes = List(
        (false, false, FixMode.Print),
        (false, true, FixMode.Print),
        (true, true, FixMode.Apply(Set(FixKind.LocalEdit, FixKind.HistosCommand))),
        (true, false, FixMode.Apply(Set(FixKind.LocalEdit, FixKind.HistosCommand, FixKind.GitHubSetting))),
      )
      assertTrue(modes.forall((fix, yes, expected) => FixMode.from(fix, yes) == expected))
    ,
    test("a noted error is that check, and a bad path does not invent a location"):
      val command = Fix.Command(ShellLine.trusted("histos help"))
      val noted   = Noted.error("demo.check", "summary", "why", command)
      val placed  = Noted.at(noted, "build.scala", Some(4))
      val missed  = Noted.at(noted, "..", Some(4))
      val carried = Noted.carry("demo.check", "summary", "why", "histos help")
      assertTrue(
        noted.level == Level.Error,
        noted.check.value == "demo.check",
        noted.title == noted.summary,
        placed.location == Some(Location(RepoPath("build.scala"), Some(4))),
        missed.location.isEmpty,
        carried.fixes match
          case Fix.Command(line) :: _ => line.value == "histos help"
          case _                      => false,
      )
    ,
    test("fault text keeps the summary, one location, and only the first fix"):
      val first  = Fix.Command(ShellLine.trusted("histos help"))
      val second = Fix.Settings(histos.HttpUrl.trusted("https://example.com/second-fix"), "second-fix")
      val same   = Noted.error("demo.check", "summary", "summary", first, second)
      val longer = Noted.error("demo.check", "summary", "why", first)
      val placed = Noted.at(longer, "build.scala", Some(4))
      val file   = Noted.at(longer, "build.scala", None)
      val steps  = Noted.error("demo.check", "summary", "why", Noted.steps("read the log", "then rerun"))
      val config = Noted.error(
        "demo.check",
        "summary",
        "why",
        Fix.Config(RepoPath("build.scala"), Edit("before", "after")),
      )
      assertTrue(
        FaultText.show(same) == "summary\nfix  histos help",
        !FaultText.show(same).contains("second-fix"),
        FaultText.show(longer) == "summary\nwhy\nfix  histos help",
        FaultText.show(placed) == "summary\nwhy\nbuild.scala:4\nfix  histos help",
        FaultText.show(file) == "summary\nwhy\nbuild.scala\nfix  histos help",
        FaultText.show(steps) == "summary\nwhy\nfix  read the log",
        FaultText.show(config) == "summary\nwhy\nfix  build.scala",
      )
    ,
    test("a suggestion is within the limit, is not the typed name, and keeps the closest three"):
      assertTrue(
        Suggest.near("", List("core")).isEmpty,
        Suggest.near("core", List("core", "Core")).isEmpty,
        Suggest.near("cort", List("core")) == List("core"),
        Suggest.near("xxxx", List("core"), 2).isEmpty,
        Suggest.near("a", List("abcd"), 2).isEmpty,
        Suggest.near("b", List("bbb", "cb", "ab", "a")) == List("a", "ab", "cb"),
        Suggest.closest("b", List("bbb", "cb", "ab", "a")) == Some("a"),
        Suggest.closest("zzzz", List("core")).isEmpty,
        Suggest.runs("docc", Nil) == List("doc"),
        Suggest.binary("dev.zio", "zio_3", "2").isEmpty,
        Suggest.binary("dev.zio", "zio", "2").exists(_.contains("dev.zio::zio:2")),
      ),
  )
