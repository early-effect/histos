package histos.cli

import histos.testrun.{SuitePattern, TagName, TagNameError, TestFilter, TestMode, TestTerm, TestTermError}
import histos.{Platform, PlatformSelection, ProjectName, ScalaSelection, ScalaToken, SelectionError}
import histos.suite.HistosSpec

import zio.test.*
import zio.ZIO
import zio.json.*

object TestGrammarSpec extends HistosSpec:
  private val selected = TestFilter(
    tests = List(TestTerm("one"), TestTerm("two")),
    tags = List(TagName("slow")),
    ignoredTags = List(TagName("flaky")),
  )

  private val asked = Command.Test(
    List(ProjectName("api")),
    AffectedChoice.Unset,
    TestMode.Quick,
    List(SuitePattern("*Grammar*")),
    selected,
  )

  def spec = suite("test selection")(
    test("another flag owns its selector-looking value") {
      val cases = List(
        "--test"       -> CliParseError.TestTerm(TestTermError.Flag("--platform")),
        "--tag"        -> CliParseError.TagName(TagNameError.Flag("--platform")),
        "--ignore-tag" -> CliParseError.TagName(TagNameError.Flag("--platform")),
        "--only"       -> CliParseError.Pattern("test --only needs a pattern"),
      )
      ZIO
        .foreach(cases) { (flag, expected) =>
          Grammar.parse(List("test", flag, "--platform", "jvm", "api")).either.map(_ == Left(expected))
        }
        .map(results => assertTrue(results.forall(identity)))
    },
    test("suite eligibility remains typed and distinct from explicit named targets") {
      for
        parsed <- Grammar.parse(
          List("test", "--suite", "[\"api\",\"web\"]", "--platform", "jvm", "--affected", "[\"web\"]")
        )
        call    <- Operation.call(parsed)
        invalid <- Grammar.parse(List("test", "api", "--suite", "[\"api\"]")).either
      yield assertTrue(
        call == histos.server.Call.Test(
          histos.TestSelection(
            histos.TestTargets.Filtered(List(ProjectName("api"), ProjectName("web"))),
            histos.PlatformSelection.only(histos.Jvm),
            Some(List(ProjectName("web"))),
          ),
          Nil,
          TestMode.Run,
          TestFilter.empty,
        ),
        invalid == Left(CliParseError.Pattern("test --suite cannot be combined with named projects")),
        call.toJson.fromJson[histos.server.Call] == Right(call),
      )
    },
    test("selector-looking values fail identically before and after valid selectors, including preview") {
      val prefixes = List(List("test"), List("preview", "test"))
      val checked  = ZIO.foreach(prefixes): prefix =>
        ZIO.foreach(List("--platform", "--main")): value =>
          val malformed = prefix ++ List("--test", value, "jvm", "api")
          for
            first   <- Grammar.parse(malformed).either
            earlier <- Grammar.parse(prefix ++ List("--platform", "jvm") ++ List("--test", value, "api")).either
          yield first == Left(CliParseError.TestTerm(TestTermError.Flag(value))) && earlier == first
      checked.map(results => assertTrue(results.flatten.forall(identity)))
    },
    test("platform unions and comma patterns are accepted by the actual grammar") {
      Grammar
        .parse(List("test", "api", "--platform", "jvm,js", "--only", "*Events*,*Protocol*"))
        .either
        .map: parsed =>
          assertTrue(
            parsed == Right(
              Command.Test(
                List(ProjectName("api")),
                AffectedChoice.Unset,
                TestMode.Run,
                List(SuitePattern("*Events*"), SuitePattern("*Protocol*")),
                TestFilter.empty,
                histos.PlatformSelection.only(histos.Platform.Jvm, histos.Platform.Js),
              )
            )
          )
    },
    test("all text lists share delimiter escaping and first encounter union semantics") {
      Grammar
        .parse(
          List(
            "test",
            "--test",
            "one,two",
            "api",
            "--only",
            "*One*,*Two*",
            "--only",
            "*One*",
            "--test",
            "one",
            "--tag",
            "slow,fast",
            "--ignore-tag",
            "flaky,broken",
            "--test",
            "literal\\,comma,path\\\\name",
          )
        )
        .either
        .map: parsed =>
          assertTrue(
            parsed == Right(
              Command.Test(
                List(ProjectName("api")),
                AffectedChoice.Unset,
                TestMode.Run,
                List(SuitePattern("*One*"), SuitePattern("*Two*")),
                TestFilter(
                  List(TestTerm("one"), TestTerm("two"), TestTerm("literal,comma"), TestTerm("path\\name")),
                  List(TagName("slow"), TagName("fast")),
                  List(TagName("flaky"), TagName("broken")),
                ),
              )
            )
          )
    },
    test("malformed lists refuse through the shared typed parser") {
      val flags  = List("--only", "--test", "--tag", "--ignore-tag")
      val values = List("", ",one", "one,", "one,,two", "one\\", "one\\x")
      ZIO
        .foreach(flags.flatMap(flag => values.map(flag -> _))) { (flag, value) =>
          Grammar
            .parse(List("test", flag, value))
            .either
            .map(parsed => parsed == histos.TextList.parse(List(value)).left.map(CliParseError.TextList(_)))
        }
        .map(results => assertTrue(results.forall(identity)))
    },
    test("structured affected JSON is not a text list and can follow selectors") {
      Grammar
        .parse(List("test", "--platform", "jvm", "--only", "*Spec*", "--affected", "[\"api\",\"wire\"]", "api"))
        .either
        .map: parsed =>
          assertTrue(
            parsed == Right(
              Command.Test(
                List(ProjectName("api")),
                AffectedChoice.Only(List(ProjectName("api"), ProjectName("wire"))),
                TestMode.Run,
                List(SuitePattern("*Spec*")),
                TestFilter.empty,
                histos.PlatformSelection.only(histos.Platform.Jvm),
              )
            )
          )
    },
    test("compile and preview share platform unions, without consuming project tokens") {
      for
        compile <- Grammar.parse(List("compile", "--platform", "jvm", "api", "--platform", "js,jvm")).either
        preview <- Grammar.parse(List("preview", "compile", "api", "--platform", "jvm,js")).either
      yield assertTrue(
        compile == Right(
          Command
            .Compile(List(ProjectName("api")), histos.PlatformSelection.only(histos.Platform.Jvm, histos.Platform.Js))
        ),
        preview == Right(
          Command.PreviewCompile(
            List(ProjectName("api")),
            histos.PlatformSelection.only(histos.Platform.Jvm, histos.Platform.Js),
          )
        ),
      )
    },
    test("scala series, a comma list, a run of two tokens, and non-jvm coverage are typed") {
      val series = ScalaSelection.Only(ScalaToken.Series2(13), Nil)
      val both   = ScalaSelection.Only(ScalaToken.Series2(13), List(ScalaToken.Series3))
      for
        tested   <- Grammar.parse(List("test", "core", "--scala", "2.13", "--platform", "js")).either
        compiled <- Grammar.parse(List("compile", "core", "--scala", "2.13,3")).either
        refused  <- Grammar.parse(List("run", "app", "--scala", "2.13,3")).either
        missing  <- Grammar.parse(List("compile", "--scala")).either
        coverage <- Grammar.parse(List("coverage", "--platform", "js")).either
        measured <- Grammar.parse(List("coverage", "core", "--platform", "jvm", "--scala", "3")).either
      yield assertTrue(
        tested == Right(
          Command.Test(
            List(ProjectName("core")),
            AffectedChoice.Unset,
            TestMode.Run,
            Nil,
            TestFilter.empty,
            PlatformSelection.only(Platform.Js),
            series,
          )
        ),
        compiled == Right(Command.Compile(List(ProjectName("core")), scala = both)),
        refused == Left(
          CliParseError.Selection(SelectionError.RunScalaMany(List(ScalaToken.Series2(13), ScalaToken.Series3)))
        ),
        missing == Left(CliParseError.Pattern("--scala needs a Scala version")),
        coverage == Left(CliParseError.Pattern("coverage measures the JVM only")),
        measured == Right(
          Command.Coverage(
            List(ProjectName("core")),
            histos.CoverageFloors(),
            histos.coverage.Excludes(Nil, Nil),
            ScalaSelection.Only(ScalaToken.Series3, Nil),
          )
        ),
      )
    },
    test("delimited and repeated list syntax obey the same law") {
      check(Gen.listOfBounded(1, 8)(Gen.elements("one", "two", "three"))): entries =>
        for
          repeated  <- Grammar.parse("test" :: entries.flatMap(value => List("--test", value))).either
          delimited <- Grammar.parse(List("test", "--test", entries.mkString(","))).either
        yield assertTrue(repeated == delimited)
    },
    test("flags in any order keep each value in the order it appeared") {
      val forward = List(
        "test",
        "--tag",
        "slow",
        "api",
        "--quick",
        "--test",
        "one",
        "--ignore-tag",
        "flaky",
        "--test",
        "two",
        "--only",
        "*Grammar*",
      )
      val reverse = List(
        "test",
        "api",
        "--only",
        "*Grammar*",
        "--test",
        "two",
        "--ignore-tag",
        "flaky",
        "--quick",
        "--test",
        "one",
        "--tag",
        "slow",
      )
      val reversedFilter = TestFilter(
        tests = List(TestTerm("two"), TestTerm("one")),
        tags = List(TagName("slow")),
        ignoredTags = List(TagName("flaky")),
      )
      val reversed = Command.Test(
        List(ProjectName("api")),
        AffectedChoice.Unset,
        TestMode.Quick,
        List(SuitePattern("*Grammar*")),
        reversedFilter,
      )
      for
        first  <- Grammar.parse(forward).either
        second <- Grammar.parse(reverse).either
      yield assertTrue(
        first == Right(asked),
        second == Right(reversed),
        selected.argv == List("-t", "one", "-t", "two", "-tags", "slow", "-ignore-tags", "flaky"),
        reversedFilter.argv == List("-t", "two", "-t", "one", "-tags", "slow", "-ignore-tags", "flaky"),
      )
    },
    test("a test term is not an only pattern") {
      for
        kept    <- Grammar.parse(List("test", "--only", "*Grammar*", "--test", "one")).either
        missing <- Grammar.parse(List("test", "--only", "--test", "one")).either
      yield assertTrue(
        kept == Right(
          Command.Test(
            Nil,
            AffectedChoice.Unset,
            TestMode.Run,
            List(SuitePattern("*Grammar*")),
            TestFilter(List(TestTerm("one")), Nil, Nil),
          )
        ),
        missing == Left(CliParseError.Pattern("test --only needs a pattern")),
      )
    },
    test("a missing value fails") {
      for
        term <- Grammar.parse(List("test", "--test")).either
        tag  <- Grammar.parse(List("test", "--tag")).either
        drop <- Grammar.parse(List("test", "--ignore-tag")).either
      yield assertTrue(
        term == Left(CliParseError.Pattern("test --test needs a term")),
        tag == Left(CliParseError.Pattern("test --tag needs a name")),
        drop == Left(CliParseError.Pattern("test --ignore-tag needs a name")),
      )
    },
    test("a value that is empty, blank, or a flag fails") {
      for
        dashed <- Grammar.parse(List("test", "--test", "--quick")).either
        blank  <- Grammar.parse(List("test", "--tag", "a b")).either
        empty  <- Grammar.parse(List("test", "--ignore-tag", "")).either
        again  <- Grammar.parse(List("test", "--quick", "--quick")).either
      yield assertTrue(
        dashed == Left(CliParseError.TestTerm(TestTermError.Flag("--quick"))),
        blank == Left(CliParseError.TagName(TagNameError.Blank("a b"))),
        empty == Left(CliParseError.TextList(histos.TextListError.EmptyEntry(""))),
        again == Left(CliParseError.Pattern("test --quick is already set")),
      )
    },
    test("preview parses the filter and does not treat the term as a suite pattern") {
      for parsed <- Grammar.parse(List("preview", "test", "--test", "one", "--only", "*Grammar*")).either
      yield assertTrue(
        parsed == Right(
          Command.PreviewTest(
            Nil,
            AffectedChoice.Unset,
            TestMode.Run,
            List(SuitePattern("*Grammar*")),
            TestFilter(List(TestTerm("one")), Nil, Nil),
          )
        )
      )
    },
  )
end TestGrammarSpec
