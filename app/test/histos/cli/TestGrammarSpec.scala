package histos.cli

import histos.testrun.{SuitePattern, TagName, TagNameError, TestFilter, TestMode, TestTerm, TestTermError}
import histos.ProjectName

import zio.test.*

object TestGrammarSpec extends ZIOSpecDefault:
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
        empty == Left(CliParseError.TagName(TagNameError.Empty)),
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
