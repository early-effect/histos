package histos.testrun

import zio.test.*

object RunnerArgSpec extends ZIOSpecDefault:
  def spec = suite("runner arguments")(
    test("terms, tags, and ignored tags are one argv") {
      val filter = TestFilter(
        tests = List(TestTerm("one"), TestTerm("two")),
        tags = List(TagName("slow")),
        ignoredTags = List(TagName("flaky")),
      )
      val argv    = TestFilter.argv(filter)
      val env     = TestFilter.environment(filter)
      val indexed = argv.zipWithIndex.map: (arg, index) =>
        env.get(s"${TestFilter.ArgPrefix}$index")
      assertTrue(
        argv == List("-t", "one", "-t", "two", "-tags", "slow", "-ignore-tags", "flaky"),
        env.get(TestFilter.ArgCount).contains(argv.length.toString),
        indexed == argv.map(Some(_)),
      )
    },
    test("empty, blank, and a leading dash are refused") {
      assertTrue(
        TestTerm.from("") == Left(TestTermError.Empty),
        TestTerm.from("a b") == Left(TestTermError.Blank("a b")),
        TestTerm.from("-t") == Left(TestTermError.Flag("-t")),
        TagName.from("") == Left(TagNameError.Empty),
        TagName.from("a b") == Left(TagNameError.Blank("a b")),
        TagName.from("-slow") == Left(TagNameError.Flag("-slow")),
      )
    },
  )
