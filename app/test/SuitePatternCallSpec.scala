package histos.cli

import histos.preview.PreviewError
import histos.server.Calls
import histos.testrun.{ModuleTestError, SuitePatternError}

import zio.test.*

object SuitePatternCallSpec extends ZIOSpecDefault:
  def spec = suite("suite pattern call")(
    test("an empty pattern is a suite-selection error"):
      assertTrue(
        Calls.suitePatterns(List("")) == Left(ModuleTestError.Pattern(SuitePatternError.Empty))
      )
    ,
    test("a blank pattern keeps the raw text"):
      assertTrue(
        Calls.suitePatterns(List("foo bar")) ==
          Left(ModuleTestError.Pattern(SuitePatternError.Blank("foo bar")))
      )
    ,
    test("a glob is one pattern"):
      assertTrue(
        Calls.suitePatterns(List("com.acme.Foo*")).map(_.map(_.value)) == Right(List("com.acme.Foo*"))
      )
    ,
    test("an empty pattern exits as usage"):
      val (code, message) = Cli.testFailure(ModuleTestError.Pattern(SuitePatternError.Empty))
      assertTrue(
        (code, message) == (CliExit.Usage.code, "a test pattern is not empty"),
        !message.contains("is not a project name"),
      )
    ,
    test("a blank pattern exits as usage"):
      val (code, message) = Cli.testFailure(ModuleTestError.Pattern(SuitePatternError.Blank("foo bar")))
      assertTrue(
        (code, message) == (CliExit.Usage.code, "'foo bar' is not a test pattern"),
        !message.contains("is not a project name"),
      )
    ,
    test("a preview pattern keeps the suite-selection message"):
      val error = PreviewError.Pattern(SuitePatternError.Empty)
      assertTrue(
        error.message == "a test pattern is not empty",
        !error.message.contains("is not a project name"),
      ),
  )
