package histos.cli

import histos.preview.PreviewError
import histos.report.{BuildFault, CallResult}
import histos.server.Call
import histos.testrun.{SuitePattern, SuitePatternError, TestMode}

import zio.test.*

object SuitePatternCallSpec extends ZIOSpecDefault:
  def spec = suite("suite pattern call")(
    test("an empty pattern is a suite-selection error"):
      assertTrue(SuitePattern.from("") == Left(SuitePatternError.Empty))
    ,
    test("a blank pattern keeps the raw text"):
      assertTrue(
        SuitePattern.from("foo bar") == Left(SuitePatternError.Blank("foo bar"))
      )
    ,
    test("a glob is one pattern"):
      for parsed <- Cli.parse(List("testOnly", "com.acme.Foo*"))
      yield assertTrue(
        SuitePattern.from("com.acme.Foo*").map(_.value) == Right("com.acme.Foo*"),
        called(parsed, "com.acme.Foo*"),
      )
    ,
    test("a blank pattern is usage and is not a call"):
      for parsed <- Cli.parse(List("testOnly", "foo bar")).either
      yield assertTrue(
        parsed == Left(CliParseError.Pattern(SuitePatternError.Blank("foo bar").message))
      )
    ,
    test("an empty pattern exits as usage"):
      val error  = SuitePatternError.Empty
      val result = CallResult.Fault(BuildFault.Pattern(error))
      assertTrue(
        CliExit.from(Call.Test(Nil, Nil, TestMode.Run), result) == CliExit.Usage,
        CliExit.Usage.code == 2,
        error.message == "a test pattern is not empty",
        !error.message.contains("is not a project name"),
      )
    ,
    test("a blank pattern exits as usage"):
      val error  = SuitePatternError.Blank("foo bar")
      val result = CallResult.Fault(BuildFault.Pattern(error))
      assertTrue(
        CliExit.from(Call.Test(Nil, Nil, TestMode.Run), result) == CliExit.Usage,
        error.message == "'foo bar' is not a test pattern",
        !error.message.contains("is not a project name"),
      )
    ,
    test("a preview pattern keeps the suite-selection message"):
      val error = PreviewError.Pattern(SuitePatternError.Empty)
      assertTrue(
        error.message == "a test pattern is not empty",
        !error.message.contains("is not a project name"),
      ),
  )

  private def called(parsed: Command, raw: String): Boolean =
    SuitePattern.from(raw) match
      case Right(pattern) =>
        Operation.call(parsed) == Right(Call.Test(Nil, List(pattern), TestMode.Run))
      case Left(_) => false
end SuitePatternCallSpec
