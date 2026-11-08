package histos.cli

import histos.coverage.{CoverageError, ExcludeKind, Excludes}
import histos.{CoverageFloorError, CoverageFloors, FloorName, ProjectName}

import zio.test.*

object CoverageGrammarSpec extends ZIOSpecDefault:
  def spec = suite("coverage grammar")(
    test("floors and repeated excludes parse, and the command is not a wire call"):
      val args = List(
        "coverage",
        "--min-stmt",
        "90",
        "--exclude-class",
        "demo\\..*",
        "--exclude-file",
        "src/gen/.*",
        "--exclude-class",
        "other\\..*",
        "api",
      )
      for
        parsed  <- Grammar.parse(args)
        watched <- Grammar.parse("~" :: args)
      yield assertTrue(
        parsed == Command.Coverage(
          List(ProjectName.trusted("api")),
          CoverageFloors(statements = Some(90)),
          Excludes(List("demo\\..*", "other\\..*"), List("src/gen/.*")),
        ),
        watched == Command.Watch(parsed, args),
        Operation.operation(parsed) == Left(CliParseError.Rejected),
        Help.names.contains("coverage"),
      )
    ,
    test("a floor outside 0 to 100, a bad regex, and a missing regex are usage"):
      for
        high    <- Grammar.parse(List("coverage", "--min-stmt", "200")).either
        broken  <- Grammar.parse(List("coverage", "--exclude-class", "(")).either
        missing <- Grammar.parse(List("coverage", "--exclude-file")).either
        file    <- Grammar.parse(List("coverage", "--exclude-class")).either
      yield assertTrue(
        high == Left(
          CliParseError.Pattern(CoverageFloorError.OutOfRange(FloorName.Statements, BigInt(200)).message)
        ),
        broken == Left(CliParseError.Pattern(CoverageError.BadRegex(ExcludeKind.Classlike, "(").message)),
        missing == Left(CliParseError.Pattern("coverage --exclude-file needs a regex")),
        file == Left(CliParseError.Pattern("coverage --exclude-class needs a regex")),
      )
    ,
    test("a repeated floor is rejected"):
      Grammar
        .parse(List("coverage", "--min-stmt", "1", "--min-stmt", "2"))
        .either
        .map: refused =>
          assertTrue(refused == Left(CliParseError.Pattern("coverage --min-stmt is already set"))),
  )
