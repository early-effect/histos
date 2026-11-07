package histos.cli

import histos.coverage.{CoverageError, ExcludeKind, Excludes}
import histos.version.Catalog
import histos.{CoverageExcludes, CoverageFloors, CoverageJob, ProjectGraph, Splits}

import zio.test.*

object CoverageArgsLawSpec extends ZIOSpecDefault:
  private val macros = CoverageExcludes(classlikes = List(".*Macro.*"))

  def spec = suite("coverage args")(
    test("a flag replaces that floor, an omitted floor stays, and excludes accumulate"):
      val graph = asked(CoverageFloors(statements = Some(80), branches = Some(70)), macros)
      assertTrue(
        Cli.coverageArgs(
          graph,
          CoverageFloors(statements = Some(90)),
          Excludes(List(".*Macro.*", "histos.Once"), List("gen/.*")),
        ) == Right(
          (
            CoverageFloors(statements = Some(90), branches = Some(70)),
            Excludes(List(".*Macro.*", "histos.Once"), List("gen/.*")),
          )
        ),
        Cli.coverageArgs(asked(CoverageFloors(), macros), CoverageFloors(), Excludes(Nil, Nil)) ==
          Right((CoverageFloors(), Excludes(List(".*Macro.*"), Nil))),
        Cli.coverageArgs(
          asked(CoverageFloors(), CoverageExcludes(classlikes = List("("))),
          CoverageFloors(),
          Excludes(Nil, Nil),
        ) == Left(CoverageError.BadRegex(ExcludeKind.Classlike, "(")),
      )
  )

  private def asked(floors: CoverageFloors, excludes: CoverageExcludes): ProjectGraph =
    ProjectGraph.trusted(
      Nil,
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      coverage = if floors == CoverageFloors() then CoverageJob.Off else CoverageJob.Ask(floors),
      coverageExcludes = excludes,
    )
end CoverageArgsLawSpec
