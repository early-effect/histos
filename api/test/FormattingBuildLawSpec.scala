package histos

import histos.plugin.RepoPath

import zio.test.*

object FormattingBuildLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"
  private val core   = module("core", Jvm)(scala3)

  def spec = suite("formatting on the build")(
    test("a build formats only when it asks, and a later setting keeps the ask"):
      val nested = RepoPath("config/scalafmt.conf")
      assertTrue(
        Projects(core).map(_.formatting) == Right(Formatting.Off),
        Projects.scalafmt(core).map(_.formatting) == Right(Formatting.Scalafmt(ScalafmtConf.Root)),
        Projects.scalafmt(nested).docCheck.locked(core).map(_.formatting) ==
          Right(Formatting.Scalafmt(ScalafmtConf.At(nested))),
        Projects
          .organization(organization"rocks.earlyeffect")
          .scalafmt
          .coverage()(core)
          .map(graph => (graph.formatting, graph.coverage, graph.organization)) == Right(
          (
            Formatting.Scalafmt(ScalafmtConf.Root),
            CoverageJob.Ask(CoverageFloors()),
            Some(organization"rocks.earlyeffect"),
          )
        ),
        ScalafmtConf.Root.relative == ".scalafmt.conf",
        ScalafmtConf.At(nested).relative == "config/scalafmt.conf",
      )
  )
end FormattingBuildLawSpec
