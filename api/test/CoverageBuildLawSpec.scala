package histos

import histos.plugin.{CiSettings, JobCheck, JobName}
import histos.supply.SupplyChain

import zio.test.*

object CoverageBuildLawSpec extends ZIOSpecDefault:
  private val scala3    = scala"3.10.0"
  private val excluded  = CoverageExcludes(classlikes = List(".*Macro.*"), files = List("generated/.*"))
  private val statement = CoverageFloors(statements = Some(90))

  def spec = suite("coverage on the build")(
    test("excludes do not ask for the job, and every later setting keeps them"):
      val chained = Projects
        .organization(organization"rocks.earlyeffect")
        .coverageExcludes(excluded)
        .resolvers(central)
        .tools(ToolCatalog.default)
        .runnerTools(RunnerTools.empty)
        .promote(Promote.Rebuild)
        .testLog
        .locked
        .license(License(license"Apache-2.0", url"https://www.apache.org/licenses/LICENSE-2.0.txt"))
        .developer(Developer(developer"early-effect", pom"Early Effect"))
        .scm(
          Scm(
            url"https://github.com/early-effect/histos",
            connection"scm:git:git@github.com:early-effect/histos.git",
          )
        )
        .description(pom"library")
        .url(url"https://example.com/histos")
        .supplyChain(SupplyChain())
        .docCheck
        .ci(CiSettings.empty)
        .jobTimeout(JobTimeout.Minutes(12))
        .coverage(statement)
      val only = Projects.coverageExcludes(excluded)(module("core", Jvm)(scala3))
      assertTrue(
        only.map(graph =>
          (
            graph.coverage,
            graph.coverageExcludes,
            JobCheck.generated(graph).contains(JobName.trusted("coverage")),
          )
        ) == Right((CoverageJob.Off, excluded, false)),
        chained(module("core", Jvm)(scala3)).map(kept) ==
          Right((CoverageJob.Ask(statement), excluded, JobTimeout.Minutes(12), DocCheck.On)),
        Projects.coverage(statement).coverageExcludes(excluded)(module("core", Jvm)(scala3)).map(kept) ==
          Right((CoverageJob.Ask(statement), excluded, JobTimeout.default, DocCheck.Off)),
        excluded.argv == List("--exclude-class", ".*Macro.*", "--exclude-file", "generated/.*"),
        CoverageFloors(statements = Some(95)).orElse(CoverageFloors(statements = Some(80), branches = Some(70))) ==
          CoverageFloors(statements = Some(95), branches = Some(70)),
      )
  )

  private def kept(graph: ProjectGraph): (CoverageJob, CoverageExcludes, JobTimeout, DocCheck) =
    (graph.coverage, graph.coverageExcludes, graph.ci.timeout, graph.docCheck)
end CoverageBuildLawSpec
