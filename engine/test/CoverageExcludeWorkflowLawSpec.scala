package histos

import histos.plugin.{JobCheck, JobName}
import histos.workflow.{CiBranch, Workflow}

import zio.test.*

object CoverageExcludeWorkflowLawSpec extends ZIOSpecDefault:
  private val scala3   = scala"3.10.0"
  private val excluded = CoverageExcludes(classlikes = List(".*Macro.*"), files = List("generated/.*"))

  given CiBranch = CiBranch("main")

  def spec = suite("coverage excludes on the workflow")(
    test("excludes alone do not render a coverage job"):
      val built = Projects
        .organization(organization"rocks.earlyeffect")
        .coverageExcludes(excluded)(
          module("core", Jvm)(scala3)
        )
      val rendered = built.flatMap(Workflow.render)
      assertTrue(
        rendered.map(text =>
          if text.linesIterator.contains("  coverage:") || text.contains("./histos coverage") then text else "absent"
        ) == Right("absent"),
        built.map(graph => JobCheck.generated(graph).contains(JobName.trusted("coverage"))) == Right(false),
      )
    ,
    test("an ask prints the build floors and the build excludes on the coverage run"):
      val built = Projects
        .organization(organization"rocks.earlyeffect")
        .coverageExcludes(excluded)
        .coverage(CoverageFloors(statements = Some(90)))(module("core", Jvm)(scala3))
      val line = """- run: "./histos coverage --min-stmt 90 --exclude-class .*Macro.* --exclude-file generated/.*""""
      val rendered = built.flatMap(Workflow.render)
      assertTrue(
        rendered.map(text => if text.contains(line) then "kept" else text) == Right("kept"),
        built.map(graph => JobCheck.generated(graph).contains(JobName.trusted("coverage"))) == Right(true),
      ),
  )
end CoverageExcludeWorkflowLawSpec
