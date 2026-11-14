package histos

import histos.plugin.{JobCheck, JobName}
import histos.workflow.{CiBranch, RenderingLaw, RenderingShip, Workflow}

import zio.test.*

object CoverageWorkflowLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"

  given CiBranch      = CiBranch("main")
  given RenderingShip = RenderingLaw.ship

  def spec = suite("coverage workflow")(
    test("a build that does not ask has no coverage job"):
      val built = Projects.organization(organization"rocks.earlyeffect")(module("core", Jvm)(scala3))
      built.flatMap(Workflow.render) match
        case Right(text) =>
          assertTrue(
            !text.linesIterator.contains("  coverage:"),
            !text.contains("./histos coverage"),
            built.forall(graph => !JobCheck.generated(graph).contains(JobName.trusted("coverage"))),
          )
        case left => assertTrue(left == Right("rendered"))
    ,
    test("an ask runs histos coverage, and a floor is a flag on that run"):
      val open    = Projects.organization(organization"rocks.earlyeffect").coverage()(module("core", Jvm)(scala3))
      val limited =
        Projects
          .organization(organization"rocks.earlyeffect")
          .coverage(CoverageFloors(statements = Some(90)))(module("core", Jvm)(scala3))
      (open.flatMap(Workflow.render), limited.flatMap(Workflow.render)) match
        case (Right(plain), Right(floored)) =>
          assertTrue(
            plain.linesIterator.contains("  coverage:"),
            plain.contains("""- run: "./histos coverage""""),
            !plain.contains("--min-stmt"),
            floored.contains("""- run: "./histos coverage --min-stmt 90""""),
            limited.forall(graph => JobCheck.generated(graph).contains(JobName.trusted("coverage"))),
          )
        case (left, right) => assertTrue(left == Right("rendered"), right == Right("rendered")),
  )
end CoverageWorkflowLawSpec
