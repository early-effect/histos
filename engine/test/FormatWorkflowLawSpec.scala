package histos

import histos.plugin.{JobCheck, JobName, RepoPath}
import histos.workflow.{CiBranch, RenderingLaw, RenderingShip, Workflow}

import zio.test.*

object FormatWorkflowLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"
  private val format = JobName.trusted("format")

  given CiBranch      = CiBranch("main")
  given RenderingShip = RenderingLaw.ship

  def spec = suite("format workflow")(
    test("a build that does not format has no format job"):
      val built = Projects.organization(organization"rocks.earlyeffect")(module("core", Jvm)(scala3))
      built.flatMap(Workflow.render) match
        case Right(text) =>
          assertTrue(
            !text.linesIterator.contains("  format:"),
            !text.contains("./histos fmt"),
            built.forall(graph => !JobCheck.generated(graph).contains(format)),
          )
        case left => assertTrue(left == Right("rendered"))
    ,
    test("a build that formats checks with histos fmt, beside the suite and without the store"):
      val rooted = Projects.organization(organization"rocks.earlyeffect").scalafmt(module("core", Jvm)(scala3))
      val nested =
        Projects
          .organization(organization"rocks.earlyeffect")
          .scalafmt(RepoPath("config/scalafmt.conf"))(
            module("core", Jvm)(scala3)
          )
      (rooted.flatMap(Workflow.render), nested.flatMap(Workflow.render)) match
        case (Right(text), Right(other)) =>
          val job = text.linesIterator.dropWhile(_ != "  format:").drop(1).takeWhile(_.startsWith("    ")).toList
          assertTrue(
            job.contains("""      - run: "./histos fmt --check""""),
            !job.exists(_.contains("needs: affected")),
            !job.exists(_.contains("cache/restore")),
            other.contains("""      - run: "./histos fmt --check""""),
            rooted.forall(graph => JobCheck.generated(graph).contains(format)),
          )
        case (left, right) => assertTrue(left == Right("rendered"), right == Right("rendered")),
  )
end FormatWorkflowLawSpec
