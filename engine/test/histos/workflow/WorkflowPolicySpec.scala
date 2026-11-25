package histos.workflow

import histos.*
import histos.plugin.{Cond, JobName, JobPermissions, PluginId, RepoPath, Site, SiteArrange, SiteShape, TaskName}
import histos.suite.HistosSpec

import zio.ZIO
import zio.test.*

object WorkflowPolicySpec extends HistosSpec:
  private val version = ScalaVersion("3.10.1")
  private val api     = module("api", Jvm)(version)
  private val built   = Projects.docCheck.scalafmt(ships(Ship("libs").next(0, 1, 0)(api)), api)
  private val site    = SiteArrange(
    JobName("docs"),
    TaskName("site"),
    JobPermissions.read,
    Nil,
    Site(RepoPath("target/site")),
    Cond.DefaultBranchPush,
  )

  private def ship = RenderingShip.from("0123456789abcdef0123456789abcdef01234567")

  def spec = suite("workflow publishing policy")(
    test("snapshots require successful same-repository CI and current head plus snapshots label") {
      for
        graph    <- ZIO.fromEither(built)
        pin      <- ZIO.fromEither(ship)
        rendered <- ZIO.fromEither:
          given RenderingShip = pin
          Workflow.snapshotWorkflow(graph)
      yield assertTrue(rendered.exists: workflow =>
        val body = workflow.body
        body.contains("workflows: [ci]") && body.contains("conclusion == 'success'") &&
        body.contains("head_repository.full_name == github.repository") &&
        body.contains(".head.sha == env.CI_SHA") && body.contains(".head.repo.full_name == env.GH_REPO") &&
        body.contains(".name == \"snapshots\"") && !body.contains(".name == \"snapshot\"") &&
        body.contains("needs.eligibility.outputs.publish == 'true'") &&
        body.contains("ref: ${{ github.event.workflow_run.head_sha || github.sha }}") &&
        body.contains("test -n \"$SONATYPE_USERNAME\" && test -n \"$SONATYPE_PASSWORD\""))
    },
    test("main snapshots remain verified and manual snapshot workflow does not become a release") {
      for
        graph    <- ZIO.fromEither(built)
        pin      <- ZIO.fromEither(ship)
        rendered <- ZIO.fromEither:
          given RenderingShip = pin
          Workflow.snapshotWorkflow(graph)
      yield assertTrue(rendered.exists: workflow =>
        val body = workflow.body
        body.contains("workflow_dispatch:") && body.contains("[ \"$CI_BRANCH\" = \"$DEFAULT_BRANCH\" ]") &&
        body.contains("[ \"$current\" = \"$CI_SHA\" ]") && body.contains("./histos publish snapshots") &&
        !body.contains("./histos publish release") && !body.contains("tags:") &&
        !body.contains("doctor --area release") && !body.contains("./histos compile api --platform jvm"))
    },
    test("site builds are required on PRs and verified main deploy waits for all required checks") {
      for
        graph <- ZIO.fromEither(built)
        pin   <- ZIO.fromEither(ship)
      yield
        given RenderingShip = pin
        val blocks          = JobYaml
          .site(graph, PluginId("site"), site, SiteShape.DeployOnPush, List("browser-check", "docs"))
          .map(_.mkString("\n"))
        assertTrue(
          blocks.exists(body =>
            body.contains("github.event_name == 'pull_request'") && body.contains("./histos run site/site") &&
              body.contains("needs: [format, compile]") && body.contains("actions/cache/restore@")
          ),
          blocks.exists(body =>
            body.contains(
              "needs: [docs, affected, compile, workflow, supply-chain, test, versions, compatibility, scaladoc, format"
            ) && body.contains("github.event_name == 'push'") && body.contains("needs.*.result") && body.contains(
              "actions/deploy-pages"
            ) && body.contains("browser-check")
          ),
          !blocks.exists(_.contains("refs/tags/")),
        )
    },
    test("manual Pages builds the site and uploads it before deployment") {
      for
        graph <- ZIO.fromEither(built)
        pin   <- ZIO.fromEither(ship)
      yield
        given RenderingShip = pin
        val body            = JobYaml.dispatch(graph, PluginId("site"), site)
        val build           = body.indexOf("./histos run site/site")
        val upload          = body.indexOf("actions/upload-pages-artifact")
        val deploy          = body.indexOf("actions/deploy-pages")
        assertTrue(
          body.contains("workflow_dispatch:"),
          build >= 0,
          upload > build,
          deploy > upload,
          !body.contains("tags:"),
        )
    },
  )
end WorkflowPolicySpec
