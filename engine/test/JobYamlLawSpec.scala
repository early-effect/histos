package histos

import histos.plugin.*
import histos.version.CommitSha
import histos.workflow.*

import zio.{Scope, ZIO}
import zio.test.*

/** In-tree plugin. `Projects` copies [[contribute]] and does not call [[run]]. */
private final class LawEcho(jobs: List[CiJob]) extends HistosPlugin:
  def id: PluginId = PluginId("test.echo")

  def contribute: Contribution =
    Contribution(
      List(
        DeclaredTask(TaskName("echo"), Seal.Open, Set.empty, Nil, Nil, Nil),
        DeclaredTask(TaskName("site"), Seal.Open, Set.empty, Nil, Nil, Nil),
      ),
      Nil,
      jobs,
    )

  def run(request: RunRequest): ZIO[Scope, PluginRunError, PluginOutput] =
    ZIO.succeed(PluginOutput(request.id.render, Nil))
end LawEcho

object JobYamlLawSpec extends ZIOSpecDefault:
  private val scala3   = scala"3.10.0"
  private val branch   = "trunk"
  private val large    = RunnerName("large")
  private val token    = SecretName("HISTOS_TOKEN")
  private val tokenKey = EnvName("TOKEN")
  private val action   = ActionName("aws-actions/configure-aws-credentials")
  private val catalog  =
    CiSettings.empty.copy(runners = Map(large -> "ubuntu-24.04-large"), secrets = Set(token))

  private val onPush =
    "github.event_name == 'push' && github.ref == format('refs/heads/{0}', github.event.repository.default_branch)"

  private val onCi =
    s"github.event_name == 'pull_request' || ($onPush)"

  private val noneFailed =
    "!cancelled() && !contains(needs.*.result, 'failure')"

  private val onCiAndNeeds =
    s"($onCi) && ($noneFailed)"

  private val echoRun = "\"./histos run test.echo/echo\""
  private val siteRun = "\"./histos run test.echo/site\""
  private val ubuntu  = "    runs-on: ubuntu-26.04"
  private val header  = CiHeader(Nil, Set.empty, Nil)

  def spec = suite("job yaml laws")(
    test("every ci fault lifts to that ci error, and any other build error is an unpinned run of its message"):
      jobName("docs"): docs =>
        jobName("test"): suite =>
          jobName("pages"): pages =>
            val core   = ProjectName("core")
            val target = TargetName("one")
            val rows   = List(
              BuildError.Ci(CiFault.IllegalSite(docs))                  -> CiError.IllegalSite(docs),
              BuildError.Ci(CiFault.UnknownNeed(docs, suite))           -> CiError.UnknownNeed(docs, suite),
              BuildError.Ci(CiFault.DuplicateNeed(docs, suite))         -> CiError.DuplicateNeed(docs, suite),
              BuildError.Ci(CiFault.UnknownModule(docs, core))          -> CiError.UnknownModule(docs, core),
              BuildError.Ci(CiFault.DuplicateTarget(docs, target))      -> CiError.DuplicateTarget(docs, target),
              BuildError.Ci(CiFault.UnpinnedAction(docs, action))       -> CiError.UnpinnedAction(docs, action),
              BuildError.Ci(CiFault.GrantMissing(docs, action))         -> CiError.GrantMissing(docs, action),
              BuildError.Ci(CiFault.UnknownRunner(docs, large))         -> CiError.UnknownRunner(docs, large),
              BuildError.Ci(CiFault.UnknownSecret(token))               -> CiError.UnknownSecret(token),
              BuildError.Ci(CiFault.UnreachableCond(docs))              -> CiError.UnreachableCond(docs),
              BuildError.Ci(CiFault.DispatchInputConflict("mode"))      -> CiError.DispatchInputConflict("mode"),
              BuildError.Ci(CiFault.ConcurrencyConflict("ci.yml"))      -> CiError.ConcurrencyConflict("ci.yml"),
              BuildError.Ci(CiFault.UnpinnedWorkflowRun("missing.yml")) -> CiError.UnpinnedWorkflowRun("missing.yml"),
              BuildError.DuplicateSite(docs, suite)                     -> CiError.DuplicateSite(docs, suite),
            )
            val other = List(
              BuildError.DuplicateJob(suite),
              BuildError.ReservedJob(pages),
              BuildError.DuplicatePlugin(PluginId("test.echo")),
              BuildError.BadName("Not A Name", SourcePos("law", 1, 1)),
            )
            val lifted = rows.forall: (error, want) =>
              JobYaml.lift(error) == want
            val wrapped = other.forall(error => JobYaml.lift(error) == CiError.UnpinnedWorkflowRun(error.message))
            assertTrue(lifted, wrapped, other.map(_.message).distinct.size == other.size)
    ,
    test("a pinned action is the pin line, and a local action is the path"):
      val custom = ActionName("example/setup")
      val pin    = ActionPin(CommitSha("0123456789abcdef0123456789abcdef01234567"), "v9")
      val pins   = List(ActionPins.checkout, ActionPins.deployPages, ActionPins.uploadPages)
      val pinned = pins.forall(item => JobYaml.uses(ActionUse.Pinned(item.name, item.pin)) == item.line)
      val tagged = pins.forall(item => item.line != item.name.value)
      assertTrue(
        pinned,
        tagged,
        JobYaml.uses(ActionUse.Pinned(custom, pin)) == ActionPins.line(custom, pin),
        ActionPins.line(custom, pin) != ActionPins.checkout.line,
        JobYaml.uses(ActionUse.Local(ActionPins.localSetup)) == ActionPins.localSetup,
        JobYaml.uses(ActionUse.Local(ActionPins.originHead)) == ActionPins.originHead,
      )
    ,
    test("one task job prints the run, the runner, the condition, the permissions, and the secret"):
      jobName("nightly"): nightly =>
        val job = TaskJob(
          nightly,
          TaskName("echo"),
          runner = Runner.Named(large),
          permissions = JobPermissions(Set(Grant.PackagesRead, Grant.IssuesRead)),
          env = List(tokenKey -> EnvValue.Secret(token)),
        )
        val text = taskText(
          "nightly",
          onCi,
          "    runs-on: ubuntu-24.04-large",
          Nil,
          Nil,
          List("      contents: read", "      packages: read", "      issues: read"),
          List("    env:", "      TOKEN: ${{ secrets.HISTOS_TOKEN }}"),
          echoRun,
        )
        assertTrue(
          yaml(built(List(job), catalog)) == Right(Right(JobYaml.Rendered(List(text), Nil, header)))
        )
    ,
    test("a dispatch site is the pages file, and the build job stays on ci"):
      jobName("docs"): docs =>
        val job   = SiteJob(docs, TaskName("site"), Site(RepoPath("docs"), SitePublish.Manual))
        val build =
          taskText("docs", onPush, ubuntu, Nil, Nil, List("      contents: read"), Nil, siteRun)
        val file = RenderedWorkflow(Workflow.pagesName, dispatchText)
        assertTrue(
          yaml(built(List(job))) == Right(Right(JobYaml.Rendered(List(build), List(file), header)))
        )
    ,
    test("a deploy site adds the pages job, the upload, and the actions grant"):
      jobName("docs"): docs =>
        val job    = SiteJob(docs, TaskName("site"), Site(RepoPath("docs"), SitePublish.Automatic))
        val deploy = taskText(
          "docs",
          onPush,
          ubuntu,
          Nil,
          Nil,
          List("      contents: read", "      actions: write"),
          Nil,
          siteRun,
          uploadDocs,
        )
        assertTrue(
          yaml(built(List(job))) ==
            Right(Right(JobYaml.Rendered(List(deploy, pagesJob("docs")), Nil, header)))
        )
    ,
    test("a job need is the needs line, and the default policy adds the none-failed condition"):
      jobName("nightly"): nightly =>
        jobName("test"): suite =>
          jobName("doc"): doc =>
            val one    = TaskJob(nightly, TaskName("echo"), needs = List(suite))
            val two    = TaskJob(nightly, TaskName("echo"), needs = List(suite, doc))
            val read   = List("      contents: read")
            val single =
              taskText("nightly", onCiAndNeeds, ubuntu, List("    needs: test"), Nil, read, Nil, echoRun)
            val both =
              taskText("nightly", onCiAndNeeds, ubuntu, List("    needs: [test, doc]"), Nil, read, Nil, echoRun)
            assertTrue(
              yaml(built(List(one))) == Right(Right(JobYaml.Rendered(List(single), Nil, header))),
              yaml(documented(List(two))) == Right(Right(JobYaml.Rendered(List(both), Nil, header))),
            )
    ,
    test("a concurrency block is the group and the cancel flag"):
      jobName("nightly"): nightly =>
        val stop = TaskJob(
          nightly,
          TaskName("echo"),
          concurrency = Some(Concurrency(GroupName("ship"), Cancel.InProgress)),
        )
        val finish = TaskJob(
          nightly,
          TaskName("echo"),
          concurrency = Some(Concurrency(GroupName("notes"), Cancel.Finish)),
        )
        val read     = List("      contents: read")
        val stopping = taskText(
          "nightly",
          onCi,
          ubuntu,
          Nil,
          List("    concurrency:", "      group: ship", "      cancel-in-progress: true"),
          read,
          Nil,
          echoRun,
        )
        val finishing = taskText(
          "nightly",
          onCi,
          ubuntu,
          Nil,
          List("    concurrency:", "      group: notes", "      cancel-in-progress: false"),
          read,
          Nil,
          echoRun,
        )
        assertTrue(
          yaml(built(List(stop))) == Right(Right(JobYaml.Rendered(List(stopping), Nil, header))),
          yaml(built(List(finish))) == Right(Right(JobYaml.Rendered(List(finishing), Nil, header))),
        )
    ,
    test("a grant is a permission in scope order, and write replaces read"):
      jobName("ship"): ship =>
        val job = TaskJob(
          ship,
          TaskName("echo"),
          permissions = JobPermissions(
            Set(
              Grant.ContentsRead,
              Grant.ContentsWrite,
              Grant.PackagesRead,
              Grant.PullRequestsWrite,
              Grant.ActionsRead,
              Grant.IdTokenWrite,
              Grant.PagesRead,
              Grant.IssuesWrite,
              Grant.SecurityEventsWrite,
            )
          ),
        )
        val text = taskText(
          "ship",
          onCi,
          ubuntu,
          Nil,
          Nil,
          List(
            "      contents: write",
            "      packages: read",
            "      pull-requests: write",
            "      actions: read",
            "      id-token: write",
            "      pages: read",
            "      issues: write",
            "      security-events: write",
          ),
          Nil,
          echoRun,
        )
        assertTrue(
          yaml(built(List(job))) == Right(Right(JobYaml.Rendered(List(text), Nil, header)))
        )
    ,
    test("an unpinned action is refused while the graph is built"):
      jobName("nightly"): nightly =>
        val job = TaskJob(
          nightly,
          TaskName("echo"),
          pre = List(PinnedAction(action, Nil, Set(Grant.IdTokenWrite))),
        )
        assertTrue(built(List(job)) == Left(BuildError.Ci(CiFault.UnpinnedAction(nightly, action)))),
  )

  private def built(jobs: List[CiJob], ci: CiSettings = CiSettings.empty): Either[BuildError, ProjectGraph] =
    Projects.organization(organization"example.com").ci(ci)(use(LawEcho(jobs)), module("core", Jvm)(scala3))

  private def documented(jobs: List[CiJob]): Either[BuildError, ProjectGraph] =
    Projects.organization(organization"example.com").docCheck(use(LawEcho(jobs)), module("core", Jvm)(scala3))

  private def yaml(
      graph: Either[BuildError, ProjectGraph]
  ): Either[BuildError, Either[CiError, JobYaml.Rendered]] =
    graph.map(found => JobYaml.render(found, branch))

  private def jobName(raw: String)(use: JobName => TestResult): TestResult =
    JobName.from(raw) match
      case Right(parsed) => use(parsed)
      case Left(_)       => assertTrue(JobName.from(raw) == Right(JobName.trusted(raw)))

  private def taskText(
      name: String,
      cond: String,
      runsOn: String,
      needs: List[String],
      afterTimeout: List[String],
      permissions: List[String],
      env: List[String],
      run: String,
      tail: List[String] = Nil,
  ): String =
    block(
      List(s"  $name:") ++ needs ++ List(yamlIf(cond), runsOn, "    timeout-minutes: 30") ++ afterTimeout ++
        List("    permissions:") ++ permissions ++ env ++ steps(name, run, tail)
    )

  private def pagesJob(need: String): String =
    block(
      List(
        "  pages:",
        s"    needs: $need",
        yamlIf(onPush),
        ubuntu,
        "    timeout-minutes: 30",
        "    environment:",
        "      name: github-pages",
        "      url: ${{ steps.deployment.outputs.page_url }}",
        "    concurrency:",
        "      group: pages",
        "      cancel-in-progress: false",
        "    permissions:",
        "      actions: read",
        "      contents: read",
        "      id-token: write",
        "      pages: write",
        "    steps:",
        "      - id: deployment",
        s"        uses: ${ActionPins.deployPages.line}",
      )
    )

  private def dispatchText: String =
    val lines = List(
      "name: pages",
      "on:",
      "  workflow_dispatch:",
      "permissions:",
      "  contents: read",
      "concurrency:",
      "  group: pages",
      "  cancel-in-progress: false",
      "jobs:",
      "  pages:",
      ubuntu,
      "    timeout-minutes: 30",
      "    environment:",
      "      name: github-pages",
      "      url: ${{ steps.deployment.outputs.page_url }}",
      "    permissions:",
      "      actions: write",
      "      contents: read",
      "      id-token: write",
      "      pages: write",
      "    steps:",
      s"      - uses: ${ActionPins.checkout.line}",
      "      - uses: ./.github/actions/setup",
      "      - run: ./histos doctor --preflight pages",
      s"      - run: $siteRun",
      s"      - uses: ${ActionPins.uploadPages.line}",
      "        with:",
      "          path: \"docs\"",
      "      - id: deployment",
      s"        uses: ${ActionPins.deployPages.line}",
    )
    lines.mkString("\n") + "\n"

  private val uploadDocs = List(
    "      - if: \"" + onPush + "\"",
    s"        uses: ${ActionPins.uploadPages.line}",
    "        with:",
    "          path: \"docs\"",
  )

  private def steps(job: String, run: String, tail: List[String]): List[String] =
    List(
      "    steps:",
      s"      - uses: ${ActionPins.checkout.line}",
      "      - uses: ./.github/actions/setup",
      s"      - run: ./histos doctor --preflight $job",
      s"      - run: $run",
    ) ++ tail

  private def yamlIf(cond: String): String =
    "    if: \"" + cond + "\""

  private def block(lines: List[String]): String =
    lines.mkString("\n")
end JobYamlLawSpec
