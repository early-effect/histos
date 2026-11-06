package histos.supply

import histos.doctor.{Fix, FixApply, GitHubCredential, GitHubProbe, JobEnv, Outcome}
import histos.plugin.SecretName
import histos.workflow.SupplyYaml
import histos.ProjectGraph
import histos.TestMachine

import java.nio.file.Files

import zio.{Ref, UIO, ZIO}
import zio.test.*

object GitHubDoctorSpec extends ZIOSpecDefault:
  private val repo           = RepoName.trusted("early-effect/specular")
  private val main           = BranchName.trusted("main")
  private val needed         = List("HISTOS_APP_ID", "HISTOS_APP_PRIVATE_KEY")
  private val repositoryBody =
    """{"default_branch":"main","owner":{"login":"early-effect","type":"Organization"},"allow_squash_merge":true,"allow_merge_commit":false,"allow_rebase_merge":false}"""
  private val userBody =
    """{"default_branch":"main","owner":{"login":"ada","type":"User"},"allow_squash_merge":true}"""
  private val workflowBody =
    """{"default_workflow_permissions":"read","can_approve_pull_request_reviews":false}"""
  private val bothSecrets =
    """{"secrets":[{"name":"HISTOS_APP_ID"},{"name":"HISTOS_APP_PRIVATE_KEY"}]}"""
  private val noSecrets = """{"secrets":[]}"""
  private val writes    =
    """{"permissions":{"contents":"write","pull_requests":"write","workflows":"write"}}"""
  private val admins =
    """{"permissions":{"contents":"admin","pull_requests":"admin","workflows":"admin"}}"""
  private val repoRule =
    """[{"type":"required_status_checks","ruleset_id":18791972,"ruleset_source_type":"Repository","ruleset_source":"early-effect/specular","parameters":{"required_status_checks":[{"context":"test"}]}}]"""
  private val orgRule =
    """[{"type":"required_status_checks","ruleset_id":42,"ruleset_source_type":"Organization","ruleset_source":"early-effect","parameters":{"required_status_checks":[{"context":"test"}]}}]"""
  private val signatureRule =
    """[{"type":"required_signatures","ruleset_id":7,"ruleset_source_type":"Repository","ruleset_source":"early-effect/specular"}]"""
  private val classicSigned =
    """{"required_status_checks":{"contexts":["supply-chain"]},"required_signatures":{"enabled":true}}"""

  private final case class Pages(
      install: Reply = Reply(200, writes),
      secrets: Reply = Reply(200, bothSecrets),
      org: Reply = Reply(200, noSecrets),
      rules: Reply = Reply(200, "[]"),
      protection: Reply = Reply(404, "off"),
      flow: Reply = Reply(200, workflowBody),
      repository: Reply = Reply(200, repositoryBody),
  )

  def spec = suite("GitHubDoctorSpec")(
    suite("credential")(
      test("GH_TOKEN beats GITHUB_TOKEN and gh is not asked"):
        val gh = (_: Option[String]) => ZIO.dieMessage("gh was called")
        GitHubCredential
          .resolve(Map("GH_TOKEN" -> "kept", "GITHUB_TOKEN" -> "other"), gh)
          .map(token => assertTrue(token.exists(_.value == "kept")))
      ,
      test("github.com, ghe.com, and localhost read GH_TOKEN; another host reads the enterprise pair"):
        val env = Map(
          "GH_TOKEN"                -> "dotcom",
          "GITHUB_TOKEN"            -> "actions",
          "GH_ENTERPRISE_TOKEN"     -> "ent",
          "GITHUB_ENTERPRISE_TOKEN" -> "other",
        )
        assertTrue(
          GitHubCredential.fromEnv(env, "github.com").contains("dotcom"),
          GitHubCredential.fromEnv(env, "acme.ghe.com").contains("dotcom"),
          GitHubCredential.fromEnv(env, "localhost").contains("dotcom"),
          GitHubCredential.fromEnv(env, "ghe.example.com").contains("ent"),
          GitHubCredential.fromEnv(Map.empty, "github.com").isEmpty,
        )
      ,
      test("an empty env uses the injected gh token and leaves the host at github.com"):
        GitHubCredential
          .resolve(
            Map.empty,
            host =>
              ZIO.succeed:
                host match
                  case None    => Some("from-gh")
                  case Some(_) => None,
          )
          .map: token =>
            assertTrue(
              token.exists(item =>
                item.value == "from-gh" && item.host == "github.com" && item.api == "https://api.github.com"
              )
            )
      ,
      test("GH_HOST selects the enterprise api root"):
        GitHubCredential
          .resolve(Map("GH_HOST" -> "ghe.example.com"), host => ZIO.succeed(host.map(_ => "from-gh")))
          .map: token =>
            assertTrue(
              token.exists(item => item.value == "from-gh" && item.api == "https://ghe.example.com/api/v3")
            ),
    ),
    suite("secrets")(
      test("an organization secret list counts"):
        ask(Pages(org = Reply(200, bothSecrets), secrets = Reply(200, noSecrets))).map: outcomes =>
          assertTrue(ok(outcomes, "supply.secret-missing"))
      ,
      test("a repository secret list counts"):
        ask(Pages(org = Reply(200, noSecrets), secrets = Reply(200, bothSecrets))).map: outcomes =>
          assertTrue(ok(outcomes, "supply.secret-missing"))
      ,
      test("organization secrets 404 does not discard a repository secret"):
        ask(Pages(org = Reply(404, "none"), secrets = Reply(200, bothSecrets))).map: outcomes =>
          assertTrue(ok(outcomes, "supply.secret-missing"))
      ,
      test("HISTOS_SECRET_PRESENT true counts and the secret lists are not read"):
        val job = JobEnv.from(
          Map(
            "HISTOS_SECRET_PRESENT_HISTOS_APP_ID"          -> "true",
            "HISTOS_SECRET_PRESENT_HISTOS_APP_PRIVATE_KEY" -> "true",
          )
        )
        recorded(Pages(org = Reply(500, "no"), secrets = Reply(500, "no")), job).map: (outcomes, urls) =>
          assertTrue(
            ok(outcomes, "supply.secret-missing"),
            !urls.exists(url => url.contains("/actions/secrets")),
          )
      ,
      test("a refused secret list is not a missing secret"):
        ask(Pages(secrets = Reply(403, "no"), org = Reply(403, "no"))).map: outcomes =>
          val text = shown(outcomes)
          assertTrue(
            found(outcomes, "supply.secret-missing"),
            text.contains("cannot see"),
            !text.contains("is not set"),
          )
      ,
      test("a missing organization secret points at the organization"):
        ask(Pages(secrets = Reply(200, noSecrets), org = Reply(200, noSecrets))).map: outcomes =>
          val text = shown(outcomes)
          assertTrue(
            text.contains("organizations/early-effect/settings/secrets/actions"),
            text.contains("a repository secret also counts"),
            !text.contains("gh secret set"),
          )
      ,
      test("a user repository points at the repository secret"):
        ask(Pages(repository = Reply(200, userBody), secrets = Reply(200, noSecrets), org = Reply(404, "none")))
          .map: outcomes =>
            val text = shown(outcomes)
            assertTrue(
              text.contains("github.com/early-effect/specular/settings/secrets/actions/new"),
              !text.contains("organizations/"),
            ),
    ),
    suite("installation")(
      test("a 401 is not a contents refusal and both app checks fail closed"):
        ask(Pages(install = Reply(401, "bad credentials"))).map: outcomes =>
          val text = shown(outcomes)
          assertTrue(
            found(outcomes, "supply.app-installed"),
            found(outcomes, "supply.app-permissions"),
            text.contains("cannot see"),
            text.contains("not a contents refusal"),
            !text.contains("supply.github-refused"),
          )
      ,
      test("a 404 fails installed and permissions"):
        ask(Pages(install = Reply(404, "missing"))).map: outcomes =>
          val text = shown(outcomes)
          assertTrue(
            found(outcomes, "supply.app-installed"),
            found(outcomes, "supply.app-permissions"),
            text.contains("not installed"),
          )
      ,
      test("write permissions are enough"):
        ask(Pages()).map: outcomes =>
          assertTrue(ok(outcomes, "supply.app-installed"), ok(outcomes, "supply.app-permissions"))
      ,
      test("admin satisfies write"):
        ask(Pages(install = Reply(200, admins))).map: outcomes =>
          assertTrue(ok(outcomes, "supply.app-permissions"))
      ,
      test("a successful app step answers both checks and skips the installation call"):
        val job = JobEnv.from(Map("HISTOS_STEP_APP" -> "success"))
        recorded(Pages(install = Reply(401, "no")), job).map: (outcomes, urls) =>
          assertTrue(
            ok(outcomes, "supply.app-installed"),
            ok(outcomes, "supply.app-permissions"),
            !urls.exists(_.endsWith("/installation")),
          )
      ,
      test("known missing secrets do not also claim the app is missing"):
        ask(Pages(secrets = Reply(200, noSecrets), org = Reply(200, noSecrets), install = Reply(401, "no"))).map:
          outcomes =>
            val text = shown(outcomes)
            assertTrue(
              text.contains("is not set"),
              !text.contains("supply.app-installed"),
              !text.contains("supply.app-permissions"),
            )
      ,
      test("a real 404 still reports that the app is not installed when the secrets are missing"):
        ask(Pages(secrets = Reply(200, noSecrets), org = Reply(200, noSecrets), install = Reply(404, "missing"))).map:
          outcomes => assertTrue(shown(outcomes).contains("not installed"), found(outcomes, "supply.secret-missing")),
    ),
    suite("rules")(
      test("classic protection off and a repository ruleset names that ruleset"):
        ask(Pages(rules = Reply(200, repoRule), protection = Reply(404, "off"))).map: outcomes =>
          val text = shown(outcomes)
          assertTrue(
            text.contains("repository ruleset 18791972"),
            text.contains("https://github.com/early-effect/specular/rules/18791972"),
            text.contains("test"),
            !text.contains("github.com/settings/rules"),
          )
      ,
      test("an organization ruleset links to the organization settings"):
        ask(Pages(rules = Reply(200, orgRule), protection = Reply(404, "off"))).map: outcomes =>
          assertTrue(shown(outcomes).contains("https://github.com/organizations/early-effect/settings/rules/42"))
      ,
      test("a rules 403 is fail closed even when classic protection lists the check"):
        ask(Pages(rules = Reply(403, "no"), protection = Reply(200, classicSigned))).map: outcomes =>
          val text = shown(outcomes)
          assertTrue(
            found(outcomes, "supply.required-check"),
            text.contains("cannot see"),
            !text.contains("does not require"),
            !text.contains("supply.signing"),
          )
      ,
      test("no rule links to the repository rules page"):
        ask(Pages(rules = Reply(200, "[]"), protection = Reply(404, "off"))).map: outcomes =>
          val text = shown(outcomes)
          assertTrue(
            text.contains("No rule on main"),
            text.contains("https://github.com/early-effect/specular/settings/rules"),
            !text.contains("github.com/settings/rules"),
          )
      ,
      test("a rules document that does not decode is unreadable"):
        ask(Pages(rules = Reply(200, "not-json"))).map: outcomes =>
          assertTrue(shown(outcomes).contains("do not understand"))
      ,
      test("a required_signatures rule is still signing"):
        ask(Pages(rules = Reply(200, signatureRule), protection = Reply(404, "off"))).map: outcomes =>
          assertTrue(signing(outcomes))
      ,
      test("classic required signatures stay signing when the rules list is empty"):
        ask(Pages(rules = Reply(200, "[]"), protection = Reply(200, classicSigned))).map: outcomes =>
          assertTrue(signing(outcomes), ok(outcomes, "supply.required-check")),
    ),
    suite("workflow permissions")(
      test("a 403 warns on both checks and is not a refusal"):
        ask(Pages(flow = Reply(403, """{"message":"Resource not accessible by integration"}"""))).map: outcomes =>
          val text = shown(outcomes)
          assertTrue(
            warning(outcomes, "supply.default-token"),
            warning(outcomes, "supply.allow-prs"),
            text.contains("cannot see workflow permissions (Administration read)"),
            !text.contains("supply.github-refused"),
          )
      ,
      test("a 200 body that does not decode is unreadable"):
        ask(Pages(flow = Reply(200, "not-json"))).map: outcomes =>
          val text = shown(outcomes)
          assertTrue(
            warning(outcomes, "supply.default-token"),
            warning(outcomes, "supply.allow-prs"),
            text.contains("do not understand"),
            !text.contains("supply.github-refused"),
          )
      ,
      test("a write default warns and an App author keeps approval at info when the read token is the Actions token"):
        val job  = JobEnv.from(Map("GITHUB_ACTIONS" -> "true", "GITHUB_TOKEN" -> "token"))
        val body =
          """{"default_workflow_permissions":"write","can_approve_pull_request_reviews":true}"""
        ask(Pages(flow = Reply(200, body)), job).map: outcomes =>
          val text = shown(outcomes)
          assertTrue(
            warning(outcomes, "supply.default-token"),
            level(outcomes, "supply.allow-prs").contains(histos.doctor.Level.Info),
            text.contains("An App opens the pull request"),
            !text.contains("supply.github-refused"),
          )
      ,
      test("a 404 on workflow permissions is still a refusal"):
        ask(Pages(flow = Reply(404, "missing"))).map: outcomes =>
          val text = shown(outcomes)
          assertTrue(
            found(outcomes, "supply.github-refused"),
            !text.contains("supply.default-token"),
            !text.contains("supply.allow-prs"),
          ),
    ),
    suite("closed")(
      test("no credential fails the supply answers and the status is 1"):
        ZIO
          .attempt(Files.createTempDirectory("histos-doctor-closed"))
          .flatMap: root =>
            val net = new Net:
              def send(method: String, url: String, body: String, headers: List[(String, String)]) =
                ZIO.dieMessage("no socket")
            DoctorSurface
              .runWith(
                root,
                ProjectGraph.trusted(Nil),
                DoctorSurface.Request(Some("supply-chain"), false, None, false, false),
                TestMachine.at(root, Map.empty),
                net,
                Map.empty,
                None,
                false,
                _ => ZIO.succeed(FixApply.Answer.No),
                _ => ZIO.unit,
                _ => ZIO.unit,
              )
              .map: (code, text) =>
                assertTrue(
                  code == 1,
                  text.contains("supply.secret-missing"),
                  text.contains("supply.app-installed"),
                  text.contains("supply.app-permissions"),
                  text.contains("supply.required-check"),
                  text.contains("No credential can answer"),
                  !text.contains("skipped supply"),
                )
      ,
      test("the token string is absent from the report"):
        val secret = "super-secret-token"
        ZIO
          .attempt(Files.createTempDirectory("histos-doctor-token"))
          .flatMap: root =>
            val net = new Net:
              def send(method: String, url: String, body: String, headers: List[(String, String)]) =
                ZIO.succeed(reply(Pages(), url))
            DoctorSurface
              .runWith(
                root,
                ProjectGraph.trusted(Nil),
                DoctorSurface.Request(Some("supply-chain"), true, None, false, false),
                TestMachine.at(root, Map.empty),
                net,
                Map("GITHUB_REPOSITORY" -> repo.value),
                Some(GitHubCredential.Token(secret, "github.com")),
                false,
                _ => ZIO.succeed(FixApply.Answer.No),
                _ => ZIO.unit,
                _ => ZIO.unit,
              )
              .map: (_, text) =>
                assertTrue(!text.contains(secret)),
    ),
    test("preflight stays offline and the full doctor mints the app token"):
      val graph = ProjectGraph.trusted(Nil)
      val pre   = SupplyYaml.preflight(graph, "workflow")
      val full  = SupplyYaml.fullDoctor(graph.supply.updates.author)
      val pat   = SupplyYaml.fullDoctor(PrAuthor.Pat(SecretName("TOKEN")))
      assertTrue(
        pre == List("      - run: ./histos doctor --preflight workflow"),
        full.exists(_.contains("actions/create-github-app-token@")),
        full.exists(_.contains("GH_TOKEN: ${{ steps.app.outputs.token || github.token }}")),
        full.count(_.contains("HISTOS_SECRET_PRESENT_")) == 2,
        !pre.exists(_.contains("GH_TOKEN")),
        pat.count(_.contains("HISTOS_SECRET_PRESENT_")) == 1,
        pat.exists(_.contains("HISTOS_SECRET_PRESENT_TOKEN")),
      ),
  )

  private def ask(pages: Pages, job: JobEnv = JobEnv.empty, token: Option[String] = Some("token")): UIO[List[Outcome]] =
    recorded(pages, job, token).map(_._1)

  private def recorded(
      pages: Pages,
      job: JobEnv = JobEnv.empty,
      token: Option[String] = Some("token"),
  ): UIO[(List[Outcome], List[String])] =
    for
      sent <- Ref.make(List.empty[String])
      net = new Net:
        def send(method: String, url: String, body: String, headers: List[(String, String)]) =
          sent.update(_ :+ url).as(reply(pages, url))
      hit  <- GitHubProbe.read(net, repo, main, token, "https://api.github.com", job, needed)
      urls <- sent.get
    yield
      val outcomes = hit match
        case GitHubProbe.Hit.Ready(read) => SupplyChecks.run(read.view, SupplyChain(), read.covered)
        case _                           => Nil
      (outcomes, urls)

  private def reply(pages: Pages, url: String): Reply =
    if url.endsWith("/actions/permissions/workflow") then pages.flow
    else if url.endsWith("/installation") then pages.install
    else if url.endsWith("/actions/organization-secrets") then pages.org
    else if url.endsWith("/actions/secrets") then pages.secrets
    else if url.contains("/rules/branches/") then pages.rules
    else if url.contains("/protection") then pages.protection
    else pages.repository

  private def ok(outcomes: List[Outcome], id: String): Boolean =
    outcomes.exists:
      case Outcome.Ok(check, _) => check.value == id
      case _                    => false

  private def found(outcomes: List[Outcome], id: String): Boolean =
    outcomes.exists:
      case Outcome.Found(diagnostic) => diagnostic.check.value == id
      case _                         => false

  private def warning(outcomes: List[Outcome], id: String): Boolean =
    level(outcomes, id).contains(histos.doctor.Level.Warning)

  private def level(outcomes: List[Outcome], id: String): Option[histos.doctor.Level] =
    outcomes.collectFirst:
      case Outcome.Found(diagnostic) if diagnostic.check.value == id => diagnostic.level

  private def signing(outcomes: List[Outcome]): Boolean =
    outcomes.exists:
      case Outcome.Ok(check, note) => check.value == "supply.signing" && note.contains("ApiSigned")
      case _                       => false

  private def shown(outcomes: List[Outcome]): String =
    outcomes
      .map:
        case Outcome.Found(diagnostic) =>
          val fixes = diagnostic.fixes.map:
            case Fix.Settings(url, click) => url.value + " " + click
            case Fix.Command(line)        => line.value
            case Fix.Config(file, _)      => file.value
            case Fix.Steps(items)         => items.mkString(" ")
          diagnostic.check.value + " " + diagnostic.summary + " " + diagnostic.explain + " " + fixes.mkString(" ")
        case Outcome.Ok(check, note)      => check.value + " " + note
        case Outcome.Skipped(check, _, _) => check.value
      .mkString("\n")
end GitHubDoctorSpec
