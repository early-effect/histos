package histos.supply

import histos.doctor.{
  DoctorCheck,
  DoctorContext,
  DoctorReport,
  Fix,
  FixKind,
  FixMode,
  Level,
  Outcome,
  Probe,
  Runner,
  Registry,
}
import histos.plugin.{
  ActionName,
  ActionPin,
  CiJob,
  Contribution,
  EntryName,
  JobName,
  JobPermissions,
  PluginId,
  RegisteredPlugin,
  RepoPath,
  TaskName,
}
import histos.version.CommitSha
import histos.workflow.{ActionUse, CiError, JobYaml, PinScan, Workflow}
import histos.{HttpUrl, ProjectGraph, ProjectName, Splits}

import java.nio.file.Files
import java.time.LocalDate

import scala.compiletime.testing.typeCheckErrors

import zio.{durationInt, ZIO}
import zio.test.*

object PinScanSpec extends ZIOSpecDefault:
  def spec = suite("PinScanSpec")(
    test("unpinned, untagged, and mismatched uses fail, and local and digest refs pass"):
      for
        root <- ZIO.attempt(Files.createTempDirectory("pins"))
        _    <- ZIO.attempt(Files.createDirectories(root.resolve(".github/workflows")))
        _    <- ZIO.attempt(
          Files.writeString(
            root.resolve(".github/workflows/ci.yml"),
            """name: ci
              |jobs:
              |  a:
              |    steps:
              |      - uses: actions/setup-java@v4
              |      - uses: actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1
              |      - uses: actions/cache@aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa # v6.1.0
              |      - uses: ./.github/actions/setup
              |      - uses: docker://example@sha256:abc
              |      - uses: someone/custom@bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb # v1
              |""".stripMargin,
          )
        )
        faults = PinScan.faults(root, ProjectGraph.trusted(Nil))
      yield assertTrue(
        faults.length == 3,
        faults.exists(_.isInstanceOf[SupplyFault.UnpinnedUses]),
        faults.exists(_.isInstanceOf[SupplyFault.PinWithoutTag]),
        faults.exists(_.isInstanceOf[SupplyFault.PinMismatch]),
        PinScan.report(faults).contains("3 pin faults"),
        typeCheckErrors("""histos.workflow.JobYaml.uses("actions/checkout@v7")""").nonEmpty,
      )
  )

object AuditSpec extends ZIOSpecDefault:
  private val purl  = Purl.trusted("pkg:maven/dev.zio/zio@2.1.27")
  private val id    = AdvisoryId.trusted("GHSA-xxxx-xxxx-xxxx")
  private val today = Date.trusted("2026-06-01")

  private def finding(severity: Severity): Finding =
    Finding(id, severity, None, List(purl), List("2.1.28"), "sample", "2026-01-01")

  def spec = suite("AuditSpec")(
    test("the batch query is never skipped and hydration runs once per changed id"):
      val queries    = new java.util.concurrent.atomic.AtomicInteger
      val hydrations = new java.util.concurrent.atomic.AtomicInteger
      for
        first <- Audit.fetch(
          List(purl),
          Map.empty,
          _ => ZIO.succeed { queries.incrementAndGet(); List(finding(Severity.High)) },
          item => ZIO.succeed { hydrations.incrementAndGet(); item },
        )
        _ <- Audit.fetch(
          List(purl),
          first._2,
          _ => ZIO.succeed { queries.incrementAndGet(); List(finding(Severity.High)) },
          item => ZIO.succeed { hydrations.incrementAndGet(); item },
        )
      yield assertTrue(queries.get() == 2, hydrations.get() == 1)
    ,
    test("severity falls back to the CVSS band"):
      val vector = Cvss.trusted("CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:H/I:H/A:H")
      assertTrue(
        Audit.classify(None, Some(vector)) == Severity.Critical,
        Audit.classify(Some("low"), None) == Severity.Low,
      )
    ,
    test("every gate, scope, outage, and ignore rule"):
      val high       = finding(Severity.High)
      val low        = finding(Severity.Low)
      val policy     = AdvisoryPolicy()
      val failed     = Audit.evaluate(List(high, low), policy, Event.PullRequest, Set(purl), today, None)
      val introduced = Audit.evaluate(
        List(high),
        policy.copy(scope = GateScope.Introduced),
        Event.PullRequest,
        Set.empty,
        today,
        None,
      )
      val closed =
        Audit.evaluate(Nil, policy, Event.Push, Set.empty, today, Some((SourceName.trusted("osv"), Hours(1))))
      val cached = Audit.evaluate(
        List(high),
        policy,
        Event.PullRequest,
        Set(purl),
        today,
        Some((SourceName.trusted("osv"), Hours(1))),
      )
      val open = Audit.evaluate(
        Nil,
        policy.copy(outage = OutageStrategy.FailOpen),
        Event.Push,
        Set.empty,
        today,
        Some((SourceName.trusted("osv"), Hours(1))),
      )
      val expired = Audit.evaluate(
        List(high),
        policy.copy(ignores = List(Ignore.until(id, Date.trusted("2026-01-01"), Reason.trusted("waiting")))),
        Event.PullRequest,
        Set(purl),
        today,
        None,
      )
      val needs = Audit.evaluate(
        Nil,
        policy.copy(ignores = List(Ignore.open(id, Reason.trusted("waiting")))),
        Event.PullRequest,
        Set.empty,
        today,
        None,
      )
      val tooLong = Audit.evaluate(
        Nil,
        policy.copy(ignores = List(Ignore.until(id, Date.trusted("2027-01-01"), Reason.trusted("waiting")))),
        Event.PullRequest,
        Set.empty,
        today,
        None,
      )
      val banned = Audit.evaluate(
        Nil,
        policy.copy(ignoreRule = IgnoreRule.Disallowed, ignores = List(Ignore.open(id, Reason.trusted("no")))),
        Event.PullRequest,
        Set.empty,
        today,
        None,
      )
      val moves = Alert.issues(Set(id), Set.empty)
      assertTrue(
        failed.exists(_.failures.exists(_.isInstanceOf[SupplyFault.Advisory])),
        failed.exists(_.annotations.nonEmpty),
        introduced.exists(_.failures.isEmpty),
        closed == Left(SupplyFault.ServiceDown(SourceName.trusted("osv"), policy.outage)),
        cached.exists(_.failures.nonEmpty),
        open == Right(AuditResult(Nil, Nil)),
        expired == Left(SupplyFault.IgnoreExpired(id, Date.trusted("2026-01-01"))),
        needs == Left(SupplyFault.IgnoreNeedsExpiry(id)),
        tooLong.isLeft,
        banned.isLeft,
        moves == List(Alert.Move.Close(id)),
      ),
  )
end AuditSpec

object BumpSpec extends ZIOSpecDefault:
  private val coord                                           = Coordinate.trusted("dev.zio:zio")
  private val today                                           = Date.trusted("2026-06-10")
  private def locked(version: String, direct: Boolean = true) =
    LockedDependency(coord, version, Purl.trusted(s"pkg:maven/dev.zio/zio@$version"), direct, false, "api")
  private def release(version: String, published: String, security: Boolean = false, pre: Boolean = false) =
    Release(
      coord,
      version,
      Date.trusted(published),
      if security then Some(AdvisoryId.trusted("GHSA-abcd-efgh-ijkl")) else None,
      pre,
    )

  def spec = suite("BumpSpec")(
    test("cooldown, pre-release, majors, and 0.x"):
      val plan = Bump.choose(
        List(locked("1.2.0")),
        List(
          release("1.2.1", "2026-06-09"),
          release("2.0.0", "2026-01-01"),
          release("1.3.0-RC1", "2026-01-01", pre = true),
        ),
        VersionPolicy(),
        today,
        false,
        TransitiveFix.default,
      )
      val zero = Bump.choose(
        List(locked("0.4.0")),
        List(release("0.5.0", "2026-01-01")),
        VersionPolicy(),
        today,
        false,
        TransitiveFix.default,
      )
      val majorZero = Bump.choose(
        List(locked("0.4.0")),
        List(release("0.5.0", "2026-01-01")),
        VersionPolicy(zeroX = ZeroX.MinorIsMajor),
        today,
        false,
        TransitiveFix.default,
      )
      assertTrue(
        plan.exists(item => item.apply.isEmpty && item.listed.nonEmpty),
        zero.exists(_.apply.nonEmpty),
        majorZero.exists(_.listed.nonEmpty),
      )
    ,
    test("a security bump is the smallest fix and a split is refused"):
      val security = Bump.choose(
        List(locked("1.0.0")),
        List(release("1.0.2", "2026-06-09", security = true), release("1.2.0", "2026-01-01", security = true)),
        VersionPolicy(),
        today,
        true,
        TransitiveFix.default,
      )
      val split = Bump.choose(
        List(locked("1.0.0").copy(cell = "jvm"), locked("1.1.0").copy(cell = "native")),
        Nil,
        VersionPolicy(),
        today,
        false,
        TransitiveFix.default,
      )
      val action = ActionName.trusted("actions/checkout")
      val sha    = CommitSha.trusted("3d3c42e5aac5ba805825da76410c181273ba90b1")
      assertTrue(
        security.exists(_.apply.headOption.exists(_.to.version == "1.0.2")),
        split.isLeft,
        Bump.verify(action, "v7.0.1", sha, false, RepoId.trusted("1"), RepoId.trusted("1")).isLeft,
        Bump.verify(action, "v7.0.1", sha, true, RepoId.trusted("1"), RepoId.trusted("2")).isLeft,
      ),
  )
end BumpSpec

object PrFlowSpec extends ZIOSpecDefault:
  private val branch = BranchName.trusted("histos/deps")
  private val main   = CommitSha.trusted("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
  private val head   = CommitSha.trusted("bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
  private def request(
      changes: List[KindChange],
      human: List[CommitSha] = Nil,
      open: Boolean = false,
      headOid: Option[CommitSha] = Some(head),
  ) =
    PrRequest(UpdatePolicy(), branch, main, headOid, changes, human, open, None)

  def spec = suite("PrFlowSpec")(
    test("api signed commits once per kind, reuses the marker, and closes a stale pull request"):
      val changes = List(KindChange(BumpKind.Libraries, List("histos.scala" -> "next")))
      val planned = PrFlow.plan(request(changes))
      val stale   = PrFlow.plan(request(Nil, open = true))
      val human   = PrFlow.plan(request(changes, human = List(head)))
      assertTrue(
        planned.exists(_.exists(_.isInstanceOf[GhCall.CommitOnBranch])),
        planned.exists(_.exists {
          case GhCall.OpenOrUpdate(title, body) => title == PrFlow.title && body.contains("histos-deps head=")
          case _                                => false
        }),
        stale.exists(_.contains(GhCall.Close(branch))),
        human.exists(_.headOption.exists(_.isInstanceOf[GhCall.Comment])),
        PrFlow.committed(branch, head, main) == Left(SupplyFault.HeadMoved(branch, head)),
      )
    ,
    test("git push and ssh signing do not call createCommitOnBranch"):
      val changes = List(KindChange(BumpKind.Actions, List("pins" -> "sha")))
      val plain   = PrFlow.plan(
        request(changes).copy(policy =
          UpdatePolicy(signing =
            CommitSigning.GitPush(Identity.Named(PersonName.trusted("Ada"), Email.trusted("ada@example.com")))
          )
        )
      )
      val ssh = PrFlow.plan(
        request(changes).copy(policy =
          UpdatePolicy(signing =
            CommitSigning.GitPushSshSigned(Identity.FirstDeveloper, histos.plugin.SecretName("KEY"))
          )
        )
      )
      assertTrue(
        plain.exists(_.exists {
          case GhCall.Push(_, false) => true
          case _                     => false
        }),
        ssh == Left(SupplyFault.IdentityMissing),
      ),
  )
end PrFlowSpec

object SnapshotSpec extends ZIOSpecDefault:
  def spec = suite("SnapshotSpec")(
    test("a lock becomes a dependency snapshot"):
      val text =
        """cell api 3.10.0 jvm
          |direct dev.zio zio 2.1.27 binary
          |artifact dev.zio zio_3 2.1.27 abc central
          |test api 3.10.0 jvm
          |artifact dev.zio zio-test_3 2.1.27 def central
          |""".stripMargin
      val manifest = SnapshotSubmit.fromLock(text)
      val body     = SnapshotSubmit.json(manifest, "abc", "refs/heads/main")
      assertTrue(
        manifest.packages.exists(item => item.relationship == "direct" && item.scope == "runtime"),
        manifest.packages.exists(_.scope == "development"),
        body.contains("\"version\":0"),
        body.contains("pkg:maven/dev.zio/zio_3@2.1.27"),
      )
  )

object SupplyChecksSpec extends ZIOSpecDefault:
  private val repo                                   = RepoName.trusted("early-effect/histos")
  private val main                                   = BranchName.trusted("main")
  private def view(token: TokenKind = TokenKind.App) =
    GitHubView(
      repo,
      token,
      true,
      Map(
        AppPermission.Contents     -> Access.Write,
        AppPermission.PullRequests -> Access.Write,
        AppPermission.Workflows    -> Access.Write,
      ),
      Set("HISTOS_APP_ID", "HISTOS_APP_PRIVATE_KEY"),
      List(JobName("supply-chain")),
      true,
      Set(MergeMethod.Squash),
      RuleSource.BranchProtection(main),
      Nil,
      Nil,
      true,
      true,
      main,
      main,
      false,
      false,
      true,
      None,
      Nil,
      true,
      RepoOwner.Organization("early-effect"),
      Sight.Known,
      Sight.Known,
      Sight.Known,
    )

  def spec = suite("SupplyChecksSpec")(
    test("a clean view is ok and each diagnostic is reachable"):
      val clean      = SupplyChecks.run(view(), SupplyChain())
      val secret     = SupplyChecks.run(view().copy(secrets = Set.empty), SupplyChain())
      val missing    = SupplyChecks.run(view().copy(appInstalled = false), SupplyChain())
      val permission = SupplyChecks.run(view().copy(permissions = Map.empty), SupplyChain())
      val token      = SupplyChecks.run(view().copy(tokenStep = StepOutcome.Failure), SupplyChain())
      val required   = SupplyChecks.run(view().copy(required = List(JobName("test"))), SupplyChain())
      val signing    = SupplyChecks.run(
        view().copy(mergeMethods = Set(MergeMethod.Merge)),
        SupplyChain().copy(updates =
          UpdatePolicy(signing =
            CommitSigning.GitPushSshSigned(
              Identity.Named(PersonName.trusted("Ada"), Email.trusted("ada@example.com")),
              histos.plugin.SecretName("KEY"),
            )
          )
        ),
      )
      val overlap =
        SupplyChecks.run(view().copy(dependabot = List(Ecosystem.GithubActions, Ecosystem.Npm)), SupplyChain())
      val security = SupplyChecks.run(view().copy(securityPrs = List(Ecosystem.Maven)), SupplyChain())
      val graph    = SupplyChecks.run(view().copy(graphOn = false), SupplyChain())
      val alerts   = SupplyChecks.run(view().copy(alertsOn = false), SupplyChain())
      val branch   = SupplyChecks.run(view().copy(defaultBranch = BranchName.trusted("trunk")), SupplyChain())
      val writes   = SupplyChecks.run(view().copy(tokenWrite = true), SupplyChain())
      val allow    = SupplyChecks.run(view(TokenKind.Github).copy(allowPrs = true), SupplyChain())
      val pins     = SupplyChecks.run(view().copy(shaPins = false), SupplyChain())
      val schedule = SupplyChecks.run(
        view().copy(disabled = Some(RepoPath.trusted(".github/workflows/histos-deps.yml"))),
        SupplyChain(),
      )
      val refused = histos.doctor.RefusalMap.refused(ApiCall.Pulls, 403, "Resource not accessible by integration")
      assertTrue(
        clean.collect { case Outcome.Found(diagnostic) if diagnostic.level == Level.Error => diagnostic }.isEmpty,
        secret.exists(found("supply.secret-missing")),
        missing.exists(found("supply.app-installed")),
        permission.exists(found("supply.app-permissions")),
        token.exists(found("supply.app-token")),
        required.exists(found("supply.required-check")),
        signing.exists(found("supply.signing")),
        overlap.exists(found("supply.dependabot-overlap")),
        security.exists(found("supply.dependabot-security")),
        graph.exists(found("supply.dependency-graph")),
        alerts.exists(found("supply.dependabot-alerts")),
        branch.exists(found("supply.default-branch")),
        writes.exists(found("supply.default-token")),
        allow.exists(found("supply.allow-prs")),
        pins.exists(found("supply.sha-pin-policy")),
        schedule.exists(found("supply.schedule-disabled")),
        refused.exists(_.level == Level.Error),
        histos.doctor.RefusalMap.refused(ApiCall.Rules, 404, "missing").isDefined,
        histos.doctor.RefusalMap.refused(ApiCall.Contents, 422, "invalid").isDefined,
      )
  )

  private def found(id: String)(outcome: Outcome): Boolean =
    outcome match
      case Outcome.Found(diagnostic) => diagnostic.check.value == id
      case _                         => false
end SupplyChecksSpec

object DoctorSpec extends ZIOSpecDefault:
  private val check = new DoctorCheck:
    def id                      = histos.doctor.CheckId.trusted("supply.sample")
    def area                    = histos.doctor.Area.SupplyChain
    def needs                   = Set(Probe.Local)
    def run(ctx: DoctorContext) = ZIO.succeed(List(Outcome.Ok(id, "ok")))

  def spec = suite("DoctorSpec")(
    test("registry order, a missing probe, a timeout, and exits"):
      val slow = new DoctorCheck:
        def id                      = histos.doctor.CheckId.trusted("supply.slow")
        def area                    = histos.doctor.Area.SupplyChain
        def needs                   = Set.empty[Probe]
        def run(ctx: DoctorContext) = ZIO.sleep(1.hour).as(Nil)
      val warn = new DoctorCheck:
        def id                      = histos.doctor.CheckId.trusted("supply.warn")
        def area                    = histos.doctor.Area.SupplyChain
        def needs                   = Set.empty[Probe]
        def run(ctx: DoctorContext) =
          val diagnostic = SupplyDiagnostic.DefaultTokenWrite(RepoName.trusted("early-effect/histos"))
          ZIO.succeed(List(Outcome.Found(diagnostic)))
      for
        skipped   <- Runner.run(List(check), DoctorContext(Nil, Set.empty), 1.second)
        lateFiber <- Runner.run(List(slow), DoctorContext(Nil, Set.empty), 1.second).fork
        _         <- TestClock.adjust(2.seconds)
        late      <- lateFiber.join
        warned    <- Runner.run(List(warn), DoctorContext(Nil, Set(Probe.Local)), 1.second)
      yield
        val shown = histos.doctor.Render.text("early-effect/histos", late)
        assertTrue(
          skipped.outcomes.headOption.exists(_.isInstanceOf[Outcome.Skipped]),
          skipped.exit == 0,
          late.timedOut.map(_.value) == List("supply.slow"),
          late.exit == 1,
          shown.contains("  x timed out supply.slow"),
          shown.contains("0 errors, 0 warnings, 0 ok, 0 skipped, 1 timed out"),
          histos.doctor.Render.annotations(late).contains("::error title=histos doctor: supply.slow::timed out"),
          histos.doctor.Render.summary("deps", late).contains("supply.slow"),
          histos.doctor.Render.summary("deps", late).contains("failed"),
          histos.doctor.Render.json(late).contains("\"exit\":1"),
          histos.doctor.Render.json(late).contains("\"timedOut\":[\"supply.slow\"]"),
          warned.exit == 0,
          warned.warnings == 1,
          histos.doctor.Render.text("early-effect/histos", warned).contains("supply.default-token"),
          !histos.doctor.Render.text("early-effect/histos", warned).contains("timed out"),
          histos.doctor.Render.annotations(warned).contains("::warning"),
          histos.doctor.Render.summary("deps", warned).contains("supply.default-token"),
          histos.doctor.Render.json(warned).contains("\"exit\":0"),
        )
  )
end DoctorSpec

object FixSpec extends ZIOSpecDefault:
  def spec = suite("FixSpec")(
    test("print changes nothing, apply edits files, and a non-tty skips settings"):
      val edit    = Fix.Config(RepoPath.trusted("histos.scala"), histos.doctor.Edit("old", "new"))
      val command = Fix.Command(histos.doctor.ShellLine.trusted("./histos workflow --update"))
      val setting = Fix.Settings(HttpUrl.trusted("https://github.com/early-effect/histos/settings/actions"), "actions")
      val printed = histos.doctor.FixApply.applyAll(
        List(edit, command, setting),
        FixMode.Print,
        _ => histos.doctor.FixApply.Answer.Yes,
        Map("histos.scala" -> "old"),
      )
      val applied = histos.doctor.FixApply.applyAll(
        List(edit, command, setting),
        FixMode.from(true, true),
        _ => histos.doctor.FixApply.Answer.NonTty,
        Map("histos.scala" -> "old"),
      )
      val kinds = FixKind.values.toSet
      assertTrue(
        printed._1("histos.scala") == "old",
        printed._2.isEmpty,
        applied._1("histos.scala") == "new",
        applied._2 == List("./histos workflow --update"),
        !kinds.exists(_.toString.toLowerCase.contains("secret")),
        (
          SupplyDiagnostic
            .SecretMissing(
              histos.plugin.SecretName("HISTOS_APP_ID"),
              JobName("deps"),
              RepoName.trusted("early-effect/histos"),
              RepoOwner.Organization("early-effect"),
            )
            .fixes match
            case Fix.Settings(url, _) :: _ =>
              url.value.contains("organizations/early-effect/settings/secrets/actions") &&
              !url.value.contains("gh secret set")
            case _ => false
        ),
      )
  )

object SetupSpec extends ZIOSpecDefault:
  def spec = suite("SetupSpec")(
    test("inserting the policy is idempotent"):
      val source = "object build:\n      .docCheck(\n"
      val once   = SetupPlan.insert(source)
      assertTrue(once.contains(".supplyChain(SupplyChain())"), SetupPlan.insert(once) == once)
    ,
    test("a zipx file maps rows and keeps an unmapped pin"):
      val versions =
        """Lib("dev.zio::zio:2.1.27")
          |Plugin("pages")
          |Action("actions/checkout@v7")
          |Pin("sha256:abc", "pkg:generic/protoc@1")
          |""".stripMargin
      val report =
        ZipxImport.read(versions, "secrets ZIPX_APP_ID ZIPX_APP_PRIVATE_KEY Introduced", "npm\ndocker\ngithub-actions")
      assertTrue(
        report.mappedCount("Lib") == 1,
        report.mappedCount("Action") == 1,
        report.unmapped.exists(_.kind == "Pin"),
        report.gate.contains("Introduced"),
        report.secrets.contains("ZIPX_APP_ID"),
        report.todos.exists(_.contains("sha256:abc")),
      ),
  )
