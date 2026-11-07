package histos.supply

import histos.doctor.DoctorPlacement
import histos.plugin.{ActionName, ActionPin, JobName, RepoPath, SecretName}
import histos.version.CommitSha
import histos.{Developer, DeveloperId, PomText}

import zio.test.*

object SupplyPolicyLawSpec extends ZIOSpecDefault:
  private val id  = SecretName("HISTOS_APP_ID")
  private val key = SecretName("HISTOS_APP_PRIVATE_KEY")
  private val pat = SecretName("HISTOS_TOKEN")

  def spec = suite("supply policy laws")(
    test("an outage strategy unwraps a nested event until a leaf"):
      val nested = OutageStrategy.ByEvent(
        OutageStrategy.ByEvent(OutageStrategy.FailOpen, OutageStrategy.FailClosed, OutageStrategy.Cached(Hours(1))),
        OutageStrategy.FailClosed,
        OutageStrategy.FailClosed,
      )
      assertTrue(
        Event.values.forall(event =>
          OutageStrategy.forEvent(OutageStrategy.FailOpen, event) == OutageStrategy.FailOpen
        ),
        OutageStrategy.forEvent(OutageStrategy.default, Event.PullRequest) == OutageStrategy.Cached(Hours(24)),
        OutageStrategy.forEvent(OutageStrategy.default, Event.Push) == OutageStrategy.FailClosed,
        OutageStrategy.forEvent(OutageStrategy.default, Event.Schedule) == OutageStrategy.FailClosed,
        OutageStrategy.forEvent(nested, Event.PullRequest) == OutageStrategy.FailOpen,
        OutageStrategy.forEvent(nested, Event.Push) == OutageStrategy.FailClosed,
      )
    ,
    test("the named defaults are the policy a build gets when it sets nothing"):
      val weekly = UpdatePolicy().schedule
      assertTrue(
        SeverityGate.default == SeverityGate.Threshold(fail = Severity.High, annotate = Severity.Low),
        IgnoreRule.default == IgnoreRule.ReasonAndExpiry(Days(90)),
        Grouping.default == Grouping.OnePr(CommitSplit.PerKind),
        PrAuthor.default == PrAuthor.Actions,
        CommitSigning.default == CommitSigning.ApiSigned,
        UpdatePolicy().kinds == BumpKind.values.toSet,
        weekly.cron.value == "0 6 * * 1",
        weekly.zone.value == "America/Chicago",
        UpdatePolicy().branch.value == "histos/deps",
        SecurityFixes.default == SecurityFixes.OutOfBand(
          BranchName("histos/security"),
          FixPick.Smallest,
          Cooldown.Off,
        ),
        TransitiveFix.default == TransitiveFix.ParentThenForce(ForceCleanup.WhenParentCatchesUp),
        Snapshot.default == Snapshot.OnPushToDefault(JobName("dependency-snapshot")),
        MainAlert.default == MainAlert.Issue(LabelName("histos-advisory")),
        DailyAudit.default == DailyAudit.Off,
        daily(DailyAudit.sixCentral) == Right(("0 6 * * *", "America/Chicago")),
        AdvisoryPolicy().sources == ::(AdvisorySource.Osv, Nil),
        SupplyChain().placement == DoctorPlacement.default,
        DoctorPlacement.default == DoctorPlacement.PreflightPlusWorkflowJob,
      )
    ,
    test("an ignore is open or it names the date it ends"):
      val advisory = AdvisoryId("GHSA-1234-abcd-ef09")
      val because  = Reason("because")
      val until    = Date("2026-11-06")
      assertTrue(
        Ignore.until(advisory, until, because) == Ignore(advisory, because, Some(until)),
        Ignore.open(advisory, because) == Ignore(advisory, because, None),
      )
    ,
    test("a pull request author names exactly the secrets that author needs"):
      val authors = List(
        PrAuthor.Actions             -> Nil,
        PrAuthor.App(id, key)        -> List(id, key),
        PrAuthor.OwnerOpens(id, key) -> List(id, key),
        PrAuthor.Pat(pat)            -> List(pat),
      )
      assertTrue(
        authors.forall((author, secrets) => author.secretNames == secrets),
        PrAuthor.histosDeps == PrAuthor.App(id, key),
      )
    ,
    test("the first developer address is that id at the noreply domain"):
      val who = Developer(DeveloperId("early-effect"), PomText("Early Effect"))
      assertTrue(developerEmail(who).value == "early-effect@users.noreply.github.com")
    ,
    test("the token sources fail closed and do not query"):
      val packages = List(Purl.trusted("pkg:maven/dev.zio/zio@2"))
      for
        osv  <- AdvisorySource.Osv.query(packages).flip
        ghsa <- AdvisorySource.Ghsa.query(packages).flip
      yield assertTrue(
        osv == SupplyFault.ServiceDown(AdvisorySource.Osv.name, OutageStrategy.FailClosed),
        ghsa == SupplyFault.ServiceDown(AdvisorySource.Ghsa.name, OutageStrategy.FailClosed),
        AdvisorySource.Osv.name.value == "osv",
        AdvisorySource.Ghsa.name.value == "ghsa",
      )
    ,
    test("a moved action names both repository ids, and a down source does not name the strategy"):
      val action = ActionName("aws-actions/configure-aws-credentials")
      val was    = RepoId.trusted("111")
      val now    = RepoId.trusted("222")
      val moved  = SupplyFault.ActionRepoMoved(action, was, now)
      val down   = SupplyFault.ServiceDown(SourceName.trusted("osv"), OutageStrategy.FailOpen)
      assertTrue(
        moved.message.contains(was.value),
        moved.message.contains(now.value),
        down.message.contains("osv"),
        !down.message.contains("FailOpen"),
      )
    ,
    test("a human commit is the twelve character abbreviation, and a pin mismatch is eight characters"):
      val sha    = CommitSha.trusted("0123456789abcdef0123456789abcdef01234567")
      val other  = CommitSha.trusted("abcdef0123456789abcdef0123456789abcdef01")
      val branch = BranchName.trusted("main")
      val file   = RepoPath.trusted(".github/workflows/ci.yml")
      val action = ActionName("aws-actions/configure-aws-credentials")
      val human  = SupplyFault.HumanCommitsOnBranch(branch, sha :: other :: Nil)
      val head   = SupplyFault.HeadMoved(branch, sha)
      val pin    = SupplyFault.PinMismatch(file, 4, action, ActionPin(sha, "v1"), ActionPin(other, "v2"))
      assertTrue(
        human.message.contains(sha.abbreviate.value),
        human.message.contains(other.abbreviate.value),
        !human.message.contains("..."),
        sha.abbreviate.value == sha.value.take(12),
        head.message.contains(sha.value),
        pin.message.contains(s"${sha.value.take(8)}..."),
        pin.message.contains(s"${other.value.take(8)}..."),
        !pin.message.contains(sha.abbreviate.value),
        pin.message.contains("v1"),
        pin.message.contains("v2"),
      ),
  )

  private def daily(audit: DailyAudit): Either[DailyAudit, (String, String)] =
    audit match
      case DailyAudit.On(Schedule(cron, zone)) => Right((cron.value, zone.value))
      case DailyAudit.Off                      => Left(audit)
end SupplyPolicyLawSpec
