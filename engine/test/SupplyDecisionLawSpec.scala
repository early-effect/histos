package histos.supply

import histos.plugin.SecretName
import histos.version.CommitSha
import histos.{Developer, DeveloperId, PomText}

import zio.*
import zio.test.*

object SupplyDecisionLawSpec extends ZIOSpecDefault:
  private val branch   = BranchName.trusted("histos/deps")
  private val main     = CommitSha.trusted("a" * 40)
  private val other    = CommitSha.trusted("b" * 40)
  private val app      = PrAuthor.App(SecretName("HISTOS_APP_ID"), SecretName("HISTOS_APP_PRIVATE_KEY"))
  private val workflow = ".github/workflows/ci.yml" -> "name: ci"
  private val lockFile = "dependencies.lock"        -> "cell core"
  private val today    = Date.trusted("2026-01-10")
  private val coord    = Coordinate.trusted("org.example:demo")
  private val purl     = Purl.trusted("pkg:maven/org.example/demo@1.0.0")
  private val advisory = AdvisoryId.trusted("GHSA-1111-2222-3333")
  private val source   = SourceName.trusted("osv")

  def spec = suite("supply decision laws")(
    test("Actions keeps a workflow file for nobody, and closes an open pull request that has nothing left"):
      val only = List(KindChange(BumpKind.Actions, List(workflow)))
      assertTrue(
        plan(PrAuthor.Actions, only, openPr = false) == Right(Nil),
        plan(PrAuthor.Actions, only, openPr = true) == Right(List(GhCall.Close(branch))),
      )
    ,
    test("Actions drops the workflow file, keeps the lock, and tells a person to regenerate"):
      val mixed   = List(KindChange(BumpKind.Libraries, List(workflow, lockFile)))
      val kept    = plan(PrAuthor.Actions, mixed, openPr = true)
      val appKept = plan(app, mixed, openPr = true)
      assertTrue(
        bodyOf(kept).exists(text => text.contains("Run ./histos workflow") && text.contains(main.value)),
        filesOf(kept) == List(lockFile),
        bodyOf(appKept).exists(text => !text.contains("Run ./histos workflow") && text.contains(main.value)),
        filesOf(appKept) == List(workflow, lockFile),
      )
    ,
    test("a human commit is only a comment when the policy says so, and a rebase still commits without moving the ref"):
      val change  = List(KindChange(BumpKind.Libraries, List(lockFile)))
      val comment = plan(app, change, human = List(other), edits = HumanEdits.CommentOnly)
      val rebase  = plan(app, change, human = List(other), edits = HumanEdits.Rebase)
      assertTrue(
        comment match
          case Right(GhCall.Comment(text) :: Nil) => text.contains(other.abbreviate.value)
          case _                                  => false
        ,
        rebase match
          case Right(calls) =>
            !calls.exists:
              case GhCall.UpdateRef(_, _, _) => true
              case _                         => false
            &&
            calls.exists:
              case GhCall.CommitOnBranch(_, _, _, _) => true
              case _                                 => false
            &&
            calls.exists:
              case GhCall.OpenOrUpdate(_, _) => true
              case _                         => false
          case _ => false,
      )
    ,
    test("an api signature updates the ref then commits, and a push is signed only for ssh"):
      val change = List(KindChange(BumpKind.Libraries, List(lockFile)))
      val api    = plan(app, change, signing = CommitSigning.ApiSigned)
      val plain  = plan(app, change, signing = CommitSigning.GitPush(named))
      val ssh    = plan(app, change, signing = CommitSigning.GitPushSshSigned(named, SecretName("HISTOS_TOKEN")))
      assertTrue(
        api match
          case Right(
                GhCall.UpdateRef(_, sha, true) :: GhCall.CommitOnBranch(_, expected, _, _) :: GhCall.OpenOrUpdate(
                  _,
                  _,
                ) :: Nil
              ) =>
            sha == main && expected == main
          case _ => false
        ,
        plain match
          case Right(GhCall.UpdateRef(_, _, _) :: GhCall.Push(_, false) :: GhCall.OpenOrUpdate(_, _) :: Nil) => true
          case _                                                                                             => false
        ,
        ssh match
          case Right(GhCall.UpdateRef(_, _, _) :: GhCall.Push(_, true) :: GhCall.OpenOrUpdate(_, _) :: Nil) => true
          case _                                                                                            => false,
      )
    ,
    test("the first developer is a push only when a developer was read, and a moved head is not the planned commit"):
      val change  = List(KindChange(BumpKind.Libraries, List(lockFile)))
      val missing = plan(app, change, signing = CommitSigning.GitPush(Identity.FirstDeveloper))
      val present = plan(
        app,
        change,
        signing = CommitSigning.GitPush(Identity.FirstDeveloper),
        developer = Some(Developer(DeveloperId.trusted("ada"), PomText.trusted("Ada"))),
      )
      assertTrue(
        missing == Left(SupplyFault.IdentityMissing),
        present match
          case Right(GhCall.UpdateRef(_, _, _) :: GhCall.Push(_, false) :: _ :: Nil) => true
          case _                                                                     => false
        ,
        PrFlow.committed(branch, main, other) == Left(SupplyFault.HeadMoved(branch, main)),
        PrFlow.committed(branch, main, main) == Right(main),
        PrFlow.marker(main).contains(main.value),
      )
    ,
    test("a bump applies a newer patch, lists a major, and applies a major only when the policy says so"):
      val locked = row("1.0.0")
      val none   =
        Bump.choose(List(locked), List(release("1.0.0", today)), versions(), today, false, TransitiveFix.ParentOnly)
      val patch =
        Bump.choose(List(locked), List(release("1.0.1", today)), versions(), today, false, TransitiveFix.ParentOnly)
      val listed =
        Bump.choose(List(locked), List(release("2.0.0", today)), versions(), today, false, TransitiveFix.ParentOnly)
      val apply = Bump.choose(
        List(locked),
        List(release("2.0.0", today)),
        versions(majors = Majors.Apply),
        today,
        false,
        TransitiveFix.ParentOnly,
      )
      assertTrue(
        none == Right(BumpPlan(Nil, Nil)),
        applied(patch) == List("1.0.1"),
        listedOnly(listed) == List("2.0.0"),
        applied(apply) == List("2.0.0"),
      )
    ,
    test("a security bump is the smallest security release, and two versions of one coordinate are a split"):
      val locked = row("1.0.0")
      val small  = release("1.0.2", today).copy(security = Some(advisory))
      val large  = release("1.2.0", today).copy(security = Some(advisory))
      val picked = Bump.choose(
        List(locked),
        List(large, small),
        versions(cooldown = Cooldown.Off),
        today,
        true,
        TransitiveFix.ParentOnly,
      )
      val parent = Coordinate.trusted("org.example:parent")
      val split  = Bump.choose(
        List(
          locked.copy(coordinate = parent, cell = "app"),
          locked,
          locked.copy(version = "1.1.0", cell = "other"),
          locked.copy(cell = "web"),
          locked.copy(coordinate = parent, version = "2.0.0", cell = "core"),
        ),
        Nil,
        versions(),
        today,
        false,
        TransitiveFix.ParentOnly,
      )
      assertTrue(
        applied(picked) == List("1.0.2"),
        split == Left(
          SupplyFault.CrossVersionSplit(parent, List(CellName.trusted("app"), CellName.trusted("core")))
        ),
        Bump.choose(
          List(locked, locked.copy(version = "1.1.0", cell = "other")),
          Nil,
          versions(),
          today,
          false,
          TransitiveFix.ParentOnly,
        ) ==
          Left(SupplyFault.CrossVersionSplit(coord, List(CellName.trusted("core"), CellName.trusted("other")))),
      )
    ,
    test("a release published today waits out a cooldown unless it is a security fix"):
      val locked  = row("1.0.0")
      val fresh   = release("1.0.1", today)
      val policy  = versions(cooldown = Cooldown.After(Days.trusted(3)))
      val waited  = Bump.choose(List(locked), List(fresh), policy, today, false, TransitiveFix.ParentOnly)
      val secured = Bump.choose(
        List(locked),
        List(fresh.copy(security = Some(advisory))),
        policy,
        today,
        true,
        TransitiveFix.ParentOnly,
      )
      assertTrue(waited == Right(BumpPlan(Nil, Nil)), applied(secured) == List("1.0.1"))
    ,
    test("a pre-release is skipped, followed only on a pre-release line, or allowed"):
      val stable = row("1.0.0")
      val early  = row("1.0.0-RC1")
      val next   = release("1.1.0-RC1", today).copy(preRelease = true)
      val skip   =
        Bump.choose(List(stable), List(next), versions(pre = PreRelease.Skip), today, false, TransitiveFix.ParentOnly)
      val follow = Bump.choose(
        List(stable),
        List(next),
        versions(pre = PreRelease.FollowCurrentLine),
        today,
        false,
        TransitiveFix.ParentOnly,
      )
      val stay = Bump.choose(
        List(early),
        List(next),
        versions(pre = PreRelease.FollowCurrentLine),
        today,
        false,
        TransitiveFix.ParentOnly,
      )
      val allow =
        Bump.choose(List(stable), List(next), versions(pre = PreRelease.Allow), today, false, TransitiveFix.ParentOnly)
      assertTrue(
        skip == Right(BumpPlan(Nil, Nil)),
        follow == Right(BumpPlan(Nil, Nil)),
        applied(stay) == List("1.1.0-RC1"),
        applied(allow) == List("1.1.0-RC1"),
      )
    ,
    test("a zero minor is a major only when the policy says so"):
      val locked = row("0.1.0")
      val next   = release("0.2.0", today)
      val minor  = Bump.choose(
        List(locked),
        List(next),
        versions(zero = ZeroX.MinorIsMinor),
        today,
        false,
        TransitiveFix.ParentOnly,
      )
      val major = Bump.choose(
        List(locked),
        List(next),
        versions(zero = ZeroX.MinorIsMajor),
        today,
        false,
        TransitiveFix.ParentOnly,
      )
      assertTrue(applied(minor) == List("0.2.0"), listedOnly(major) == List("0.2.0"))
    ,
    test("a direct is never forced, a parent that already has the version is not forced, and report-only is listed"):
      val direct = row("1.0.0")
      val child  = row("1.0.0", direct = false)
      val next   = release("1.0.1", today)
      val parent = Release(Coordinate.trusted("org.example:parent"), "1.0.1", today, None, false)
      val forced =
        Bump.choose(List(child), List(next), versions(), today, false, TransitiveFix.ForceOnly(ForceCleanup.Never))
      val directFix =
        Bump.choose(List(direct), List(next), versions(), today, false, TransitiveFix.ForceOnly(ForceCleanup.Never))
      val caught = Bump.choose(
        List(child),
        List(next, parent),
        versions(),
        today,
        false,
        TransitiveFix.ParentThenForce(ForceCleanup.WhenParentCatchesUp),
      )
      val reported = Bump.choose(List(child), List(next), versions(), today, false, TransitiveFix.ReportOnly)
      assertTrue(
        forcedFlag(forced).contains(true),
        forcedFlag(directFix).contains(false),
        forcedFlag(caught).contains(false),
        listedOnly(reported) == List("1.0.1"),
      )
    ,
    test("a severity word wins over a vector, and an unread word is low"):
      assertTrue(
        Audit.classify(Some("HIGH"), None) == Severity.High,
        Audit.classify(Some("medium"), None) == Severity.Moderate,
        Audit.classify(Some("low"), Some(Cvss.trusted("CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:H/I:H/A:H"))) == Severity.Low,
        Audit.classify(None, None) == Severity.Low,
      )
    ,
    test("an outage fails closed, drops findings when open, and keeps gating a fresh cache"):
      val high  = finding(Severity.High)
      val open  = policy(outage = OutageStrategy.FailOpen)
      val shut  = policy(outage = OutageStrategy.FailClosed)
      val held  = policy(outage = OutageStrategy.Cached(Hours.trusted(24)))
      val down  = Some((source, Hours.trusted(1)))
      val stale = Some((source, Hours.trusted(25)))
      assertTrue(
        Audit.evaluate(List(high), shut, Event.Push, Set(purl), today, down) ==
          Left(SupplyFault.ServiceDown(source, shut.outage)),
        Audit.evaluate(List(high), open, Event.Push, Set(purl), today, down) == Right(AuditResult(Nil, Nil)),
        Audit.evaluate(List(high), held, Event.Push, Set(purl), today, down) match
          case Right(AuditResult(SupplyFault.Advisory(found) :: Nil, Nil)) => found.id == advisory
          case _                                                           => false
        ,
        Audit.evaluate(List(high), held, Event.Push, Set(purl), today, stale) ==
          Left(SupplyFault.ServiceDown(source, held.outage)),
        OutageStrategy.forEvent(OutageStrategy.default, Event.PullRequest) == OutageStrategy.Cached(Hours.trusted(24)),
      )
    ,
    test("a high finding fails, a low finding is a note, an unknown rank is silent, and annotate-only never fails"):
      val gate = policy()
      val note = policy(gate = SeverityGate.AnnotateOnly)
      assertTrue(
        failed(
          Audit.evaluate(
            List(finding(Severity.High), finding(Severity.Critical)),
            gate,
            Event.Push,
            Set(purl),
            today,
            None,
          )
        ),
        noted(
          Audit
            .evaluate(List(finding(Severity.Low), finding(Severity.Moderate)), gate, Event.Push, Set(purl), today, None)
        ),
        Audit.evaluate(List(finding(Severity.Unknown)), gate, Event.Push, Set(purl), today, None) == Right(
          AuditResult(Nil, Nil)
        ),
        Audit.evaluate(List(finding(Severity.Critical)), note, Event.Push, Set(purl), today, None) match
          case Right(AuditResult(Nil, annotated)) => annotated.map(_.severity) == List(Severity.Critical)
          case _                                  => false,
      )
    ,
    test("introduced scope drops a finding that was not introduced"):
      val otherPurl = Purl.trusted("pkg:maven/org.example/other@1.0.0")
      val result    = Audit.evaluate(
        List(finding(Severity.High).copy(affected = List(otherPurl))),
        policy(scope = GateScope.Introduced),
        Event.Push,
        Set(purl),
        today,
        None,
      )
      assertTrue(result == Right(AuditResult(Nil, Nil)))
    ,
    test("an ignore is a fault when disallowed, expired, open, or too long, and a reason drops the finding"):
      val high    = finding(Severity.High)
      val reason  = Reason.trusted("accepted")
      val banned  = policy(ignoreRule = IgnoreRule.Disallowed, ignores = List(Ignore.open(advisory, reason)))
      val empty   = policy(ignoreRule = IgnoreRule.Disallowed)
      val dropped = policy(ignoreRule = IgnoreRule.ReasonOnly, ignores = List(Ignore.open(advisory, reason)))
      val open    = policy(ignores = List(Ignore.open(advisory, reason)))
      val expired = policy(ignores = List(Ignore.until(advisory, Date.trusted("2026-01-01"), reason)))
      val long    = policy(ignores = List(Ignore.until(advisory, Date.trusted("2026-06-10"), reason)))
      val held    = policy(ignores = List(Ignore.until(advisory, Date.trusted("2026-02-01"), reason)))
      assertTrue(
        Audit.evaluate(List(high), banned, Event.Push, Set(purl), today, None) match
          case Left(SupplyFault.IgnoresDisallowed(ids)) => ids == List(advisory)
          case _                                        => false
        ,
        Audit.evaluate(List(high), empty, Event.Push, Set(purl), today, None) match
          case Right(AuditResult(_ :: Nil, _)) => true
          case _                               => false
        ,
        Audit.evaluate(List(high), dropped, Event.Push, Set(purl), today, None) == Right(AuditResult(Nil, Nil)),
        open match
          case _ =>
            Audit.evaluate(List(high), open, Event.Push, Set(purl), today, None) ==
              Left(SupplyFault.IgnoreNeedsExpiry(advisory))
        ,
        Audit.evaluate(List(high), expired, Event.Push, Set(purl), today, None) ==
          Left(SupplyFault.IgnoreExpired(advisory, Date.trusted("2026-01-01"))),
        Audit.evaluate(List(high), long, Event.Push, Set(purl), today, None) ==
          Left(SupplyFault.IgnoreTooLong(advisory, Days.trusted(90))),
        Audit.evaluate(List(high), held, Event.Push, Set(purl), today, None) == Right(AuditResult(Nil, Nil)),
      )
    ,
    test("a cached advisory is not hydrated again, and a new modified is"):
      val stored  = finding(Severity.High).copy(summary = "stored", modified = "same")
      val listed  = stored.copy(summary = "listed")
      val program =
        for
          hits <- Ref.make(0)
          seen <- Ref.make(List.empty[Purl])
          hit  <- Audit.fetch(
            List(purl),
            Map(advisory -> ("same", stored)),
            purls => seen.set(purls) *> ZIO.succeed(List(listed)),
            _ => hits.update(_ + 1).as(listed.copy(summary = "full")),
          )
          changed <- Audit.fetch(
            List(purl),
            Map(advisory -> ("old", stored)),
            _ => ZIO.succeed(List(listed.copy(modified = "new"))),
            found => hits.update(_ + 1).as(found.copy(summary = "full")),
          )
          count <- hits.get
          asked <- seen.get
        yield assertTrue(
          hit._1.map(_.summary) == List("stored"),
          changed._1.map(_.summary) == List("full"),
          count == 1,
          asked == List(purl),
        )
      program
    ,
    test("an alert opens what is new, updates what remains, and closes what is gone"):
      val kept    = AdvisoryId.trusted("GHSA-2222-3333-4444")
      val created = AdvisoryId.trusted("GHSA-3333-4444-5555")
      val closed  = AdvisoryId.trusted("GHSA-4444-5555-6666")
      val moves   = Alert.issues(Set(kept, closed), Set(kept, created)).toSet
      assertTrue(
        moves == Set(Alert.Move.Open(created), Alert.Move.Update(kept), Alert.Move.Close(closed))
      )
    ,
    test("a lock artifact is a purl, a test cell is development, and a checksum edit does not change the purl"):
      val direct = SnapshotSubmit.fromLock(lockText("1.0.0", hex))
      val test   = SnapshotSubmit.fromLock(
        s"""test core 3.3.7 jvm
           |artifact org.example demo 1.0.0 $hex central
           |""".stripMargin
      )
      val bumped = SnapshotSubmit.fromLock(lockText("1.2.3", hex))
      val hashed = SnapshotSubmit.fromLock(lockText("1.0.0", "c" * 64))
      val body   = SnapshotSubmit.json(direct, main.value, "refs/heads/main")
      assertTrue(
        direct.file == "dependencies.lock",
        direct.packages == List(SnapshotPackage("pkg:maven/org.example/demo@1.0.0", "direct", "runtime")),
        test.packages == List(SnapshotPackage("pkg:maven/org.example/demo@1.0.0", "indirect", "development")),
        bumped.packages.map(_.purl) == List("pkg:maven/org.example/demo@1.2.3"),
        hashed.packages == direct.packages,
        body.contains("pkg:maven/org.example/demo@1.0.0"),
        body.contains(""""relationship":"direct""""),
        body.contains(""""scope":"runtime""""),
      ),
  )

  private val hex = "ab" * 32

  private val named = Identity.Named(PersonName.trusted("Ada Lovelace"), Email.trusted("ada@example.com"))

  private def lockText(version: String, sha: String): String =
    s"""cell core 3.3.7 jvm
       |direct org.example demo $version java
       |artifact org.example demo $version $sha central
       |""".stripMargin

  private def plan(
      author: PrAuthor,
      changes: List[KindChange],
      openPr: Boolean = true,
      human: List[CommitSha] = Nil,
      edits: HumanEdits = HumanEdits.Rebase,
      signing: CommitSigning = CommitSigning.ApiSigned,
      developer: Option[Developer] = None,
  ): Either[SupplyFault, List[GhCall]] =
    PrFlow.plan(
      PrRequest(
        UpdatePolicy(author = author, signing = signing, humanEdits = edits),
        branch,
        main,
        None,
        changes,
        human,
        openPr,
        developer,
      )
    )

  private def bodyOf(result: Either[SupplyFault, List[GhCall]]): Option[String] =
    result.toOption.flatMap: calls =>
      calls.collectFirst:
        case GhCall.OpenOrUpdate(_, body) => body

  private def filesOf(result: Either[SupplyFault, List[GhCall]]): List[(String, String)] =
    result.toOption.toList.flatMap: calls =>
      calls
        .collect:
          case GhCall.CommitOnBranch(_, _, _, files) => files
        .flatten

  private def row(version: String, direct: Boolean = true): LockedDependency =
    LockedDependency(coord, version, purl, direct, false, "core")

  private def release(version: String, published: Date): Release =
    Release(coord, version, published, None, false)

  private def versions(
      cooldown: Cooldown = Cooldown.Off,
      pre: PreRelease = PreRelease.Skip,
      majors: Majors = Majors.ListOnly,
      zero: ZeroX = ZeroX.MinorIsMinor,
  ): VersionPolicy =
    VersionPolicy(cooldown, pre, majors, zero)

  private def applied(result: Either[SupplyFault, BumpPlan]): List[String] =
    result.toOption.toList.flatMap(_.apply.map(_.to.version))

  private def listedOnly(result: Either[SupplyFault, BumpPlan]): List[String] =
    result.toOption.toList.flatMap(_.listed.map(_.to.version))

  private def forcedFlag(result: Either[SupplyFault, BumpPlan]): Option[Boolean] =
    result.toOption.flatMap(_.apply.map(_.forced).headOption)

  private def finding(severity: Severity): Finding =
    Finding(advisory, severity, None, List(purl), Nil, "summary", "mod")

  private def policy(
      gate: SeverityGate = SeverityGate.Threshold(Severity.High, Severity.Low),
      scope: GateScope = GateScope.All,
      outage: OutageStrategy = OutageStrategy.FailClosed,
      ignoreRule: IgnoreRule = IgnoreRule.ReasonAndExpiry(Days.trusted(90)),
      ignores: List[Ignore] = Nil,
  ): AdvisoryPolicy =
    AdvisoryPolicy(gate = gate, scope = scope, outage = outage, ignoreRule = ignoreRule, ignores = ignores)

  private def failed(result: Either[SupplyFault, AuditResult]): Boolean =
    result match
      case Right(AuditResult(failures, Nil)) => failures.nonEmpty
      case _                                 => false

  private def noted(result: Either[SupplyFault, AuditResult]): Boolean =
    result match
      case Right(AuditResult(Nil, notes)) => notes.nonEmpty
      case _                              => false
end SupplyDecisionLawSpec
