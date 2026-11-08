package histos.version

import histos.doctor.{Fix, Level}

import zio.test.*

object VersionDecisionLawSpec extends ZIOSpecDefault:
  private val sha            = CommitSha.trusted("0123456789abcdef0123456789abcdef01234567")
  private val histos         = ShipName.trusted("histos")
  private val api            = ShipName.trusted("api")
  private val other          = ShipName.trusted("other")
  private val core           = _root_.histos.ProjectName.trusted("core")
  private val app            = _root_.histos.ProjectName.trusted("app")
  private val one            = ShipNumber.trusted(1, 4, 2)
  private val two            = ShipNumber.trusted(1, 4, 3)
  private val row: Ship.Line = Ship.Line(histos, one, ::(core, Nil)) match
    case line: Ship.Line => line
  private val second: Ship.Line = Ship.Line(api, two, ::(app, Nil)) match
    case line: Ship.Line => line
  private val single = Catalog.Lines(::(row, Nil))
  private val both   = Catalog.Lines(::(row, List(second)))
  private val dev    = SessionSource(Some("development"), None, None, None)

  def spec = suite("version decision laws")(
    test("an empty catalog is no catalog before the session is read"):
      val garbage = SessionSource(Some("nope"), None, None, None)
      assertTrue(
        Versions.assess(Catalog.Empty, garbage, Evidence.Unchecked) == Right(Decision.NoCatalog),
        Versions.decide(Catalog.Empty, Session.Release, Evidence.Unchecked) == Decision.NoCatalog,
      )
    ,
    test("a line catalog refuses an unknown session and an unchecked publish"):
      val snap = SessionSource(Some("snapshot"), Some(sha.value), Some("default"), None)
      assertTrue(
        Versions.assess(single, SessionSource(Some("nope"), None, None, None), Evidence.Unchecked) ==
          Left(VersionError.Session(SessionError.Unknown("nope"))),
        Versions.assess(single, dev, Evidence.Unchecked) ==
          Right(Decision.Develop(Drift.Unchecked, ::(DevelopLine(histos, CiVersion(one)), Nil))),
        Versions.assess(single, snap, Evidence.Unchecked) ==
          Right(Decision.Refused(::(Refusal.Unchecked, Nil))),
      )
    ,
    test("assess refuses a registry ship the catalog does not name, and decide does not"):
      val extra = Registry.parse(List("other matches"))
      assertTrue(
        extra.map(reg => Versions.assess(single, dev, Evidence.Checked(reg))) ==
          Right(Left(VersionError.UnknownShip(other))),
        extra.map(reg => Versions.decide(single, Session.Development, Evidence.Checked(reg))) ==
          Right(Decision.Develop(Drift.Clear, ::(DevelopLine(histos, CiVersion(one)), Nil))),
      )
    ,
    test("a moved or unproven tag refuses development before a compile line exists"):
      val moved   = Registry.parse(List("histos moved"))
      val unknown = Registry.parse(List("histos unknown"))
      val clear   = Registry.parse(List("histos matches"))
      assertTrue(
        moved.map(reg => Versions.decide(single, Session.Development, Evidence.Checked(reg))) ==
          Right(Decision.Refused(::(Refusal.Shadowed(histos, one), Nil))),
        unknown.map(reg => Versions.decide(single, Session.Development, Evidence.Checked(reg))) ==
          Right(Decision.Refused(::(Refusal.Unproven(histos, one), Nil))),
        clear.map(reg => Versions.decide(single, Session.Development, Evidence.Checked(reg))) ==
          Right(Decision.Develop(Drift.Clear, ::(DevelopLine(histos, CiVersion(one)), Nil))),
      )
    ,
    test("refusals stay in ship order, and a release of only current ships publishes nothing"):
      val shadowed = Registry.parse(List("histos moved", "api unknown"))
      val current  = Registry.parse(List("histos matches", "api matches"))
      assertTrue(
        shadowed.map(reg => Versions.decide(both, Session.Release, Evidence.Checked(reg))) ==
          Right(Decision.Refused(::(Refusal.Shadowed(histos, one), List(Refusal.Unproven(api, two))))),
        current.map(reg => Versions.decide(both, Session.Release, Evidence.Checked(reg))) ==
          Right(Decision.Refused(::(Refusal.NothingToRelease, Nil))),
      )
    ,
    test("a release publishes the unreleased ship and holds the one already released"):
      val mixed = Registry.parse(List("histos matches"))
      val fresh = Registry.parse(Nil)
      assertTrue(
        mixed.map(reg => Versions.decide(both, Session.Release, Evidence.Checked(reg))) ==
          Right(publish(PublishKind.Release, holdRelease :: release :: Nil)),
        fresh.map(reg => Versions.decide(single, Session.Release, Evidence.Checked(reg))) ==
          Right(publish(PublishKind.Release, List(releaseOnly))),
      )
    ,
    test("a snapshot on the default branch moves the pointer, and every other publish does not"):
      val fresh = Registry.parse(Nil)
      val held  = Registry.parse(List("histos matches"))
      val pull  = PullRequestNumber.trusted(7)
      assertTrue(
        fresh.map(reg =>
          Versions.decide(single, Session.Snapshot(sha, Branch.Default), Evidence.Checked(reg))
        ) == Right(publish(PublishKind.Snapshot(sha, Branch.Default), List(pointer))),
        fresh.map(reg => Versions.decide(single, Session.Snapshot(sha, Branch.Other), Evidence.Checked(reg))) == Right(
          publish(PublishKind.Snapshot(sha, Branch.Other), List(stored))
        ),
        fresh.map(reg => Versions.decide(single, Session.PullRequest(pull, sha), Evidence.Checked(reg))) == Right(
          publish(PublishKind.PullRequest(pull, sha), List(stored))
        ),
        held.map(reg => Versions.decide(single, Session.Snapshot(sha, Branch.Default), Evidence.Checked(reg))) == Right(
          publish(PublishKind.Snapshot(sha, Branch.Default), List(everyday))
        ),
      )
    ,
    test("a tag is prefixed only when the catalog has more than one ship"):
      val rendered = one.render
      assertTrue(
        ReleaseTag.of(0, histos, rendered) == s"v$rendered",
        ReleaseTag.of(1, histos, rendered) == s"v$rendered",
        ReleaseTag.of(2, histos, rendered) == s"${histos.value}/v$rendered",
      )
    ,
    test("a ship finding is that check, and should-bump says whether the tag moved"):
      val session = ShipDiagnostic.Session("detail")
      val unread  = ShipDiagnostic.Releases("detail")
      val moved   = ShipDiagnostic.ShouldBump(histos, one, shadowed = true)
      val still   = ShipDiagnostic.ShouldBump(histos, one, shadowed = false)
      assertTrue(
        session.check.value == "ship.session",
        session.level == Level.Error,
        command(session) == Some(ShipAdvice.snapshotExport),
        unread.check.value == "ship.releases",
        unread.level == Level.Error,
        command(unread) == Some("touch releases"),
        moved.check.value == "ship.should-bump",
        moved.level == Level.Warning,
        still.level == Level.Warning,
        moved.summary.contains("has changes"),
        still.summary.contains("already released"),
        !still.summary.contains("has changes"),
        steps(moved).nonEmpty,
        Catalog.Empty.get(histos).isEmpty,
        single.get(histos).contains(row),
        single.get(api).isEmpty,
      ),
  )

  private def publish(kind: PublishKind, lines: List[PublishLine]): Decision =
    lines match
      case head :: tail => Decision.Publish(kind, ::(head, tail))
      case Nil          => Decision.NoCatalog

  private def holdRelease: PublishLine =
    PublishLine(histos, PublishAction.Hold(HoldCompile.Release(ReleaseVersion(one)), HoldReason.AlreadyReleased))

  private def release: PublishLine =
    PublishLine(api, PublishAction.Release(ReleaseVersion(two)))

  private def releaseOnly: PublishLine =
    PublishLine(histos, PublishAction.Release(ReleaseVersion(one)))

  private def pointer: PublishLine =
    PublishLine(
      histos,
      PublishAction.Snapshot(
        CiVersion(one),
        SnapshotCoordinate(one, sha.abbreviate),
        Some(SnapshotPointer(one)),
      ),
    )

  private def stored: PublishLine =
    PublishLine(
      histos,
      PublishAction.Snapshot(CiVersion(one), SnapshotCoordinate(one, sha.abbreviate), None),
    )

  private def everyday: PublishLine =
    PublishLine(
      histos,
      PublishAction.Hold(HoldCompile.Everyday(CiVersion(one)), HoldReason.AlreadyReleased),
    )

  private def command(diagnostic: ShipDiagnostic): Option[String] =
    diagnostic.fixes.toList.collectFirst { case Fix.Command(line) => line.value }

  private def steps(diagnostic: ShipDiagnostic): List[String] =
    diagnostic.fixes.toList.collect { case Fix.Steps(head :: _) => head }
end VersionDecisionLawSpec
