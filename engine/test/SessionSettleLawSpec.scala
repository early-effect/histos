package histos.publish

import histos.version.{Branch, Catalog, CommitSha, Session, SessionSource, Ship, ShipName, ShipNumber}
import histos.workspace.Workspace
import histos.ProjectName

import zio.ZIO
import zio.test.*

object SessionSettleLawSpec extends ZIOSpecDefault:
  private val fixed = CommitSha.trusted("0123456789abcdef0123456789abcdef01234567")
  private val core  = ProjectName.trusted("core")
  private val histosName = ShipName.trusted("histos")
  private val number     = ShipNumber.trusted(0, 2, 1)
  private val row: Ship.Line = Ship.Line(histosName, number, ::(core, Nil)) match
    case line: Ship.Line => line
  private val catalog = Catalog.Lines(::(row, Nil))
  private val dev     = SessionSource(None, None, None, None)

  def spec = suite("session settle laws")(
    test("only a development local or snapshot write is settled"):
      val incomplete = SessionSource(Some("snapshot"), None, None, None)
      val release    = SessionSource(Some("release"), None, None, None)
      val named      = SessionSource(Some("development"), None, None, None)
      val snapshot   = SessionSource(Some("snapshot"), Some(fixed.value), Some("other"), None)
      val pull       = SessionSource(Some("pr"), Some(fixed.value), None, Some("7"))
      assertTrue(
        SessionSettle.settle(dev, PublishWrite.Local),
        SessionSettle.settle(dev, PublishWrite.Snapshots),
        SessionSettle.settle(named, PublishWrite.Snapshots),
        !SessionSettle.settle(dev, PublishWrite.Release),
        !SessionSettle.settle(release, PublishWrite.Local),
        !SessionSettle.settle(release, PublishWrite.Snapshots),
        !SessionSettle.settle(release, PublishWrite.Release),
        !SessionSettle.settle(incomplete, PublishWrite.Snapshots),
        !SessionSettle.settle(snapshot, PublishWrite.Local),
        !SessionSettle.settle(pull, PublishWrite.Snapshots),
        !SessionSettle.settle(SessionSource(Some("nope"), None, None, None), PublishWrite.Local),
      )
    ,
    test("an empty catalog is not asked for a commit"):
      Workspace.work("histos-settle-empty").flatMap: repo =>
        SessionSettle.prepare(repo, Catalog.Empty, Map.empty, PublishWrite.Snapshots).map: found =>
          assertTrue(found == Map.empty)
    ,
    test("a development write names HEAD, and publish snapshots moves the pointer off the default branch"):
      Workspace.work("histos-settle-head").flatMap: repo =>
        for
          sha   <- LawGit.commit(repo)
          local <- SessionSettle.prepare(repo, catalog, Map.empty, PublishWrite.Local)
          snaps <- SessionSettle.prepare(repo, catalog, Map("SONATYPE_USERNAME" -> "user"), PublishWrite.Snapshots)
        yield assertTrue(
          session(local) == Right(Session.Snapshot(sha, Branch.Other)),
          snaps.get("SONATYPE_USERNAME").contains("user"),
          session(snaps) == Right(Session.Snapshot(sha, Branch.Default)),
        )
    ,
    test("publish snapshots keeps an explicit sha and still moves the pointer"):
      Workspace.work("histos-settle-other").flatMap: repo =>
        val other = Map(
          "HISTOS_SESSION" -> "snapshot",
          "HISTOS_SHA"     -> fixed.value,
          "HISTOS_BRANCH"  -> "other",
        )
        for
          _     <- LawGit.commit(repo)
          snaps <- SessionSettle.prepare(repo, catalog, other, PublishWrite.Snapshots)
          local <- SessionSettle.prepare(repo, catalog, other, PublishWrite.Local)
        yield assertTrue(
          session(snaps) == Right(Session.Snapshot(fixed, Branch.Default)),
          local == other,
        )
    ,
    test("the recorded default branch moves the pointer"):
      Workspace.work("histos-settle-default").flatMap: repo =>
        for
          sha   <- LawGit.commit(repo, "main")
          _     <- LawGit.recordDefault(repo, "main")
          found <- SessionSettle.prepare(repo, catalog, Map.empty, PublishWrite.Snapshots)
        yield assertTrue(session(found) == Right(Session.Snapshot(sha, Branch.Default)))
    ,
    test("a release session is kept for a local write, and a release write is not settled"):
      Workspace.work("histos-settle-release").flatMap: repo =>
        val env = Map("HISTOS_SESSION" -> "release")
        for
          local   <- SessionSettle.prepare(repo, catalog, env, PublishWrite.Local)
          release <- SessionSettle.prepare(repo, catalog, Map.empty, PublishWrite.Release)
        yield assertTrue(local == env, release == Map.empty)
    ,
    test("a complete snapshot or pull-request session is kept"):
      Workspace.work("histos-settle-kept").flatMap: repo =>
        val snapshot = Map(
          "HISTOS_SESSION" -> "snapshot",
          "HISTOS_SHA"     -> fixed.value,
          "HISTOS_BRANCH"  -> "default",
        )
        val pull = Map(
          "HISTOS_SESSION" -> "pr",
          "HISTOS_SHA"     -> fixed.value,
          "HISTOS_PR"      -> "7",
        )
        for
          _        <- LawGit.commit(repo)
          keptSnap <- SessionSettle.prepare(repo, catalog, snapshot, PublishWrite.Local)
          keptPull <- SessionSettle.prepare(repo, catalog, pull, PublishWrite.Snapshots)
        yield assertTrue(keptSnap == snapshot, keptPull == pull)
    ,
    test("a directory with no commit does not name a snapshot"):
      Workspace.work("histos-settle-none").flatMap: repo =>
        SessionSettle.prepare(repo, catalog, Map.empty, PublishWrite.Snapshots).either.map: found =>
          assertTrue(found == Left(LocalPublishError.NoCommit))
    ,
  )

  private def session(env: Map[String, String]): Either[histos.version.SessionError, Session] =
    Session.from(
      SessionSource(
        env.get(Session.Key),
        env.get(Session.ShaKey),
        env.get(Session.BranchKey),
        env.get(Session.PullRequestKey),
      )
    )
end SessionSettleLawSpec
