package histos.version

import zio.test.*

object VersionTextLawSpec extends ZIOSpecDefault:
  private val number = ShipNumber.trusted(1, 4, 2)
  private val ship   = ShipName.trusted("core")
  private val sha    = CommitSha.trusted("0123456789abcdef0123456789abcdef01234567")
  private val stored = SnapshotCoordinate(number, sha.abbreviate)

  def spec = suite("version text laws")(
    test("a release line publishes the bare number, and a snapshot without a pointer does not invent one"):
      val release = PublishLine(ship, PublishAction.Release(ReleaseVersion(number)))
      val pointer = PublishLine(ship, PublishAction.Snapshot(CiVersion(number), stored, None))
      assertTrue(
        Decision.rendered(release) == "core compile 1.4.2 publish 1.4.2",
        Decision.rendered(pointer) == "core compile 1.4.2-ci publish 1.4.2-0123456789ab-SNAPSHOT",
        Decision.text(Decision.Publish(PublishKind.Release, ::(release, Nil))) ==
          "session release\ncore compile 1.4.2 publish 1.4.2",
      )
    ,
    test("an unchecked registry says the drift was not read"):
      val line = DevelopLine(ship, CiVersion(number))
      assertTrue(
        Decision.text(Decision.Develop(Drift.Unchecked, ::(line, Nil))) ==
          "session development\ndrift unchecked\ncore compile 1.4.2-ci"
      ),
  )
end VersionTextLawSpec
