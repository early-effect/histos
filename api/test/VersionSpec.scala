package histos.version

import histos.{ProjectName, ProjectNameError}

import zio.{ConfigProvider, IO, ZIO}
import zio.test.*

object VersionSpec extends ZIOSpecDefault:
  private val number       = ShipNumber("1.4.2")
  private val clientNumber = ShipNumber("0.3.0")
  private val sha          = CommitSha("1234567890abcdef1234567890abcdef12345678")
  private val abbrev       = sha.abbreviate

  private enum FixtureError:
    case Ship(error: ShipNameError)
    case Project(error: ProjectNameError)
    case Pull(error: PullRequestError)
    case Rows(error: CatalogError)

  private final case class Fixture(
      libs: ShipName,
      client: ShipName,
      core: ProjectName,
      one: Catalog,
      both: Catalog,
      pull: PullRequestNumber,
  )

  private val opened: Either[FixtureError, Fixture] =
    for
      libs   <- ShipName.from("libs").left.map(FixtureError.Ship(_))
      client <- ShipName.from("client").left.map(FixtureError.Ship(_))
      core   <- ProjectName.from("core").left.map(FixtureError.Project(_))
      tests  <- ProjectName.from("tests").left.map(FixtureError.Project(_))
      pull   <- PullRequestNumber.parse("42").left.map(FixtureError.Pull(_))
      one    <- Catalog.from(List(Ship.Line(libs, number, ::(core, Nil)))).left.map(FixtureError.Rows(_))
      both   <- Catalog
        .from(List(Ship.Line(libs, number, ::(core, Nil)), Ship.Line(client, clientNumber, ::(tests, Nil))))
        .left
        .map(FixtureError.Rows(_))
    yield Fixture(libs, client, core, one, both, pull)

  private def use(f: Fixture => TestResult): IO[FixtureError, TestResult] =
    ZIO.fromEither(opened).map(f)

  private val clear = Evidence.Checked(Registry.empty)

  private def registry(lines: String*): Either[RegistryError, Evidence] =
    Registry.parse(lines.toList).map(Evidence.Checked(_))

  private val generated =
    for
      major <- Gen.int(0, 12)
      minor <- Gen.int(0, 12)
      patch <- Gen.int(0, 12)
      hex   <- Gen.listOfN(40)(Gen.elements("0123456789abcdef"*))
    yield (ShipNumber.trusted(major, minor, patch), CommitSha.trusted(hex.mkString))

  private val pins = suite("pins")(
    test("a pin is a stored snapshot or a release"):
      val stored = SnapshotCoordinate(number, abbrev).render
      assertTrue(
        abbrev.value == "1234567890ab",
        Pin.from(stored) == Right(Pin.Snapshot(SnapshotCoordinate(number, abbrev))),
        Pin.from(number.render) == Right(Pin.Release(ReleaseVersion(number))),
        Pin.from(SnapshotPointer(number).render) == Left(PinError.Pointer(SnapshotPointer(number))),
        Pin.from(CiVersion(number).render) == Left(PinError.CompileOnly(CiVersion(number))),
        Pin.from(s"${number.render}-${abbrev.value}") == Left(PinError.Unstored(number, abbrev)),
        Pin.from(s"${number.render}-abc") == Left(PinError.ShortSha("abc")),
        Pin.from("") == Left(PinError.Empty),
        CommitSha.from(sha.value.toUpperCase) == Right(sha),
        AbbrevSha.from(abbrev.value.toUpperCase) == Left(AbbrevShaError.NotLower(abbrev.value.toUpperCase)),
      )
    ,
    test("every canonical ship number round-trips, and a short sha is not stored"):
      check(generated): (ship, commit) =>
        val stored = SnapshotCoordinate(ship, commit.abbreviate)
        assertTrue(
          ShipNumber.from(ship.render) == Right(ship),
          Pin.from(stored.render) == Right(Pin.Snapshot(stored)),
          Pin.from(SnapshotPointer(ship).render) == Left(PinError.Pointer(SnapshotPointer(ship))),
          Pin.from(CiVersion(ship).render) == Left(PinError.CompileOnly(CiVersion(ship))),
        ),
  )

  private val edge = suite("edge")(
    test("development is the absent session, and a snapshot without a commit is refused"):
      val snapshot = SessionSource(Some("snapshot"), Some(sha.value), Some("default"), None)
      assertTrue(
        Session.from(SessionSource(None, None, None, None)) == Right(Session.Development),
        Session.from(SessionSource(Some("development"), None, None, None)) == Right(Session.Development),
        Session.from(SessionSource(Some("snapshot"), None, None, None)) == Left(SessionError.MissingSha),
        Session.from(SessionSource(Some("snapshot"), Some("abc"), None, None)) == Left(
          SessionError.Sha(CommitShaError.NotHex("abc"))
        ),
        Session.from(snapshot) == Right(Session.Snapshot(sha, Branch.Default)),
        Session.from(SessionSource(Some("pr"), Some(sha.value), None, None)) == Left(SessionError.MissingPullRequest),
        Session.from(SessionSource(Some("release"), None, None, None)) == Right(Session.Release),
        Session.from(SessionSource(Some("nope"), None, None, None)) == Left(SessionError.Unknown("nope")),
        PullRequestNumber.parse("01") == Left(PullRequestError.NotANumber("01")),
        PullRequestNumber.from(0) == Left(PullRequestError.NotPositive(0)),
      )
    ,
    test("a map provider loads the four session keys and leaves a missing key absent"):
      val env = Map(
        Session.Key            -> "snapshot",
        Session.ShaKey         -> sha.value,
        Session.BranchKey      -> "default",
        Session.PullRequestKey -> "7",
        "PATH"                 -> "/usr/bin",
      )
      val blank = Map(Session.Key -> "", Session.ShaKey -> "", Session.BranchKey -> "", Session.PullRequestKey -> "")
      for
        loaded <- ConfigProvider.fromMap(env).load(SessionSource.config)
        absent <- ConfigProvider.fromMap(Map.empty).load(SessionSource.config)
        empty  <- ConfigProvider.fromMap(blank).load(SessionSource.config)
      yield assertTrue(
        loaded == SessionSource(Some("snapshot"), Some(sha.value), Some("default"), Some("7")),
        absent == SessionSource(None, None, None, None),
        empty == SessionSource(Some(""), Some(""), Some(""), Some("")),
      )
    ,
    test("a registry line names a ship and a relation, and a duplicate is refused"):
      use: fixture =>
        val parsed = Registry.parse(List("libs matches", "# note", ""))
        assertTrue(
          parsed.map(_.relation(fixture.libs)) == Right(Some(TagRelation.Matches)),
          Registry.parse(List("libs matches", "# note", "", "libs moved")) == Left(
            RegistryError.Duplicate(fixture.libs)
          ),
          Registry.parse(List("libs sideways")) == Left(
            RegistryError.Relation(TagRelationError.Unknown("sideways"))
          ),
          Registry.parse(List("1.4.2 matches")) == Left(RegistryError.Name(ShipNameError.Bad("1.4.2"))),
        ),
  )

  private val transitions = suite("transitions")(
    test("development compiles -ci, and an unchecked registry is not a passed drift gate"):
      use: fixture =>
        val checked = Versions.decide(fixture.one, Session.Development, clear)
        val missed  = Versions.decide(fixture.one, Session.Development, Evidence.Unchecked)
        val compile = CiVersion(number)
        val lines   = ::(DevelopLine(fixture.libs, compile), Nil)
        assertTrue(
          compile.render == "1.4.2-ci",
          checked == Decision.Develop(Drift.Clear, lines),
          missed == Decision.Develop(Drift.Unchecked, lines),
          Decision.text(missed).contains("drift unchecked"),
        )
    ,
    test("a default-branch snapshot stores the commit and names the pointer; another branch does not"):
      use: fixture =>
        val onDefault = Versions.decide(fixture.one, Session.Snapshot(sha, Branch.Default), clear)
        val elsewhere = Versions.decide(fixture.one, Session.Snapshot(sha, Branch.Other), clear)
        val stored    = SnapshotCoordinate(number, abbrev)
        assertTrue(
          stored.render == "1.4.2-1234567890ab-SNAPSHOT",
          onDefault == Decision.Publish(
            PublishKind.Snapshot(sha, Branch.Default),
            ::(
              PublishLine(
                fixture.libs,
                PublishAction.Snapshot(CiVersion(number), stored, Some(SnapshotPointer(number))),
              ),
              Nil,
            ),
          ),
          elsewhere == Decision.Publish(
            PublishKind.Snapshot(sha, Branch.Other),
            ::(PublishLine(fixture.libs, PublishAction.Snapshot(CiVersion(number), stored, None)), Nil),
          ),
        )
    ,
    test("a pull request stores the same commit and cannot name the pointer"):
      use: fixture =>
        val decision = Versions.decide(fixture.one, Session.PullRequest(fixture.pull, sha), clear)
        val stored   = SnapshotCoordinate(number, abbrev)
        assertTrue(
          fixture.pull.value == 42,
          decision == Decision.Publish(
            PublishKind.PullRequest(fixture.pull, sha),
            ::(PublishLine(fixture.libs, PublishAction.Snapshot(CiVersion(number), stored, None)), Nil),
          ),
        )
    ,
    test("a released ship that still matches publishes nothing, and a moved or unproven ship publishes nothing at all"):
      use: fixture =>
        val current = registry(s"${fixture.libs.value} matches")
        val changed = registry(s"${fixture.libs.value} moved")
        val unknown = registry(s"${fixture.libs.value} unknown")
        val held    =
          current.map(evidence => Versions.decide(fixture.one, Session.Snapshot(sha, Branch.Default), evidence))
        val shadowed =
          changed.map(evidence => Versions.decide(fixture.both, Session.Snapshot(sha, Branch.Default), evidence))
        val unproven = unknown.map(evidence => Versions.decide(fixture.one, Session.Development, evidence))
        assertTrue(
          held == Right(
            Decision.Publish(
              PublishKind.Snapshot(sha, Branch.Default),
              ::(
                PublishLine(
                  fixture.libs,
                  PublishAction.Hold(HoldCompile.Everyday(CiVersion(number)), HoldReason.AlreadyReleased),
                ),
                Nil,
              ),
            )
          ),
          shadowed == Right(Decision.Refused(::(Refusal.Shadowed(fixture.libs, number), Nil))),
          unproven == Right(Decision.Refused(::(Refusal.Unproven(fixture.libs, number), Nil))),
        )
    ,
    test("a manual catalog publishes the stated version and does not read git"):
      use: fixture =>
        val stated   = ManualVersion("1.2.3-SNAPSHOT")
        val catalog  = Catalog.from(List(Ship.Manual(fixture.libs, stated, ::(fixture.core, Nil))))
        val decided  = catalog.map(Versions.decide(_, Session.Snapshot(sha, Branch.Default), Evidence.Unchecked))
        val assessed = catalog.map(
          Versions.assess(_, SessionSource(Some("snapshot"), None, None, None), Evidence.Unchecked)
        )
        val manual = Decision.Manual(::(ManualLine(fixture.libs, stated), Nil))
        assertTrue(
          decided == Right(manual),
          assessed == Right(Right(manual)),
        )
    ,
    test("a publish does not run until the registry has been read"):
      use: fixture =>
        val snapshot = Versions.decide(fixture.one, Session.Snapshot(sha, Branch.Default), Evidence.Unchecked)
        val release  = Versions.decide(fixture.one, Session.Release, Evidence.Unchecked)
        val absent   = Versions.decide(Catalog.empty, Session.Snapshot(sha, Branch.Default), Evidence.Unchecked)
        assertTrue(
          snapshot == Decision.Refused(::(Refusal.Unchecked, Nil)),
          release == Decision.Refused(::(Refusal.Unchecked, Nil)),
          absent == Decision.NoCatalog,
        )
    ,
    test("a release publishes the catalog number and leaves that number in place"):
      use: fixture =>
        val first = Versions.decide(fixture.one, Session.Release, clear)
        val again = registry(s"${fixture.libs.value} matches").map: evidence =>
          Versions.decide(fixture.one, Session.Release, evidence)
        val mixed = registry(s"${fixture.libs.value} matches").map: evidence =>
          Versions.decide(fixture.both, Session.Release, evidence)
        val held = PublishLine(
          fixture.libs,
          PublishAction.Hold(HoldCompile.Release(ReleaseVersion(number)), HoldReason.AlreadyReleased),
        )
        val published = PublishLine(
          fixture.client,
          PublishAction.Release(ReleaseVersion(clientNumber)),
        )
        val line = PublishLine(fixture.libs, PublishAction.Release(ReleaseVersion(number)))
        assertTrue(
          ReleaseVersion(number).render == "1.4.2",
          first == Decision.Publish(PublishKind.Release, ::(line, Nil)),
          Decision.rendered(line) == s"${fixture.libs.value} compile 1.4.2 publish 1.4.2",
          again == Right(Decision.Refused(::(Refusal.NothingToRelease, Nil))),
          mixed == Right(Decision.Publish(PublishKind.Release, ::(held, published :: Nil))),
        )
    ,
    test("a ship named by the registry and missing from the catalog is not a scheme refusal"):
      use: fixture =>
        val assessed =
          registry("client matches").map(Versions.assess(fixture.one, SessionSource(None, None, None, None), _))
        assertTrue(assessed == Right(Left(VersionError.UnknownShip(fixture.client)))),
  )

  def spec = suite("version scheme")(pins, edge, transitions)
end VersionSpec
