package histos.version

import histos.{ProjectName, ProjectNameError}

import zio.test.*

object VersionSpec extends ZIOSpecDefault:
  private val number = ShipNumber("1.4.2")
  private val clientNumber = ShipNumber("0.3.0")
  private val sha = CommitSha("1234567890abcdef1234567890abcdef12345678")
  private val abbrev = sha.abbreviate

  private enum FixtureError:
    case Ship(error: ShipNameError)
    case Project(error: ProjectNameError)
    case Pull(error: PullRequestError)
    case Rows(error: CatalogError)

    def message: String = this match
      case Ship(error)    => error.message
      case Project(error) => error.message
      case Pull(error)    => error.message
      case Rows(error)    => error.message

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
      both   <- Catalog.from(List(Ship.Line(libs, number, ::(core, Nil)), Ship.Line(client, clientNumber, ::(tests, Nil)))).left.map(FixtureError.Rows(_))
    yield Fixture(libs, client, core, one, both, pull)

  private def use(f: Fixture => TestResult): TestResult =
    opened match
      case Right(fixture) => f(fixture)
      case Left(error)    => assertTrue(error.message == "fixture")

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
        )
    ,
  )

  private val edge = suite("edge")(
    test("development is the absent session, and a snapshot without a commit is refused"):
      val snapshot = Map(Session.Key -> "snapshot", Session.ShaKey -> sha.value, Session.BranchKey -> "default")
      assertTrue(
        Session.from(Map.empty) == Right(Session.Development),
        Session.from(Map(Session.Key -> "snapshot")) == Left(SessionError.MissingSha),
        Session.from(Map(Session.Key -> "snapshot", Session.ShaKey -> "abc")) == Left(SessionError.Sha(CommitShaError.NotHex("abc"))),
        Session.from(snapshot) == Right(Session.Snapshot(sha, Branch.Default)),
        Session.from(Map(Session.Key -> "pr", Session.ShaKey -> sha.value)) == Left(SessionError.MissingPullRequest),
        Session.from(Map(Session.Key -> "nope")) == Left(SessionError.Unknown("nope")),
        PullRequestNumber.parse("01") == Left(PullRequestError.NotANumber("01")),
        PullRequestNumber.from(0) == Left(PullRequestError.NotPositive(0)),
      )
    ,
    test("a registry line names a ship and a relation, and a duplicate is refused"):
      use: fixture =>
        val parsed = Registry.parse(List("libs matches", "# note", ""))
        assertTrue(
          parsed match
            case Right(found) => found.relation(fixture.libs) == Some(TagRelation.Matches)
            case Left(error)  => error.message == "matches"
          ,
          Registry.parse(List("libs matches", "# note", "", "libs moved")) == Left(RegistryError.Duplicate(fixture.libs)),
          Registry.parse(List("libs sideways")) == Left(RegistryError.BadRelation("sideways")),
          Registry.parse(List("1.4.2 matches")) == Left(RegistryError.BadName("1.4.2")),
        )
    ,
  )

  private val transitions = suite("transitions")(
    test("development compiles -ci, and an unchecked registry is not a passed drift gate"):
      use: fixture =>
        val checked = Versions.decide(fixture.one, Session.Development, clear)
        val missed = Versions.decide(fixture.one, Session.Development, Evidence.Unchecked)
        assertTrue(
          checked match
            case Decision.Develop(Drift.Clear, line :: Nil) => line.compile.render == "1.4.2-ci"
            case other                                      => Decision.text(other) == "development"
          ,
          missed match
            case Decision.Develop(Drift.Unchecked, line :: Nil) => line.compile.render == "1.4.2-ci"
            case other                                          => Decision.text(other) == "unchecked"
          ,
          Decision.text(missed).contains("drift unchecked"),
        )
    ,
    test("a default-branch snapshot stores the commit and names the pointer; another branch does not"):
      use: fixture =>
        val onDefault = Versions.decide(fixture.one, Session.Snapshot(sha, Branch.Default), clear)
        val elsewhere = Versions.decide(fixture.one, Session.Snapshot(sha, Branch.Other), clear)
        assertTrue(
          onDefault match
            case Decision.Publish(_, line :: Nil) =>
              line.action match
                case PublishAction.Snapshot(compile, stored, pointer) =>
                  compile.render == "1.4.2-ci" &&
                  stored.render == "1.4.2-1234567890ab-SNAPSHOT" &&
                  pointer == Some(SnapshotPointer(number))
                case _ => false
            case other => Decision.text(other) == "snapshot default"
          ,
          elsewhere match
            case Decision.Publish(_, line :: Nil) =>
              line.action match
                case PublishAction.Snapshot(_, stored, pointer) =>
                  stored.render == "1.4.2-1234567890ab-SNAPSHOT" && pointer.isEmpty
                case _ => false
            case other => Decision.text(other) == "snapshot other"
          ,
        )
    ,
    test("a pull request stores the same commit and cannot name the pointer"):
      use: fixture =>
        val decision = Versions.decide(fixture.one, Session.PullRequest(fixture.pull, sha), clear)
        assertTrue:
          decision match
            case Decision.Publish(PublishKind.PullRequest(pull, _), line :: Nil) =>
              pull.value == 42 &&
              (line.action match
                case PublishAction.Snapshot(_, stored, pointer) =>
                  stored.render == "1.4.2-1234567890ab-SNAPSHOT" && pointer.isEmpty
                case _ => false)
            case other => Decision.text(other) == "pr"
    ,
    test("a released ship that still matches publishes nothing, and a moved or unproven ship publishes nothing at all"):
      use: fixture =>
        val current = registry(s"${fixture.libs.value} matches")
        val changed = registry(s"${fixture.libs.value} moved")
        val unknown = registry(s"${fixture.libs.value} unknown")
        val held = current.map(evidence => Versions.decide(fixture.one, Session.Snapshot(sha, Branch.Default), evidence))
        val shadowed = changed.map(evidence => Versions.decide(fixture.both, Session.Snapshot(sha, Branch.Default), evidence))
        val unproven = unknown.map(evidence => Versions.decide(fixture.one, Session.Development, evidence))
        assertTrue(
          held match
            case Right(Decision.Publish(_, line :: Nil)) =>
              line.action match
                case PublishAction.Hold(compile, HoldReason.AlreadyReleased) => compile.render == "1.4.2-ci"
                case _                                                        => false
            case other => other.fold(_.message, Decision.text) == "hold"
          ,
          shadowed match
            case Right(Decision.Refused(refusal :: Nil)) =>
              refusal == Refusal.Shadowed(fixture.libs, number)
            case other => other.fold(_.message, Decision.text) == "shadowed"
          ,
          unproven match
            case Right(Decision.Refused(Refusal.Unproven(ship, released) :: Nil)) =>
              ship == fixture.libs && released == number
            case other => other.fold(_.message, Decision.text) == "unproven"
          ,
        )
    ,
    test("a manual catalog publishes the stated version and does not read git"):
      use: fixture =>
        val stated = ManualVersion("1.2.3-SNAPSHOT")
        Catalog.from(List(Ship.Manual(fixture.libs, stated, ::(fixture.core, Nil)))) match
          case Left(error) => assertTrue(error.message == "manual")
          case Right(catalog) =>
            val decided = Versions.decide(catalog, Session.Snapshot(sha, Branch.Default), Evidence.Unchecked)
            val assessed = Versions.assess(catalog, Map(Session.Key -> "snapshot"), Evidence.Unchecked)
            assertTrue(
              decided match
                case Decision.Manual(line :: Nil) => line.ship == fixture.libs && line.version == stated
                case other                        => Decision.text(other) == "manual"
              ,
              assessed match
                case Right(decision @ Decision.Manual(line :: Nil)) =>
                  line.version.value == "1.2.3-SNAPSHOT" &&
                  Decision.text(decision).contains("session manual") &&
                  !Decision.text(decision).contains("-ci")
                case other => other.fold(_.message, Decision.text) == "assessed"
              ,
            )
    ,
    test("a publish does not run until the registry has been read"):
      use: fixture =>
        val snapshot = Versions.decide(fixture.one, Session.Snapshot(sha, Branch.Default), Evidence.Unchecked)
        val release = Versions.decide(fixture.one, Session.Release, Evidence.Unchecked)
        val absent = Versions.decide(Catalog.empty, Session.Snapshot(sha, Branch.Default), Evidence.Unchecked)
        assertTrue(
          snapshot == Decision.Refused(::(Refusal.Unchecked, Nil)),
          release == Decision.Refused(::(Refusal.Unchecked, Nil)),
          absent == Decision.NoCatalog,
        )
    ,
    test("a release publishes the bare number and a pull-request bump, and does not release a number twice"):
      use: fixture =>
        val first = Versions.decide(fixture.one, Session.Release, clear)
        val again = registry(s"${fixture.libs.value} matches").map: evidence =>
          Versions.decide(fixture.one, Session.Release, evidence)
        val mixed = registry(s"${fixture.libs.value} matches").map: evidence =>
          Versions.decide(fixture.both, Session.Release, evidence)
        assertTrue(
          first match
            case Decision.Publish(PublishKind.Release, line :: Nil) =>
              line.action match
                case PublishAction.Release(compile, bump) =>
                  compile.render == "1.4.2" && bump == CatalogBump(fixture.libs, number)
                case _ => false
            case other => Decision.text(other) == "release"
          ,
          again match
            case Right(Decision.Refused(Refusal.NothingToRelease :: Nil)) => true
            case other => other.fold(_.message, Decision.text) == "nothing"
          ,
          mixed match
            case Right(Decision.Publish(PublishKind.Release, held :: published :: Nil)) =>
              (held.action, published.action) match
                case (PublishAction.Hold(heldAt, HoldReason.AlreadyReleased), PublishAction.Release(compile, bump)) =>
                  held.ship == fixture.libs &&
                  heldAt.render == "1.4.2" &&
                  published.ship == fixture.client &&
                  compile.render == "0.3.0" &&
                  bump == CatalogBump(fixture.client, clientNumber)
                case _ => false
            case other => other.fold(_.message, Decision.text) == "mixed"
          ,
        )
    ,
    test("a ship named by the registry and missing from the catalog is not a scheme refusal"):
      use: fixture =>
        registry("client matches") match
          case Left(error) => assertTrue(error.message == "client")
          case Right(found) =>
            val assessed = Versions.assess(fixture.one, Map.empty, found)
            assertTrue:
              assessed match
                case Left(VersionError.UnknownShip(name)) => name == fixture.client
                case other                                 => other.fold(_.message, Decision.text) == "unknown"
    ,
  )

  def spec = suite("version scheme")(pins, edge, transitions)
