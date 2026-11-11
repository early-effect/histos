package histos

import histos.version.{AbbrevSha, CommitSha, Pin, PinError, ReleaseVersion, Revision, RevisionError, ShipNumber}

import zio.test.*

object ShipPinLawSpec extends ZIOSpecDefault:
  private val number = ShipNumber.trusted(1, 4, 2)
  private val sha    = CommitSha.trusted("0123456789abcdef0123456789abcdef01234567")
  private val org    = Organization.trusted("rocks.earlyeffect")
  private val name   = ArtifactBase.trusted("histos-api")

  def spec = suite("ship pin laws")(
    test("a pointer, a commit, and a release each render one revision and depend as a binary"):
      val pointer = PointerPin.trusted(number)
      val commit  = CommitPin.trusted(number, sha)
      val release = ReleasePin.trusted(number)
      val binary  = release.dep(org, name)
      assertTrue(
        pointer.revision == s"${number.render}-SNAPSHOT",
        commit.revision == s"${number.render}-${sha.abbreviate.value}-SNAPSHOT",
        release.revision == number.render,
        release.revisionValue == Revision.Release(ReleaseVersion(number)),
        binary.organization == org.value,
        binary.name == name.value,
        binary.revision.value == number.render,
        binary.kind == DepKind.Binary,
        pointer.dep(org, name).revision.value == pointer.revision,
        commit.dep(org, name).revision.value == commit.revision,
        commit.dep(org, name).kind == DepKind.Binary,
      )
    ,
    test("an empty revision, a word, and a bare abbreviation are not pins"):
      val bare = s"${number.render}-${sha.abbreviate.value}"
      assertTrue(
        Revision.parse("") == Left(RevisionError.Empty),
        Revision.parse("nope") == Left(RevisionError.NotARevision("nope")),
        Pin.from("") == Left(PinError.Empty),
        Pin.from("nope") == Left(PinError.NotAPin("nope")),
        Pin.from(bare) == Left(PinError.Unstored(number, AbbrevSha.trusted(sha.abbreviate.value))),
      ),
  )
end ShipPinLawSpec
