package histos.version

import zio.test.*

object RevisionRestLawSpec extends ZIOSpecDefault:
  private val number   = ShipNumber.trusted(1, 4, 2)
  private val rendered = number.render
  private val abbrev   = AbbrevSha.trusted("0123456789ab")

  def spec = suite("revision rest laws")(
    test("a short sha, a bad abbreviation, and a compile suffix each fail on their own arm"):
      val upper = s"$rendered-0123456789AB-SNAPSHOT"
      val junk  = s"$rendered-zzzzzzzzzzzz"
      val ci    = s"$rendered-0123456789ab-ci"
      assertTrue(
        Revision.parse(s"$rendered-abc") == Left(RevisionError.ShortSha("abc")),
        Revision.parse(s"$rendered-abc-SNAPSHOT") == Left(RevisionError.ShortSha("abc")),
        Revision.parse(s"$rendered-ci-SNAPSHOT") == Left(RevisionError.NotARevision(s"$rendered-ci-SNAPSHOT")),
        Revision.parse(junk) == Left(RevisionError.NotARevision(junk)),
        Revision.parse(s"$junk-SNAPSHOT") == Left(RevisionError.NotARevision(s"$junk-SNAPSHOT")),
        Revision.parse(upper) == Left(RevisionError.NotARevision(upper)),
        Revision.parse("nope-ci") == Left(RevisionError.NotARevision("nope-ci")),
        Revision.parse(ci) == Left(RevisionError.NotARevision(ci)),
        Revision.parse("-SNAPSHOT") == Left(RevisionError.NotARevision("-SNAPSHOT")),
        Revision.parse(s"$rendered-CI") == Left(RevisionError.NotARevision(s"$rendered-CI")),
        Pin.from(s"$rendered-abc") == Left(PinError.ShortSha("abc")),
        Pin.from(s"$junk-SNAPSHOT") == Left(PinError.NotAPin(s"$junk-SNAPSHOT")),
        Pin.from(upper) == Left(PinError.NotAPin(upper)),
        Pin.from("nope-ci") == Left(PinError.NotAPin("nope-ci")),
        Pin.from(ci) == Left(PinError.NotAPin(ci)),
      )
    ,
    test("a revision error and a pin error name that failure, not the other kind"):
      val stored = s"$rendered-${abbrev.value}-SNAPSHOT"
      assertTrue(
        RevisionError.Empty.message == "a revision is not empty",
        RevisionError.ShortSha("abc").message ==
          "'abc' is shorter than 12 hex characters. A snapshot sha is the 12-character abbreviation of the commit.",
        RevisionError.Unstored(number, abbrev).message ==
          s"$rendered-${abbrev.value} is not stored anywhere. Pin $stored.",
        RevisionError.NotARevision("nope").message == "'nope' is not a revision",
        PinError.Empty.message == "a pin is not empty",
        PinError.Pointer(SnapshotPointer(number)).message == s"$rendered-SNAPSHOT is the snapshot pointer, not a build",
        PinError.CompileOnly(CiVersion(number)).message == s"$rendered-ci is the compile version, not a pin",
        PinError.ShortSha("abc").message ==
          "'abc' is shorter than 12 hex characters. A snapshot sha is the 12-character abbreviation of the commit.",
        PinError.Unstored(number, abbrev).message ==
          s"$rendered-${abbrev.value} is not stored anywhere. Pin $stored.",
        PinError.NotAPin("nope").message == "'nope' is not a pin",
        Pin.Release(ReleaseVersion(number)).render == rendered,
        Pin.Snapshot(SnapshotCoordinate(number, abbrev)).render == stored,
      ),
  )
end RevisionRestLawSpec
