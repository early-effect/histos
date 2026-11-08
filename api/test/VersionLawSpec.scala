package histos.version

import zio.test.*

object VersionLawSpec extends ZIOSpecDefault:
  private val components = Gen.int(0, 9999)
  private val abbrev     = AbbrevSha.trusted("0123456789ab")

  def spec = suite("version laws")(
    test("a ship number round-trips, and a leading zero is not a component"):
      check(components, components, components, Gen.int(0, 9999)): (major, minor, patch, padded) =>
        val number = ShipNumber.trusted(major, minor, patch)
        val zeroed = s"0$padded.0.1"
        assertTrue(
          ShipNumber.from(number.render) == Right(number),
          ShipNumber.from("") == Left(ShipNumberError.Empty),
          ShipNumber.from(zeroed) == Left(ShipNumberError.NotAShip(zeroed)),
          ShipNumber.from(s"${number.render}.0") == Left(ShipNumberError.NotAShip(s"${number.render}.0")),
        )
    ,
    test("render then parse is the identity of a revision, and only a stored snapshot or a release is a pin"):
      check(components, components, components): (major, minor, patch) =>
        val number = ShipNumber.trusted(major, minor, patch)
        val forms  = List(
          Revision.Release(ReleaseVersion(number)),
          Revision.Ci(CiVersion(number)),
          Revision.Pointer(SnapshotPointer(number)),
          Revision.Snapshot(SnapshotCoordinate(number, abbrev)),
        )
        val stored = s"${number.render}-${abbrev.value}-SNAPSHOT"
        val bare   = s"${number.render}-${abbrev.value}"
        assertTrue(
          forms.forall(form => Revision.parse(form.render) == Right(form)),
          Pin.from(number.render) == Right(Pin.Release(ReleaseVersion(number))),
          Pin.from(stored) == Right(Pin.Snapshot(SnapshotCoordinate(number, abbrev))),
          Pin.from(s"${number.render}-ci") == Left(PinError.CompileOnly(CiVersion(number))),
          Pin.from(s"${number.render}-SNAPSHOT") == Left(PinError.Pointer(SnapshotPointer(number))),
          Revision.parse(bare) == Left(RevisionError.Unstored(number, abbrev)),
        )
    ,
    test("a snapshot sha shorter than 12 hex is not stored"):
      check(Gen.int(1, 11)): width =>
        val short = "a" * width
        val raw   = s"1.2.3-$short-SNAPSHOT"
        assertTrue(
          Revision.parse(raw) == Left(RevisionError.ShortSha(short)),
          Pin.from(raw) == Left(PinError.ShortSha(short)),
        ),
  )
end VersionLawSpec
