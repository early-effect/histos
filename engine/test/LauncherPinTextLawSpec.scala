package histos.publish

import histos.version.{ShipNumber, SnapshotPointer}

import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** A pin file is one field. A base is https, loopback http, or a file URL. */
object LauncherPinTextLawSpec extends ZIOSpecDefault:
  def spec = suite("launcher pin text laws")(
    test("a comment is ignored, and a bad line, field, repeat, or version is that fault"):
      val release  = LauncherPin.parse("\n# note\nhistos.version=1.2.3\n")
      val pointer  = LauncherPin.parse("histos.version=1.2.3-SNAPSHOT\n")
      val missing  = LauncherPin.parse("# only\n")
      val line     = LauncherPin.parse("nope")
      val unknown  = LauncherPin.parse("other=1")
      val repeated = LauncherPin.parse("histos.version=1.2.3\nhistos.version=1.2.4\n")
      val version  = LauncherPin.parse("histos.version=nope")
      assertTrue(
        release.exists(pin =>
          pin.version == "1.2.3" && pin.choice == LauncherChoice.Release(number) && pin.shown == "histos.version=1.2.3"
        ),
        pointer.exists(pin =>
          pin.version == "1.2.3-SNAPSHOT" &&
            pin.choice == LauncherChoice.Pointer(LauncherChannel.Line(SnapshotPointer(number)))
        ),
        missing == Left(LauncherError.Missing),
        line == Left(LauncherError.Line("nope")),
        unknown == Left(LauncherError.Unknown("other")),
        repeated == Left(LauncherError.Duplicate),
        version == Left(LauncherError.Version("nope")),
        message(missing) == "launcher properties are missing histos.version",
        message(line) == "launcher properties line is not a field: nope",
        message(unknown) == "launcher properties field is unknown: other",
        message(repeated) == "launcher properties repeat histos.version",
        message(version).contains("not a release, a snapshot pointer, or a timestamped revision"),
      )
    ,
    test("https, loopback, and file are bases, and any other spelling is refused"):
      val https    = LauncherPin.renderBase("https://example.test/repo")
      val loopback = LauncherPin.renderBase("http://127.0.0.1:9/repo")
      val local    = LauncherPin.renderBase("http://localhost/repo")
      val file     = LauncherPin.renderBase("file:///tmp/repo")
      val remote   = LauncherPin.renderBase("http://example.test/repo")
      val spaced   = LauncherPin.renderBase("https://example.test/has space")
      assertTrue(
        https == Right("https://example.test/repo"),
        loopback == Right("http://127.0.0.1:9/repo"),
        local == Right("http://localhost/repo"),
        file == Right("file:///tmp/repo"),
        remote == Left(LauncherError.Repository("http://example.test/repo")),
        spaced == Left(LauncherError.Repository("https://example.test/has space")),
        message(remote) == "launcher repository is not a supported base: http://example.test/repo",
      ),
  )

  private val number = ShipNumber("1.2.3")

  private def message(found: Either[LauncherError, ?]): String =
    found match
      case Left(error) => error.message
      case Right(_)    => ""
end LauncherPinTextLawSpec
