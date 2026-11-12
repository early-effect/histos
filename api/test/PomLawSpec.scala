package histos

import zio.test.*

object PomLawSpec extends ZIOSpecDefault:
  def spec = suite("pom laws")(
    test("an sbt series is digits and dots, and pom text is one non-empty line"):
      assertTrue(
        SbtSeries.from("2").map(_.value) == Right("2"),
        SbtSeries.from("1.0").map(_.value) == Right("1.0"),
        SbtSeries.from("2.0.1").map(_.value) == Right("2.0.1"),
        SbtSeries.from("") == Left(SbtSeriesError.Bad("")),
        SbtSeries.from("v2") == Left(SbtSeriesError.Bad("v2")),
        SbtSeries.from("2.") == Left(SbtSeriesError.Bad("2.")),
        PomText.from("a library").map(_.value) == Right("a library"),
        PomText.from("") == Left(PomTextError.Empty),
        PomText.from("   ") == Left(PomTextError.Empty),
        PomText.from("a\nb") == Left(PomTextError.Bad("a\nb")),
      )
    ,
    test("a pom url, license, developer, and scm connection each refuse their own shape"):
      assertTrue(
        HttpUrl.from("https://example.com/histos").map(_.value) == Right("https://example.com/histos"),
        HttpUrl.from("https://") == Left(HttpUrlError.Bad("https://")),
        HttpUrl.from("http://example.com") == Left(HttpUrlError.Bad("http://example.com")),
        HttpUrl.from("https://example.com/a b") == Left(HttpUrlError.Bad("https://example.com/a b")),
        LicenseName.from("Apache-2.0").map(_.value) == Right("Apache-2.0"),
        LicenseName.from("") == Left(LicenseNameError.Bad("")),
        LicenseName.from("-MIT") == Left(LicenseNameError.Bad("-MIT")),
        DeveloperId.from("early-effect").map(_.value) == Right("early-effect"),
        DeveloperId.from("") == Left(DeveloperIdError.Bad("")),
        DeveloperId.from("1early") == Left(DeveloperIdError.Bad("1early")),
        ScmConnection.from("scm:git:git@github.com:early-effect/histos.git").map(_.value) ==
          Right("scm:git:git@github.com:early-effect/histos.git"),
        ScmConnection.from("scm:") == Left(ScmConnectionError.Bad("scm:")),
        ScmConnection.from("git:somewhere") == Left(ScmConnectionError.Bad("git:somewhere")),
        ScmConnection.from("scm:git:has space") == Left(ScmConnectionError.Bad("scm:git:has space")),
      ),
  )
