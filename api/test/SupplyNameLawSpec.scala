package histos.supply

import zio.test.*

object SupplyNameLawSpec extends ZIOSpecDefault:
  private val spans = Gen.int(-3, 12)

  def spec = suite("supply name laws")(
    test("a time zone is an IANA id, and an unknown id is refused"):
      val chicago = TimeZone.from("America/Chicago")
      val utc     = TimeZone.from("UTC")
      assertTrue(
        chicago.map(_.value) == Right("America/Chicago"),
        utc.map(_.value) == Right("UTC"),
        TimeZone.from("Mars/Base") == Left(TimeZoneError.Unknown("Mars/Base")),
      )
    ,
    test("a branch name is the git check-ref-format shape"):
      val refused = List(
        "",
        "main/",
        "main.",
        "/main",
        "a/../b",
        "a@{b",
        "a//b",
        "@",
        "a.lock",
        ".hidden",
        "a b",
        "a~b",
        "a^b",
        "a:b",
        "a?b",
        "a*b",
        "a[b",
        "a\\b",
        "a\u007fb",
      )
      val kept = List("main", "histos/deps", "feature")
      assertTrue(
        refused.forall(raw => BranchName.from(raw) == Left(BranchNameError.Bad(raw))),
        kept.forall(raw => BranchName.from(raw).map(_.value) == Right(raw)),
      )
    ,
    test("a day count and an hour count refuse only a negative"):
      check(spans): raw =>
        val days  = if raw < 0 then Left(SpanError.Negative(raw)) else Right(raw)
        val hours = if raw < 0 then Left(SpanError.Negative(raw)) else Right(raw)
        assertTrue(
          Days.from(raw).map(_.value) == days,
          Hours.from(raw).map(_.value) == hours,
        )
    ,
    test("an advisory id is GHSA and three lowercase groups of four"):
      val kept = "GHSA-1234-abcd-ef09"
      assertTrue(
        AdvisoryId.from(kept).map(_.value) == Right(kept),
        AdvisoryId.from("GHSA-123-abcd-ef09") == Left(AdvisoryError.Bad("GHSA-123-abcd-ef09")),
        AdvisoryId.from("GHSA-ABCD-abcd-ef09") == Left(AdvisoryError.Bad("GHSA-ABCD-abcd-ef09")),
        AdvisoryId.from("ghsa-1234-abcd-ef09") == Left(AdvisoryError.Bad("ghsa-1234-abcd-ef09")),
        AdvisoryId.from("GHSA-1234-abcd") == Left(AdvisoryError.Bad("GHSA-1234-abcd")),
      )
    ,
    test("a reason, a date, a coordinate, a repository, and a purl each refuse their own shape"):
      assertTrue(
        Reason.from("because").map(_.value) == Right("because"),
        Reason.from("") == Left(ReasonError.Bad),
        Reason.from("a\nb") == Left(ReasonError.Bad),
        Date.from("2026-11-06").map(_.value) == Right("2026-11-06"),
        Date.from("2026-13-01") == Left(DateError.Bad("2026-13-01")),
        Date.from("06-11-2026") == Left(DateError.Bad("06-11-2026")),
        Coordinate.from("dev.zio:zio").map(_.value) == Right("dev.zio:zio"),
        Coordinate.from("dev.zio:zio:2") == Left(CoordinateError.Bad("dev.zio:zio:2")),
        Coordinate.from("dev.zio:") == Left(CoordinateError.Bad("dev.zio:")),
        Coordinate.from("") == Left(CoordinateError.Bad("")),
        RepoName.from("early-effect/histos").map(_.value) == Right("early-effect/histos"),
        RepoName.from("early-effect/histos/api") == Left(RepoNameError.Bad("early-effect/histos/api")),
        RepoName.from("early-effect/") == Left(RepoNameError.Bad("early-effect/")),
        Purl.from("pkg:maven/dev.zio/zio@2").map(_.value) == Right("pkg:maven/dev.zio/zio@2"),
        Purl.from("pkg:maven/dev.zio/zio") == Left(PurlError.Bad("pkg:maven/dev.zio/zio")),
        Purl.from("maven/dev.zio/zio@2") == Left(PurlError.Bad("maven/dev.zio/zio@2")),
      )
    ,
    test("a CVSS 3.1 vector bands by its base score, and a prefix is still low"):
      val critical = "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:H/I:H/A:H"
      val high     = "CVSS:3.1/AV:N/AC:L/PR:N/UI:R/S:U/C:H/I:H/A:H"
      val moderate = "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:L/I:N/A:N"
      val low      = "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:N/I:N/A:N"
      assertTrue(
        Cvss.from(critical).map(_.band) == Right(Severity.Critical),
        Cvss.from(high).map(_.band) == Right(Severity.High),
        Cvss.from(moderate).map(_.band) == Right(Severity.Moderate),
        Cvss.from(low).map(_.band) == Right(Severity.Low),
        Cvss.from("CVSS:3.1/").map(_.band) == Right(Severity.Low),
        Cvss.from("CVSS:3.0/AV:N") == Left(CvssError.Bad("CVSS:3.0/AV:N")),
      )
    ,
    test("a severity word maps onto the rank, and at least follows that rank"):
      val order = List(Severity.Unknown, Severity.Low, Severity.Moderate, Severity.High, Severity.Critical)
      val words = List(
        "low"      -> Severity.Low,
        "moderate" -> Severity.Moderate,
        "medium"   -> Severity.Moderate,
        "HIGH"     -> Severity.High,
        "Critical" -> Severity.Critical,
        "none"     -> Severity.Unknown,
      )
      assertTrue(
        words.forall((word, severity) => Severity.fromWord(word) == severity),
        order.indices.forall: left =>
          order.indices.forall: right =>
            order(left).atLeast(order(right)) == (left >= right),
      )
    ,
    test("a label, a source, a person, an email, a cell, and a repository id keep their own refusal"):
      val label = "a" * 50
      assertTrue(
        LabelName.from(label).map(_.value) == Right(label),
        LabelName.from(label + "a") == Left(LabelNameError.Bad(label + "a")),
        LabelName.from("a b") == Left(LabelNameError.Bad("a b")),
        LabelName.from("") == Left(LabelNameError.Bad("")),
        SourceName.from("osv.dev").map(_.value) == Right("osv.dev"),
        SourceName.from("osv dev") == Left(SourceNameError.Bad("osv dev")),
        SourceName.from("") == Left(SourceNameError.Bad("")),
        PersonName.from("Early Effect").map(_.value) == Right("Early Effect"),
        PersonName.from("") == Left(PersonNameError.Bad("")),
        PersonName.from("a\nb") == Left(PersonNameError.Bad("a\nb")),
        Email.from("a@b.example").map(_.value) == Right("a@b.example"),
        Email.from("@b") == Left(EmailError.Bad("@b")),
        Email.from("a@") == Left(EmailError.Bad("a@")),
        Email.from("a @b") == Left(EmailError.Bad("a @b")),
        CellName.from("core/jvm").map(_.value) == Right("core/jvm"),
        CellName.from("core jvm") == Left(CellNameError.Bad("core jvm")),
        CellName.from("") == Left(CellNameError.Bad("")),
        RepoId.from("123").map(_.value) == Right("123"),
        RepoId.from("1 2") == Left(RepoIdError.Bad("1 2")),
        RepoId.from("") == Left(RepoIdError.Bad("")),
      ),
  )
end SupplyNameLawSpec
