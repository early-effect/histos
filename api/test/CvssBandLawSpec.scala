package histos.supply

import zio.test.*

object CvssBandLawSpec extends ZIOSpecDefault:
  def spec = suite("cvss band laws")(
    test("each metric letter lands in the band that letter is worth, and a vector that is not 3.1 is refused"):
      val rows = List(
        "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:H/I:H/A:H"        -> Severity.Critical,
        "CVSS:3.1/AV:A/AC:L/PR:N/UI:N/S:U/C:H/I:H/A:H"        -> Severity.High,
        "CVSS:3.1/AV:L/AC:L/PR:N/UI:N/S:U/C:H/I:N/A:N"        -> Severity.Moderate,
        "CVSS:3.1/AV:P/AC:L/PR:N/UI:N/S:U/C:H/I:N/A:N"        -> Severity.Moderate,
        "CVSS:3.1/AV:N/AC:H/PR:N/UI:N/S:U/C:H/I:H/A:H"        -> Severity.High,
        "CVSS:3.1/AV:N/AC:L/PR:L/UI:N/S:C/C:H/I:H/A:H"        -> Severity.Critical,
        "CVSS:3.1/AV:N/AC:L/PR:L/UI:N/S:U/C:H/I:H/A:H"        -> Severity.High,
        "CVSS:3.1/AV:N/AC:L/PR:H/UI:N/S:C/C:H/I:N/A:N"        -> Severity.Moderate,
        "CVSS:3.1/AV:N/AC:L/PR:H/UI:N/S:U/C:L/I:N/A:N"        -> Severity.Low,
        "CVSS:3.1/AV:N/AC:L/PR:N/UI:R/S:U/C:H/I:H/A:H"        -> Severity.High,
        "CVSS:3.1/AV:N/AC:L/PR:N/UI:R/S:U/C:L/I:L/A:N"        -> Severity.Moderate,
        "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:H/I:N/A:N"        -> Severity.High,
        "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:L/I:N/A:N"        -> Severity.Moderate,
        "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:N/I:L/A:N"        -> Severity.Moderate,
        "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:N/I:N/A:L"        -> Severity.Moderate,
        "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:N/I:N/A:N"        -> Severity.Low,
        "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:C/C:H/I:H/A:H"        -> Severity.Critical,
        "CVSS:3.1/AV:P/AC:H/PR:H/UI:R/S:C/C:N/I:N/A:N"        -> Severity.Low,
        "CVSS:3.1/"                                           -> Severity.Low,
        "CVSS:3.1/AV:Z/AC:L/PR:N/UI:N/S:U/C:H/I:H/A:H"        -> Severity.Critical,
        "CVSS:3.1/AV:N/AC:Z/PR:N/UI:N/S:U/C:H/I:H/A:H"        -> Severity.Critical,
        "CVSS:3.1/AV:N/AC:L/PR:X/UI:N/S:U/C:L/I:N/A:N"        -> Severity.Moderate,
        "CVSS:3.1/AV:N/AC:L/PR:N/UI:Z/S:U/C:L/I:L/A:N"        -> Severity.Moderate,
        "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:H/I:H/A:H/orphan" -> Severity.Critical,
      )
      val missed = rows.filter((raw, band) => Cvss.from(raw).map(_.band) != Right(band))
      assertTrue(
        missed == Nil,
        Cvss.from("CVSS:3.0/AV:N") == Left(CvssError.Bad("CVSS:3.0/AV:N")),
      )
  )
