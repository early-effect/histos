package histos

import histos.version.{PullRequestError, PullRequestNumber, ShipName, ShipNameError}

import zio.test.*

object NameLiteralSpec extends ZIOSpecDefault:
  def spec = suite("name literals")(
    test("a literal is the value it names, and a runtime string keeps the typed error"):
      assertTrue(
        ProjectName("core").value == "core",
        ShipName("histos").value == "histos",
        PullRequestNumber("12").value == 12,
        ProjectName.from("") == Left(ProjectNameError.Empty),
        ProjectName.from("1core") == Left(ProjectNameError.Bad("1core")),
        ShipName.from("1libs") == Left(ShipNameError.Bad("1libs")),
        PullRequestNumber.parse("0") == Left(PullRequestError.NotPositive(0)),
        PullRequestNumber.parse("01") == Left(PullRequestError.NotANumber("01")),
        PullRequestNumber.parse("12").map(_.value) == Right(12),
      )
  )
