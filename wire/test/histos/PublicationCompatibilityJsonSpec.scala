package histos

import histos.publish.*
import histos.report.WireJson.given
import histos.version.{BumpPart, ShipName, ShipNumber}
import java.nio.file.Path
import zio.json.*
import zio.test.*

object PublicationCompatibilityJsonSpec extends ZIOSpecDefault:
  private val previous   = ShipNumber.trusted(0, 15, 0)
  private val number     = ShipNumber.trusted(0, 15, 1)
  private val coordinate = PublicationCoordinate(Organization("example.test"), "second-artifact_3")
  private val outcomes   = List(
    PublicationCompatibilityOutcome.Policy(previous, BumpPart.Minor, 1),
    PublicationCompatibilityOutcome.Incomplete(PublicationCompatibilityGap.BaselineMissing(previous)),
    PublicationCompatibilityOutcome.Incomplete(PublicationCompatibilityGap.BaselineUnavailable(previous)),
    PublicationCompatibilityOutcome.Incomplete(
      PublicationCompatibilityGap.ListingStatus("https://example.test/metadata", 503)
    ),
    PublicationCompatibilityOutcome.Incomplete(PublicationCompatibilityGap.MissingCurrent(Path.of("classes/second"))),
  )

  def spec = suite("publication compatibility wire errors")(
    test("typed local publication failures preserve exact member, version, coordinate and outcome") {
      val errors = outcomes.map(outcome =>
        LocalPublishError.Compatibility(
          ::(
            PublicationCompatibilityFailure(
              ShipName("library"),
              number,
              ProjectName("second"),
              ScalaVersion("3.8.2"),
              coordinate,
              outcome,
            ),
            Nil,
          )
        )
      )
      val results = errors.map(error => histos.report.CallResult.Fault(histos.report.BuildFault.Publish(error)))
      assertTrue(
        errors.forall(error => error.toJson.fromJson[LocalPublishError] == Right(error)),
        results.forall(result => result.toJson.fromJson[histos.report.CallResult] == Right(result)),
      )
    },
    test("unknown compatibility outcomes cannot decode") {
      val failure = PublicationCompatibilityFailure(
        ShipName("library"),
        number,
        ProjectName("second"),
        ScalaVersion("3.8.2"),
        coordinate,
        PublicationCompatibilityOutcome.Policy(previous, BumpPart.Minor, 1),
      )
      assertTrue(failure.toJson.replace("Policy", "Unchecked").fromJson[PublicationCompatibilityFailure].isLeft)
    },
  )
end PublicationCompatibilityJsonSpec
