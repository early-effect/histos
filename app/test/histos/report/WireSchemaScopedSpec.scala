package histos.report

import heddle.endpoint.{Schema, SchemaDoc}
import histos.load.PluginFault
import histos.preview.PreviewError
import histos.publish.LocalPublishError
import histos.testrun.{ModuleTestError, ScopedTestFailure}
import histos.report.WireSchema.given
import zio.test.*

object WireSchemaScopedSpec extends ZIOSpecDefault:
  def spec = suite("scoped execution schemas")(
    test("recursive result schemas expose scoped failures without losing their cases") {
      assertTrue(
        labels[ScopedTestFailure].contains("MissingGenerated"),
        labels[ModuleTestError].contains("Scoped"),
        labels[PluginFault].contains("Scoped"),
        labels[PreviewError].contains("Test"),
        labels[LocalPublishError].contains("Build"),
        labels[LocalPublishError].contains("Compatibility"),
        labels[histos.publish.PublicationCompatibilityOutcome].contains("Incomplete"),
        labels[histos.publish.PublicationCompatibilityOutcome].contains("Policy"),
        labels[histos.publish.PublicationCompatibilityGap].contains("BaselineMissing"),
        labels[CallResult].contains("Fault"),
      )
    }
  )

  private def labels[A: Schema]: List[String] =
    summon[Schema[A]].cases.flatMap {
      case SchemaDoc.Object(_, fields, _) => fields.map(_.name)
      case _                              => Nil
    }
end WireSchemaScopedSpec
