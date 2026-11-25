package histos

import histos.report.WireJson.given
import histos.suite.HistosSpec
import histos.supply.Email

import zio.json.*
import zio.test.*

object PomMetadataJsonSpec extends HistosSpec:
  private val facts = OrganizationFacts(
    license = Some(License(LicenseName("Apache-2.0"), MetadataUri("http://www.apache.org/licenses/LICENSE-2.0.txt"))),
    developer = Some(
      Developer(
        DeveloperId("russwyte"),
        PomText("Russ White"),
        Email("356303+russwyte@users.noreply.github.com"),
        MetadataUri("https://github.com/russwyte"),
      )
    ),
    scm = Some(
      Scm(
        MetadataUri("https://github.com/early-effect/heddle"),
        ScmConnection("scm:git@github.com:early-effect/heddle.git"),
      )
    ),
    description = Some(PomText("HTTP as an effect. Loom under the floor.")),
    url = Some(MetadataUri("https://github.com/early-effect/heddle")),
    organizationName = Some(PomText("Early Effect")),
    organizationHomepage = Some(MetadataUri("https://www.earlyeffect.rocks")),
    versionScheme = Some(VersionScheme.EarlySemver),
  )

  def spec = suite("publication metadata JSON")(
    test("all publication facts and the typed URI error round trip") {
      val encoded = facts.toJson
      val error   = MetadataUriError.Bad("ftp://example.com")
      assertTrue(
        encoded.fromJson[OrganizationFacts] == Right(facts),
        encoded.contains("\"versionScheme\":\"early-semver\""),
        encoded.contains("http://www.apache.org/licenses/LICENSE-2.0.txt"),
        error.toJson.fromJson[MetadataUriError] == Right(error),
        OrganizationFacts.empty.toJson.fromJson[OrganizationFacts] == Right(OrganizationFacts.empty),
        Developer(DeveloperId("russwyte"), PomText("Russ White")).toJson.fromJson[Developer] == Right(
          Developer(DeveloperId("russwyte"), PomText("Russ White"))
        ),
      )
    },
    test("decoding rejects malformed metadata URIs and unknown version schemes") {
      assertTrue(
        "\"http://example.com/license\"".fromJson[MetadataUri].map(_.value) == Right("http://example.com/license"),
        "\"ftp://example.com/license\"".fromJson[MetadataUri].isLeft,
        "\"https:///missing-host\"".fromJson[MetadataUri].isLeft,
        "\"https://example.com/%GG\"".fromJson[MetadataUri].isLeft,
        "\"semver-ish\"".fromJson[VersionScheme].isLeft,
        facts.toJson.replace("https://www.earlyeffect.rocks", "relative/home").fromJson[OrganizationFacts].isLeft,
      )
    },
  )
end PomMetadataJsonSpec
