package histos

import zio.test.*

object ResolutionSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion("3.9.0")

  def spec = suite("resolution")(
    test("omission resolves from the build"):
      val core = module("core", Jvm)(scala3)
      assertTrue(Projects(core).map(_.resolution) == Right(Resolution.Solve))
    ,
    test("locked pins the build, and a later organization keeps the pin"):
      val core = module("core", Jvm)(scala3)
      assertTrue(
        Projects.locked(core).map(_.resolution) == Right(Resolution.Lock),
        Projects.locked
          .organization(Organization("rocks.earlyeffect"))(core)
          .map(_.resolution) == Right(Resolution.Lock),
      ),
  )
end ResolutionSpec
