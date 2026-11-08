package histos.testrun

import histos.resolve.{ClasspathScope, ResolveError}
import histos.{Platform, ProjectName, ScalaVersion}

import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** A cover that cannot be stored says which input is missing. */
object CoverGapLawSpec extends ZIOSpecDefault:
  private val name    = ProjectName.trusted("cover")
  private val version = ScalaVersion("3.10.0")

  def spec = suite("cover gap laws")(
    test("a lock error, a missing cell, node, and an API digest each name the gap"):
      val missing = ResolveError.UnsupportedPlatform(Platform.Js)
      val locked  = CoverGap.Locked(missing)
      val absent = CoverGap.Absent(name, version, Platform.Jvm, ClasspathScope.Test)
      val node   = CoverGap.NoNode
      val api    = CoverGap.NoApi(name, version, Platform.Native)
      assertTrue(
        locked.message == missing.message,
        absent.message == "test cover 3.10.0 jvm is not in the dependency lock",
        node.message == "node is not on disk",
        api.message == "'cover' 3.10.0 native has no readable API digest",
      )
  )
end CoverGapLawSpec
