package histos.resolve

import coursier.cache.CachePolicy

import zio.test.*

object RecheckSpec extends ZIOSpecDefault:
  def spec = suite("toolchain pack resolve")(
    test("a changing snapshot is checked again") {
      val cache = CoursierResolve.files("/tmp/histos-recheck", CoursierResolve.CacheUse.Recheck)
      assertTrue(
        cache.ttl.isEmpty,
        cache.cachePolicies == Seq(CachePolicy.UpdateChanging),
      )
    },
    test("a retained resolve keeps a cached snapshot") {
      val cache = CoursierResolve.files("/tmp/histos-recheck", CoursierResolve.CacheUse.Locked)
      assertTrue(
        cache.cachePolicies == Seq(CachePolicy.LocalOnly, CachePolicy.FetchMissing)
      )
    },
  )
