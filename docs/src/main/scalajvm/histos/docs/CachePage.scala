package histos.docs

import mermoid.ascent.MermoidAscent
import specular.*

import zio.test.*

/** The content store and the remote cache. */
object Cache extends DocSpec:

  def doc = page("Cache")(
    md"""
Compile and package are content-addressed. A blob's name is its digest. The store keeps a
project's keep set, and a clean removes entries older than the retention that no project
keeps.
""",
    cite[histos.cache.ContentStore](_.put),
    cite[histos.cache.ContentStore.type](_.Retention),
    section("Two digests")(
      md"""
The API digest is Zinc's public signature. A comment or a method body does not change it, so
a downstream compile hits. The product digest is the class bytes with line-number tables and
the TASTY attribute taken out, and with the checkout root removed from a path. Every other
string stays.
""",
      cite[histos.zinc.ApiDigest.type](_.of),
      cite[histos.zinc.ProductDigest.type](_.normalize),
    ),
    section("Remote")(
      md"""
`HISTOS_CACHE_URI` selects a remote. `http` and `https` use HTTP. `grpc` is plaintext and
`grpcs` is TLS. Unset is off. A bad value fails and does not fall back to off.

`HISTOS_CACHE_MODE` is `read` unless it is `write`. A laptop's `write` opens as `read`. Only
CI stores. `HISTOS_CACHE_NAMESPACE` is `main` unless it names `pr/<n>` or `dev/<name>`.
""",
      cite[histos.schedule.CacheConfig.type](_.decide),
      cite[histos.schedule.RemoteMode].signature,
    ),
    section("Lookup")(
      md"""
A lookup tries this namespace, then `main`, then the local store, and then runs the action.
`cache promote` copies an HTTP index from one namespace to another.
""",
      illustration {
        MermoidAscent.diagram(Figures.lookup)
      }.assert { ui =>
        val text = ui.toString
        assertTrue(text.contains("namespace"), text.contains("local store"))
      },
      cite[histos.schedule.CacheConfig.type](_.promote),
    ),
  )
end Cache
