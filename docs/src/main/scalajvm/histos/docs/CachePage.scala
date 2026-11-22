package histos.docs

import mermoid.ascent.MermoidAscent
import specular.*

import zio.test.*

/** The content store and the remote cache. */
object Cache extends DocSpec:

  def doc = page("Cache")(
    md"""
Compiling and packaging are the slow parts of a build. Histos keeps their outputs so the
next run can reuse them when the inputs have not changed. The store is a directory of
blobs. A blob's name is the digest of its bytes, so two builds that produced the same
bytes share one file, and a changed input cannot accidentally read the old output.

Each project remembers which blobs it still needs. `histos cache clean` removes entries
older than the retention that no project is keeping. `histos cache path` prints the
directory. `histos cache list` prints each digest and its size.
""",
    cite[histos.cache.ContentStore](_.put),
    cite[histos.cache.ContentStore.type](_.Retention),
    section("What has to change before a module rebuilds")(
      md"""
Histos keeps two digests, because "nothing changed" means two different things.

The API digest is the public signature: the names and types other modules compile
against. Zinc computes it. A comment, or a change inside a method body, does not move the
signature. A downstream module then reuses its previous compile. That is the skip you
want when you fixed a typo in a comment.

The product digest is the class files as a later packager would see them. Line-number
tables and the TASTY attribute are removed first, and the absolute path of your checkout
is removed where it sits on a path boundary. Everything else stays. A change that would
affect a published jar changes this digest. A change that would not, does not.
""",
      cite[histos.zinc.ApiDigest.type](_.of),
      cite[histos.zinc.ProductDigest.type](_.normalize),
    ),
    section("Sharing a cache with CI")(
      md"""
A remote cache is optional. Set `HISTOS_CACHE_URI` to turn it on. `http` and `https` use
HTTP. `grpc` is plaintext and `grpcs` is TLS. Leave the variable unset and Histos stays
local. A value it does not understand fails the command. It does not quietly ignore the
variable.

`HISTOS_CACHE_MODE` is `read` unless you set `write`. A laptop that asks for `write` is
opened as `read`. Only CI stores results. That is so a developer's machine cannot publish
a bad output that every other machine then trusts.

`HISTOS_CACHE_NAMESPACE` separates those results. It is `main` unless you set `pr/<n>` for
a pull request or `dev/<name>` for a person. A pull request reads its own namespace
first, then `main`.
""",
      cite[histos.schedule.CacheConfig.type](_.decide),
      cite[histos.schedule.RemoteMode].signature,
    ),
    section("Where a lookup looks")(
      md"""
When an action can be cached, Histos looks in this order: the namespace you named, then
`main`, then the local store. The first hit is used. If nothing hits, the action runs.
`histos cache promote --from <namespace> --to <namespace>` copies an HTTP index from one
namespace to another, which is how a merged pull request becomes the `main` cache.
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
