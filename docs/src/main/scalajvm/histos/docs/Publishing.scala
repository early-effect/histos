package histos.docs

import specular.*

import zio.ZIO
import zio.test.*

/** Ships, the pin, and the publish commands. */
object Publishing extends DocSpec:

  def doc = page("Publish")(
    md"""
Publishing is how a module becomes a coordinate other builds can depend on. Histos keeps
two different version numbers, and mixing them up is the usual mistake.

A ship is a row in the catalog inside `histos.scala`. It names the modules that publish
together and the next release number, written as `.next(major, minor, patch)`. You never
write `SNAPSHOT` on that row. When you publish a snapshot, Histos derives the snapshot
coordinate from the commit. When you publish a release, it publishes the number on
`.next`.

`histos.version` in `build.properties` is not that number. It is the Histos toolchain
this project runs. Changing the ship does not change the toolchain, and `histos bump`
does not rewrite the pin.
""",
    cite[histos.ShipDef].signature.elided(12),
    cite[histos.publish.LauncherPin.type](_.Key),
    section("Publication metadata is not a network URL")(
      md"""
Use `MetadataUri("http://...")` or `MetadataUri("https://...")` for license,
SCM, developer and organization homepages, and organization/ship publication URLs.
Literals are checked at compilation. `MetadataUri.from(raw)` validates runtime input
and returns `Either[MetadataUriError, MetadataUri]`; keep that typed error when
lifting it into ZIO. `url"..."` still constructs `HttpUrl`, the HTTPS network
capability. It no longer fits publication URL parameters. There is no implicit
conversion between the two.

POM descriptions prefer module, then ship, then organization. The POM name is the
unsuffixed artifact base (or module name). Developer identity, email and homepage,
organization display name/homepage, SCM connections, license and version scheme
are publication facts, not runtime dependencies.

A build using these APIs needs a matching Histos snapshot toolchain, including its
API and daemon. Install that snapshot and pin it in `build.properties` before
loading the migrated build; reboot any older daemon. An older installed shim is not
a validated bootstrap for new build declarations. The repository README describes
the handoff without relying on a temporary source-launcher script.
""",
      cite[histos.MetadataUri.type](_.from),
      cite[histos.MetadataUriError].signature,
      exampleValue {
        import histos.*

        License(license"Apache-2.0", MetadataUri("http://www.apache.org/licenses/LICENSE-2.0.txt")).url.value
      }.assert(uri => assertTrue(uri == "http://www.apache.org/licenses/LICENSE-2.0.txt")),
      exampleZIO {
        import histos.*

        val parsed = ZIO.fromEither(MetadataUri.from("https://example.org/project"))
        parsed.map(_.value)
      }.assert(uri => assertTrue(uri == "https://example.org/project")),
    ),
    section("Which command to run")(
      md"""
`histos publish check` compiles the published JVM members the diff selects and compares
them. It writes no Ivy files and moves no snapshot pointer. `histos publish local` writes
the snapshot coordinate of your current commit into the local Ivy repository, so another
project on this machine can depend on it before you publish anywhere else. Both follow
the same selection as `histos affected`: a build file, a failed diff, `--base all`, or no
base compares every published JVM member. A member the diff did not select is named and
is not reported as compatible.

`histos publish snapshots` publishes that same coordinate to the snapshot repository.
`histos publish release` publishes the ship number. It runs only when
`HISTOS_SESSION=release`, which is how a person and a CI job agree that this invocation
is a release and not an accident. Both compare every published JVM member and every
declared Scala version before any upload. `--base` does not narrow them.

`histos publish assets` uploads a packaged file after the deployment is validated and
the GitHub release exists. `publish promote` and `publish drop` finish a deployment that
was held for a check. `histos publish withdraw` removes the local Ivy revisions and the
toolchain directory for this commit. The pin stays.

`histos bump` moves `.next` once a published stable has caught the number that is there.
It does not change `histos.version`.
""",
      cite[histos.cli.Help.type](_.entries).elided(12),
    ),
    section("What kind of bump")(
      md"""
`histos publish check` and `histos publish local` compare the published JVM members the
diff selects, on every declared Scala version of those members. `histos publish snapshots`
and `histos publish release` compare every published JVM member before upload. A coordinate
with no stable release is an explicit first publication, not a claim that a baseline was
compared. Missing current classes, unreachable metadata, missing baseline jars, and failed
analysis are incomplete verification and fail the check. The release area of `histos doctor`
checks the session, the releases file, the bump, and the pin. It does not read classfiles.

The `CompatibilityAnalyzer` service currently uses MiMa for backward JVM classfile
compatibility only. It does not prove source, behavioral, TASTy, Scala.js IR, or Native ABI
compatibility. JS and Native requests return `UnsupportedBackend`; future analyzers can
implement the same service with backend-specific evidence. Compile/link and artifact
inventory checks for those backends remain separate.

Baseline resolution is separate from bump policy. On a `0.x` line any binary difference
requires a minor bump. On stable lines a missing class requires a major bump and other
binary problems require a minor bump. A catalog number that already covers the required
component passes.

Job preflight is product-independent: it checks prerequisites before compilation,
not compatibility of classes that do not exist yet. A pin host's compatibility job runs
`histos publish check`. A local host's publish job runs `histos publish local`. Neither
compiles the world and then asks doctor to read classfiles. Missing or incomplete
evidence fails closed.

`publish check` compiles only the selected closure, because it writes nothing. `publish
local`, `publish snapshots`, and `publish release` still compile and write what they
publish today. Check and local narrow only the MiMa set. Snapshots and release compare
every JVM member and Scala version of each ship that contains a selected publication.
Selecting fewer publication tokens does not reduce that full gate to one representative
member. An empty selection succeeds and says nothing was selected. It is not a full-proof
claim. Explicit first publication needs no invented baseline. Ordinary compile and test
do not fetch release metadata or compatibility baselines; JS and Native linking and
inventory are separate proof.

`histos bump` rewrites `.next`. It does not decide the component. The default is the
next patch. A binary difference is reported by publish, not by doctor.
""",
      cite[histos.version.BumpPart](_.token),
    ),
    section("What happens after the bundle is uploaded")(
      md"""
The default release publishes to Maven Central's portal and waits until the portal says
the deployment is published. You do not have to promote it by hand. A build that wants
the other behavior sets a different release mode. Automatic is the one Histos itself
uses.
""",
      cite[histos.HistosBuild](_.sona),
      cite[histos.ReleaseMode].signature,
    ),
  )
end Publishing
