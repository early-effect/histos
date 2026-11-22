package histos.docs

import specular.*

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
    section("Which command to run")(
      md"""
`histos publish local` writes the snapshot coordinate of your current commit into the
local Ivy repository, so another project on this machine can depend on it before you
publish anywhere else.

`histos publish snapshots` publishes that same coordinate to the snapshot repository.

`histos publish release` publishes the ship number. It runs only when
`HISTOS_SESSION=release`, which is how a person and a CI job agree that this invocation
is a release and not an accident.

`histos publish assets` uploads a packaged file after the deployment is validated and
the GitHub release exists. `publish promote` and `publish drop` finish a deployment that
was held for a check. `histos publish withdraw` removes the local Ivy revisions and the
toolchain directory for this commit. The pin stays.

`histos bump` moves `.next` once a published stable has caught the number that is there.
It does not change `histos.version`.
""",
      cite[histos.cli.Help.type](_.entries).elided(12),
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
