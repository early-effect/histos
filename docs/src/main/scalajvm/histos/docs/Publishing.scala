package histos.docs

import specular.*

/** Ships, the pin, and the publish commands. */
object Publishing extends DocSpec:

  def doc = page("Publish")(
    md"""
A ship is a catalog row. The number on `.next` is the next release. SNAPSHOT is never
written in the catalog. `histos.version` in `build.properties` is a different value: it is
the Histos this project runs.
""",
    cite[histos.ShipDef].signature.elided(12),
    cite[histos.publish.LauncherPin.type](_.Key),
    section("Commands")(
      md"""
`publish local` writes the snapshot coordinate of HEAD into the local Ivy repository.
`publish snapshots` publishes that coordinate. `publish release` publishes the ship number
and requires `HISTOS_SESSION=release`. `publish assets` uploads a packaged file once the
deployment is validated. `publish promote` and `publish drop` finish a held deployment.
`bump` moves `.next` and does not change the pin.
""",
      cite[histos.cli.Help.type](_.entries).elided(12),
    ),
    section("The default release")(
      md"""
`HistosBuild.sona` is `Sona.layer(ReleaseMode.Automatic)`. Automatic publishes and waits
until the portal reports the deployment published.
""",
      cite[histos.HistosBuild](_.sona),
      cite[histos.ReleaseMode].signature,
    ),
  )
end Publishing
