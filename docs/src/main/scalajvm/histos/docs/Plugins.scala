package histos.docs

import specular.*

/** A plugin is a service the build turns on. A coordinate does not install it. */
object Plugins extends DocSpec:

  def doc = page("Plugins")(
    md"""
`histos.plugins.scala` names coordinates and does not import a plugin class. `histos.scala`
turns a capability on by its interface. A coordinate on a classpath is not a service.
""",
    cite[histos.plugin.HistosPlugins](_.sources),
    cite[histos.plugin.BuildServices].signature,
    section("Publication")(
      md"""
`.pluginLibrary` publishes a rebuildable bundle: sources and the inputs a later host can
recompile. `.pluginLibrary(PluginPublication.BinaryOnly)` keeps binding metadata and no
recipe. Rebuildable is the default.
""",
      cite[histos.PluginPublication].signature,
    ),
    section("Specular")(
      md"""
Specular is a plugin. `Site.forProject` names the docs module and the `DocsSite` main.
`Specular.layer` builds the plugin from that `Site`. This repository registers it with
`.add[Specular]` and `.tasks[Specular]` on one layer.

`linkClient` copies the linked docs script to `assets/client.js`. That script reloads the
tab when the preview stamp changes.

`./histos run dev.earlyeffect.specular/preview` leaves the preview server up. `preview` is
already a Histos command, so the contributed command is `serve` and it runs that task.
`./histos ~ run dev.earlyeffect.specular/site` rebuilds when the docs module changes. The
server stays up and reloads the tab.
""",
      cite[histos.plugin.Site](_.publish),
    ),
  )
end Plugins
