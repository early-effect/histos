package histos.docs

import specular.*

/** A plugin is a service the build turns on. A coordinate does not install it. */
object Plugins extends DocSpec:

  def doc = page("Plugins")(
    md"""
A plugin adds a capability: a command, a task, or a CI job. Putting a jar on the
classpath does not turn that capability on. Two files share the work.

`histos.plugins.scala` only names coordinates. It is read before any plugin class exists,
so it cannot import one. It says where to resolve the plugin from, and whether the lock
must match.

`histos.scala` turns a capability on by its Scala type. You provide a `ZLayer` for that
service. If the build never registers the service, the plugin's commands are not there,
even if the jar was downloaded.
""",
    cite[histos.plugin.HistosPlugins](_.sources),
    cite[histos.plugin.BuildServices].signature,
    section("What you publish for the next machine")(
      md"""
A plugin library is published so a later Histos can load it. The default publication is
rebuildable: it includes the sources and the inputs a later compiler needs, so a host on
a newer Scala can recompile the plugin. `PluginPublication.BinaryOnly` keeps the binding
metadata and skips that recipe. Use binary-only when you are shipping a closed binary and
you do not want a later host to rebuild it.
""",
      cite[histos.PluginPublication].signature,
    ),
    section("Specular, as a worked example")(
      md"""
This documentation site is built by Specular, which is a Histos plugin. The build names
the docs module and the `DocsSite` main, then registers the service twice: once so the
plugin can contribute commands, and once so it can contribute tasks. One layer is shared,
so both registrations see the same site.

`linkClient` copies the linked browser script to `assets/client.js`. That script is what
reloads the preview tab when the site is rebuilt.

You build the site with `./histos run dev.earlyeffect.specular/site`. You serve it with
`./histos run dev.earlyeffect.specular/preview`, which leaves the server up. `preview` is
already a Histos command, so the plugin's command is named `serve` and it runs that same
task. `./histos serve` is the short form. `./histos ~ run dev.earlyeffect.specular/site`
rebuilds when the docs sources change, and the preview reloads.
""",
      cite[histos.plugin.Site](_.publish),
    ),
  )
end Plugins
