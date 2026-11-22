package histos.docs

import specular.*

/** Pins, the shim, and the native client. */
object Install extends DocSpec:

  def doc = page("Install")(
    md"""
A project names the Histos it runs in `build.properties`. The key is `histos.version`. The
pin wins over the default under `~/.histos`.
""",
    cite[histos.publish.LauncherPin.type](_.Key),
    cite[histos.publish.LauncherPin.type](_.read),
    section("Three forms")(
      md"""
`LauncherPin.from` accepts three shapes. A release is a ship number. A pointer follows the
newest snapshot of that line. A revision freezes one build: a timestamp, or a commit pointer.
""",
      cite[histos.publish.LauncherPin.type](_.from),
    ),
    section("This process")(
      md"""
`HISTOS_PIN` selects the toolchain directory for this process. The value is one path segment,
the id under `~/.histos/toolchains`. It does not rewrite `histos.version`. `histos version`
prints the toolchain this command runs.
""",
      cite[histos.cli.Cli.type](_.versionText),
      cite[histos.cli.Help.Entry](_.name),
    ),
    section("The native client")(
      md"""
`compile`, `test`, `run`, `clean`, `fmt`, and `complete` can run through `histos-client` when
that binary was linked for this jar. `toolchain client` links it. In CI, `./histos` runs
`java -jar`. `toolchain install --client` is the opt-in that places the kit and links when
`CI` is set.
""",
      cite[histos.cli.Command].signature.elided(16),
    ),
    section("Shim commands")(
      md"""
`toolchain install`, `toolchain list`, `update`, and `self update` are listed in help. The
installed shim runs them.
""",
      cite[histos.cli.Help.type](_.entries),
    ),
  )
end Install
