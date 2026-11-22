package histos.docs

import specular.*

/** How a person installs Histos, then how a project pins it. */
object Install extends DocSpec:

  def doc = page("Install")(
    md"""
Histos is one program you install once, then point a project at the version you want it to
use. It runs on macOS and Linux. On Windows, use WSL. It needs a JDK 25 or newer, and it
does not install that JDK for you. On macOS, `brew install --cask temurin@25` is enough. On
Linux, install Temurin 25.
""",
    section("Run the install script")(
      md"""
The installer and the `histos` command are the same POSIX script. From any directory:

```sh
curl -fsSL https://github.com/early-effect/histos/releases/download/dist/shim | sh
```

That downloads the script and runs it with no arguments. It does five things.

It fetches the script and a sha256 from the Histos release. The default place is
`https://github.com/early-effect/histos/releases/download/dist`. `HISTOS_DIST` can name
another place. That URL has to be `https`, or `http` on this machine's loopback. Anything
else is refused.

It checks the downloaded file against the sha256. A mismatch stops the install.

It writes the script to `~/.histos/bin/histos` and marks it executable. Set `HISTOS_HOME` if
that directory should live somewhere else. The script does not edit your shell startup
files.

If `~/.histos/default` is missing or empty, it records a default toolchain. It uses a
published channel file when the release has one. Otherwise it takes the newest stable
release listed there. If there is no release, it takes the newest snapshot line. If it
cannot find either, it stops and tells you to run `histos toolchain install <version>`.

It then prints one line and exits:

```sh
export PATH="$$HOME/.histos/bin:$$PATH"
```

Add that directory to your `PATH` for this shell. Add the same line to your shell startup
file if you want `histos` next time you open a terminal. Then run `histos version`. That
prints the toolchain this command is using. The first command that needs the jar downloads
it under `~/.histos/toolchains`.
"""
    ),
    section("A project can choose a different Histos")(
      md"""
The default under `~/.histos` is what you get outside a project. Inside a project,
`build.properties` can name the Histos that project runs. The key is `histos.version`. That
pin wins over the default. A teammate who clones the repo gets the same Histos you pinned,
once they have the `histos` command on their `PATH`.
""",
      cite[histos.publish.LauncherPin.type](_.Key),
      cite[histos.publish.LauncherPin.type](_.read),
    ),
    section("Three ways to write the pin")(
      md"""
A release pin is a ship number, such as the stable you want frozen. A pointer ends in
`SNAPSHOT` and follows the newest build of that line. A revision pin freezes one build: a
timestamp, or a commit. `histos update` rewrites the pin. `--follow` keeps a moving line.
`--freeze` records the exact build you have.
""",
      cite[histos.publish.LauncherPin.type](_.from),
    ),
    section("One command, one toolchain")(
      md"""
`HISTOS_PIN` selects the toolchain for this process only. The value is a single directory
name under `~/.histos/toolchains`. It does not rewrite `build.properties`. Use it when you
want to try a different installed Histos without changing the project. `histos version`
prints which one this command is actually running, and, if a server is already up, that
server's version too.
""",
      cite[histos.cli.Cli.type](_.versionText),
      cite[histos.cli.Help.Entry](_.name),
    ),
    section("The native client")(
      md"""
Most commands run on the JVM. `compile`, `test`, `run`, `clean`, `fmt`, and `complete` can
instead go through `histos-client`, a native binary linked beside the jar. That binary is
optional. The install script does not build it, and it does not install clang or the
Coursier `cs` tool. `histos toolchain client` links the client for the jar you have. In CI,
`./histos` stays on `java -jar`. `histos toolchain install --client` is how a CI job opts in
and links. Passing `--client` and `--no-client` together fails.
""",
      cite[histos.cli.Command].signature.elided(16),
    ),
    section("Commands the shim handles itself")(
      md"""
`toolchain install`, `toolchain list`, `update`, and `self update` run in the script, before
a project is loaded. `toolchain install <version>` downloads one toolchain. `toolchain list`
prints the ones you have. `self update` replaces the script when the published hash differs.
The help catalog lists them with the rest of the commands.
""",
      cite[histos.cli.Help.type](_.entries),
    ),
  )
end Install
