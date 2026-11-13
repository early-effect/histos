# How Histos builds Histos

This is how the machinery works today. [cli-work-plan.md](cli-work-plan.md) is the plan that replaces most of it. The toolchain is on the ship row. `publish local` still installs nothing, and that install is the Dist layer. `assembly` is the fat-jar verb and takes module names. `histos client` is removed, and the error names `publish`. `histos launcher` is removed, and the error names `update`. `portal` stays.

## The pin is the builder

`histos.version` in `build.properties` names the published Histos that builds this tree. CI has no local build. Each job's setup action installs that toolchain, and every `./histos` command runs it. `histos.scala` is compiled against the api packed inside the running jar, so a change that adds build API this file uses needs a pin that has it.

The pin moves through a pull request:

1. Commit the change, so the snapshot coordinate names HEAD.
2. Run `./histos publish snapshots` on the laptop.
3. Pin the commit channel, `<ship>-<sha>-SNAPSHOT`, in `build.properties`.
4. Push.

## Fetch the lock before the suite

The engine laws read compilers and other locked tools from the Coursier cache offline, because a test does not call Central. On a fresh clone, run `./histos resolve` once before `./histos test`. It fetches everything `dependencies.lock` records, including tools the build itself does not compile with, such as the Scala 3.9 compiler the TASTy laws use. CI's setup action runs the same command.

## A local assembly

`./histos assembly app` compiles that module and the modules it depends on, and writes the module's asset. For this repository that path is `dist/histos.jar`. When `app` is the ship's launcher, the jar packs the ship's library members at `bootstrap/histos-api.jar` and writes `META-INF/histos/toolchain.json`: the ship coordinate, the full commit, and whether the tree was dirty. No source file carries the commit. The command does not write `.histos/assembly.sha256`. An empty argument list is an error that names the assembly modules.

The repository `./histos` is rendered by `workflow --update` from the ship number. It uses `HISTOS_ASSEMBLY` or `dist/histos.jar` when that jar's checksum matches `.histos/assembly.sha256`, and fails on a mismatch. With no local jar, it runs the installed shim. With a local jar and an executable `dist/histos-client`, it runs that client.

There is no `client` command. A removed `./histos client` names `publish`. `publish local`, `publish snapshots`, and `publish release` are the commands that link the client, and that link is the Dist layer, which has not landed. Until then the wrapper runs `dist/histos-client` only when that file is already there and executable.

A running server keeps the jar it started with. The next command compares the version line and the assembly checksum, and a mismatch reboots the server first. The replacement starts with the command's `HISTOS_VERSION` and `HISTOS_ASSEMBLY`, or without one the command does not have, never with the old server's.

When the local assembly's snapshot id is not what HEAD would publish, a command fails before it loads the build:

```text
toolchain is 0.2.10-7230ba21293f-SNAPSHOT; this checkout would publish 0.3.0-1b88d92a50e1-SNAPSHOT. Run histos assembly.
```

`assembly`, `version`, and `reboot` still run. A gitignored `.histos/allow-drift` turns the failure into a warning on the command line. The MCP `test` tool still refuses.

## Snapshots

`./histos publish snapshots` writes the Maven snapshot and uploads to the GitHub release named `dist`:

- `channel-<ship>-<sha>-SNAPSHOT` and `channel-<ship>-SNAPSHOT`, each a file whose body is one timestamp
- `<timestamp>-manifest.json`, which names the jar's sha256 and each client's
- `<timestamp>-histos.jar`
- `<timestamp>-client-<platform>` for each file under `dist/clients/client-<platform>/histos-client`

The ship names `.toolchain(launcher = app, client = client)`, so a publish of this ship expects a client for every dist platform. A platform with no file is omitted from the manifest, and none at all is refused before a token. A Mac links only `macos-aarch64` until the container item, so copy `dist/histos-client` to `dist/clients/client-macos-aarch64/histos-client` first. A laptop publish then has no Linux client, and CI on that pin runs `java -jar`. The token is `GITHUB_TOKEN` or `gh auth token`, and the repository is `GITHUB_REPOSITORY` or the origin remote.

`histos-snapshots.yml` is the same publish on GitHub runners. It is dispatch only. A toolchain ship links both clients on their own runners, assembles the launcher, and publishes with `HISTOS_BRANCH=default`. A ship with no `.toolchain` publishes no client. A laptop publish also moves the shared pointer.

## Releases

`gh workflow run histos-release.yml --ref main` publishes the ship to Central. A toolchain ship then assembles the launcher, links both clients on Ubuntu and macOS with Temurin 25, and uploads the jar, the clients, the manifest, and the shim to `dist`. A ship with no `.toolchain` publishes no client. After the release is on Central, `histos bump` moves `.next`.

## The other self-host commands

- `histos launcher` is removed. The error names `update`, which moves the pin. Inside a project it rewrites `build.properties`. Outside one it rewrites `~/.histos/default`.
- `histos portal` binds a loopback Central portal and prints its base url. Set `HISTOS_PORTAL` to that url to rehearse a release. It needs `SONATYPE_USERNAME` and `SONATYPE_PASSWORD`.

`.bsp/histos.json` is tracked. Its `version` is the pin line, and a command rewrites it when the pin changes.
