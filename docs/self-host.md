# How Histos builds Histos

This is how the machinery works today. The toolchain is on the ship row, `Ship("histos").next(0, 3, 0)(api, app).toolchain(launcher = app, client = client)`. `publish local`, `publish snapshots`, and `publish release` pack the launcher, link the host client, and prove the pair before anything uploads. `assembly` is the fat-jar verb and takes module names. It does not install a toolchain. `histos client` is removed, and the error names `publish`. `histos launcher` is removed, and the error names `update`. `portal` stays.

## The pin is the builder

`histos.version` in `build.properties` names the published Histos that builds this tree. CI has no local build. Each job's setup action installs that toolchain, and every `./histos` command runs it. `histos.scala` is compiled against the api packed inside the running jar, so a change that adds build API this file uses needs a pin that has it.

The pin moves through a pull request:

1. Commit the change, so the snapshot coordinate names HEAD.
2. Run `./histos publish snapshots` on the laptop. That command packs the launcher, links the host client, and proves the pair before the Maven publish and before the `dist` upload.
3. Pin the commit channel, `<ship>-<sha>-SNAPSHOT`, in `build.properties`.
4. Push.

D20. When `workflow --update` would write the remote setup action, steps 2 and 4 run from a commit that is already on `main`, and the pin is its own pull request.

The pin is `aa4f835`, channel `0.3.0-aa4f8351e207-SNAPSHOT`. `publish snapshots` of that commit packed the launcher, linked the macOS client, proved the pair, and uploaded. The manifest names that jar and `macos-aarch64` only. A laptop publish stays host-only, so a Linux job on this pin runs `java -jar`. `./histos` installs `shim-<sha256>` when the user shim is missing, then execs it. The shim reads `.histos/toolchain` when it is set and otherwise this channel.

## Fetch the lock before the suite

The engine laws read compilers and other locked tools from the Coursier cache offline, because a test does not call Central. On a fresh clone, run `./histos resolve` once before `./histos test`. It fetches everything `dependencies.lock` records, including tools the build itself does not compile with, such as the Scala 3.9 compiler the TASTy laws use. CI's setup action runs the same command.

## A local toolchain

`./histos publish local` writes the Maven snapshot into Coursier's `ivy2Local` and, for a ship that names `.toolchain`, packs the launcher, links the host client, and proves the pair first. The pair is the new client running `version`, then `graph`, against the new jar, in a copy of HEAD. A failed handshake is `PairError.Handshake` and the install does not happen.

The install is `~/.histos/toolchains/<id>/`: `histos.jar`, this platform's `histos-client`, and `manifest.json`. The command writes `.histos/toolchain`, one path segment, the ship coordinate. The shim prefers that file over `build.properties` and does not fetch. A missing jar fails with `local toolchain <id> is not installed`. Delete the file to return to the pin. The tracked pin does not change.

`./histos assembly <module>` still writes that module's asset. For this repository `app`'s asset is `dist/histos.jar`. When `app` is the ship's launcher, a pack writes the ship's library members at `bootstrap/histos-api.jar` and `META-INF/histos/toolchain.json`: the ship coordinate, the full commit, and whether the tree was dirty. When the checkout has `install/histos`, the same pack writes `META-INF/histos/shim.sha256`, the digest the bootstrap wrapper installs. No source file carries the commit. Nothing writes `.histos/assembly.sha256`. The checkout script does not launch `dist/histos.jar`. An empty argument list is an error that names the assembly modules.

`./histos` is the bootstrap wrapper `workflow --update` writes for every build, including one with no assembly module. When `~/.histos/bin/histos` is missing it refuses a JDK older than `Toolchain.Floor`, installs `shim-<sha256>` from `dist`, checks that sha256, and execs the shim. It does not fetch `shim` or `shim.sha256`. The shim chooses the local toolchain or the pin and sets `HISTOS_VERSION`. The committed `./histos` in this repository is that wrapper. `workflow --check` compares the file to the pin.

There is no `client` command and no `launcher` command. A removed `./histos client` names `publish`. A removed `./histos launcher` names `update`.

`histos version` prints two lines: the toolchain id, then the server's id. With `.histos/toolchain` set, the id is that local toolchain. A local toolchain built from another commit, or from a dirty tree, prints one warning on stderr:

```text
local toolchain built from <commit> (dirty); this checkout is <head>
```

The `(dirty)` word is omitted when the pack was clean. `doctor` reports the same sentence as `toolchain.local`, a warning, and the exit stays 0. The command runs. There is no stop and no `.histos/allow-drift`.

A running server keeps the jar it started with. The next command compares the version. A different version reboots. The client notes the `server.json` stamp before it spawns, and after the socket accepts it reads only a record written strictly later, so it does not take the previous server's file. The restart error names the version:

```text
histos server at <socket> did not record version <wanted>; it has <got>
```

The successor starts with the requester's `HISTOS_VERSION`. An inherited `HISTOS_ASSEMBLY` is dropped and is not set.

## Snapshots

`./histos publish snapshots` writes the Maven snapshot and uploads to the GitHub release named `dist`:

- `channel-<ship>-<sha>-SNAPSHOT` and `channel-<ship>-SNAPSHOT`, each a file whose body is one timestamp. The channel is last
- `<timestamp>-manifest.json`, which names the jar's sha256 and each client's
- `<timestamp>-histos.jar`
- `<timestamp>-client-<platform>` for each file under `dist/clients/client-<platform>/histos-client`
- `shim-<sha256>`, `shim`, and `shim.sha256`, when the repository has `install/histos`. The pinned name is the digest, so those bytes do not move. `shim` and `shim.sha256` are the same bytes, and `self update` still fetches them. A repository without that file uploads no shim

The ship names `.toolchain(launcher = app, client = client)`. This layer links the host only. A platform that was not linked is omitted from the manifest, and none at all is refused before a token. A Mac publish names `macos-aarch64`. A Linux publish names `linux-x86_64`. A laptop does not cross-build the other platform, so a Linux job on this pin runs `java -jar`. The proof runs before the upload. `PairError.Handshake` refuses the Maven publish and the `dist` upload. The token is `GITHUB_TOKEN` or `gh auth token`, and the repository is `GITHUB_REPOSITORY` or the origin remote.

The generator renders `histos-snapshots.yml` as one `macos-26` job that runs `./histos publish local` and uploads `client-macos-aarch64`, then a publish job that needs that job, downloads `client-*`, installs Linux LLVM, and runs `./histos publish snapshots`. The committed workflow is the pin's render. `workflow --check` compares the file to the pin. A ship with no `.toolchain` publishes no client. A laptop publish also moves the shared pointer.

## Releases

`gh workflow run histos-release.yml --ref main` publishes the ship to Central. The generator, for a toolchain ship, renders a `macos-26` client job that runs `./histos publish local`, then a release job that needs it, installs Linux LLVM, runs `./histos publish release` (which proves the pair before the portal upload), and uploads the jar, `shim-<sha256>`, `shim`, `shim.sha256`, and the Linux client. The committed workflow is the pin's render. `workflow --check` compares the file to the pin. A ship with no `.toolchain` publishes no client. After the release is on Central, `histos bump` moves `.next`.

## The other self-host commands

- `histos launcher` is removed. The error names `update`, which moves the pin. Inside a project it rewrites `build.properties`. Outside one it rewrites `~/.histos/default`.
- `histos portal` binds a loopback Central portal and prints its base url. Set `HISTOS_PORTAL` to that url to rehearse a release. It needs `SONATYPE_USERNAME` and `SONATYPE_PASSWORD`.

`.bsp/histos.json` is tracked. Its `version` is the pin line, and a command rewrites it when the pin changes.
