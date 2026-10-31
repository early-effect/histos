# Histos

Histos is a Scala build tool. The build is data. The work is a ZIO effect. A person and an agent ask for the same operation and read the same result.

The terminal prints that result and chooses an exit. An agent receives the result as data and sees each line as it is printed. An editor uses BSP. If the work ran, the result is a report, including a compile or a test that failed. If the request never started, the result is a fault.

This is pre-release. The API may break when that makes it more honest. Other repositories take snapshots.

What we still intend is [next](docs/next.md).

## A build

A build file is a Scala program that returns a graph. A module names its platforms (`jvm`, `js`, `native`), its Scala versions, and its dependencies. A literal that is not a version, a dependency, or a name does not compile. This repository is built by [histos.scala](histos.scala). The literals are [Literals.scala](api/src/histos/Literals.scala).

## This repository

Histos builds itself. [histos](histos) and [histos.bat](histos.bat) come from `workflow --update`. Each script finds a JVM and starts one jar.

`HISTOS_ASSEMBLY`, when set, is that jar. Otherwise `dist/histos.jar` is that jar when the file exists. Either jar has to match `.histos/assembly.sha256`. A missing file or a different checksum fails, and the script does not fall through to the lock. With neither jar, the script downloads the snapshot named in [.histos/launcher.lock](.histos/launcher.lock) and checks its sha256. `dist/` and `.histos/assembly.sha256` stay uncommitted, so CI has neither jar and every job runs the pin. A local jar can compile [histos.scala](histos.scala) while CI still runs that pin. `workflow --check` compares the files this jar renders with the files in the tree, so the two jars can disagree. Do not hand-edit the lock.

[histos.scala](histos.scala) compiles against `bootstrap/histos-api.jar` packed inside the running jar. `.histos/classes/api` is the api compiled for the modules after that.

`assembly` compiles with the running jar and writes `dist/histos.jar` unless a path is given. It packs the api it just compiled at `bootstrap/histos-api.jar` inside that jar. Only the default path writes `.histos/assembly.sha256`. The next `./histos` in this checkout runs `dist/histos.jar` when that checksum matches.

`launcher --update` is the only command that writes the lock. It reads the repository already named there, takes the latest snapshot revision of `rocks.earlyeffect:histos` from Maven metadata, and writes that revision and the published jar's sha256. It does not hash `dist/histos.jar`. Commit the lock after it changes. The next CI run uses that jar, and [histos.scala](histos.scala) compiles against the api packed inside it.

The script version is the ship number. The lock version is that number, or that number with `-SNAPSHOT` appended. This checkout's lock is `0.0.1-SNAPSHOT`. [histos.scala](histos.scala) calls `ScalacFlag.MaxInlines` on `app`. The jar named by the lock was published before that flag existed, so CI cannot load this build file until a launcher that contains it is the pin.

## Publishing

The decision is [publishing.md](docs/publishing.md). `publishLocal`, `publish snapshots`, and `publish release` are three commands. The last two close the build's `sona` layer. The staged revision chooses the Sonatype branch. The command name does not.

- A snapshot (`<ship>-SNAPSHOT`, or the unique `<ship>-<sha>-SNAPSHOT`) is a PUT to the Central snapshot repository. It needs `SONATYPE_USERNAME` and `SONATYPE_PASSWORD`. It does not sign.
- A release number is signed, then uploaded as one portal bundle.
- A `-ci` coordinate is not a Sonatype publish. `publish snapshots` and `publish release` fail that session before staging. `publishLocal` writes it.

This catalog is `manual("histos", manual"0.0.1")`. That number is a release, so `publish snapshots` and `publish release` both sign it and portal it. A new launcher snapshot is a publish of a snapshot coordinate, then `launcher --update`.

The default `sona` is `Sona.layer(ReleaseMode.Automatic)`. A release bundle is published in that command, and `.histos/deployments/<ship>` is written as `PUBLISHED`. A build that overrides `sona` with `Sona.layer(ReleaseMode.Hold)` stops at `VALIDATED`. `--automatic` forces Automatic for that one run. There is no `--hold`. The snapshot branch ignores the mode.

`publish release` does not stage a snapshot coordinate. It reports that ship and continues with the others. A ship that depends on a snapshot is reported and not uploaded. One ship's upload failure does not fail the others. Missing credentials, a missing signing key, or a portal base that cannot be read fails the whole invocation before compile, when the layer's secret names are the five below. `publish snapshots` checks the username and password only. A layer whose secret names differ skips the check.

Unset, `PGP_SECRET` is the key and `PGP_PASSPHRASE` unlocks it. `PGP_KEY_HEX`, when set, selects that fingerprint. `HISTOS_SIGNER=gpg` signs with gpg, and then the fingerprint is required. The release workflow lists `SONATYPE_USERNAME`, `SONATYPE_PASSWORD`, `PGP_SECRET`, `PGP_PASSPHRASE`, and `PGP_KEY_HEX`.

`publishLocal` installs where Coursier resolves `ivy2Local`. A token is a ship, an aggregate, or a module. No token publishes every ship the command can write. It does not take a directory, it does not write `~/.m2`, and it does not sign. It accepts a snapshot, a release, and a `-ci` coordinate.

`publish promote <deployment>` publishes a `VALIDATED` deployment. `publish drop <deployment>` drops the deployment first, then deletes the GitHub asset when that ship names one. A failed delete leaves the deployment dropped.

`publish assets` uploads the file named on an assembly. A named ship fails unless `.histos/deployments/<ship>` says `VALIDATED`. No name skips a ship with no file, or a file in another state. This repository writes `PUBLISHED`, and [histos-release.yml](.github/workflows/histos-release.yml) runs `./histos publish assets histos` after `./histos publish release histos`. That step fails: `histos is PUBLISHED, not VALIDATED`.

The workflow is dispatch only. A tag does not publish. A ship that names an asset runs `./histos assembly`, then `./histos publish release <ship>`, then `./histos publish assets <ship>`. A library ship runs `publish release` only. The file does not pass `--automatic`. This build is Automatic anyway, because that is the default layer. Hold is the build that also renders `histos-promote.yml`. That file's only input is the deployment id, and its command is `./histos publish promote`.

Rehearse with `./histos portal`. It binds loopback and prints a base URL. Set `HISTOS_PORTAL` to that URL. Unset, the client uses `https://central.sonatype.com`. [ReleaseSpec](engine/test/ReleaseSpec.scala) runs that client against [PortalServer](central/src/histos/central/PortalServer.scala) and does not call Central.

The ship `histos` publishes `api`, a library, and `app`, an assembly. The assembly names `dist/histos.jar`. That file is not a Maven artifact and it is not in the portal bundle. The pom carries the organization description and url, which Central requires. Histos does not create the tag or the GitHub release. Humans create the GitHub release. [AssetSpec](engine/test/AssetSpec.scala) is a loopback stand-in and does not call GitHub.

`ci.yml` does not publish and does not run on a tag. The release job does not run the suite again. After `0.0.1` is on Central, the next development catalog is a snapshot. The lock stays a snapshot of the ship number so `./histos` still boots.

## An operation

You ask for one thing: compile, test, run, publishLocal, publish snapshots, publish release, publish assets, publish promote, publish drop, assembly, clean, resolve, or a contributed plugin command. `testOnly` and `testQuick` are that same test operation. `preview` of compile, test, publishLocal, publish snapshots, clean, cleanFull, or run writes nothing. Compile and test name the sources the last analysis says are stale, and a test preview names the suites a quick run would skip. When that analysis or digest cannot be read, it says it does not know. `observe` reads the last tally and does not compile. `cancel` names the run id the command minted. `server` keeps a warm process. `mcp` and `bsp` open a transport. They are not operations.

The tally is source files, class files written this round, and modules.

## Tools already on the machine

Histos does not install a JDK or Node. The running JVM must be JDK 21 or newer. A JavaScript module also needs Node 25 or newer on `PATH`. A newer install meets the floor. `.histos/toolchain.local` and `.histos/node.local` name a home you already chose. A file that points at a missing or too-old home fails. Histos does not pick a different one.

`resolve --update` solves dependencies and writes `.histos/dependencies.lock`. Later runs fetch what the lock recorded and check the checksums. `launcher --update` is the only command that moves [.histos/launcher.lock](.histos/launcher.lock).

## Plugins

A plugin is a Scala 3 library with one [HistosPlugin](api/src/histos/plugin/HistosPlugin.scala). The build file names it in the header and registers it with `use`. It compiles against `histos.api`. It does not see the engine. [PluginSpec](api/test/PluginSpec.scala) locks that contract.

## Scenarios

A scenario checks a build by standing it up. One content store, one or more checkouts, sources written by the test, and an assertion on the result. That is how Histos is checked against itself: two copies of a project share a cache, a clean restores, and a skip that hides a change fails. The same shape is the Histos side when the other side is sbt and zipx. That comparison is the bench in [the cache plan](docs/cache-work-plan.md).

The decisions are [scenario-work-plan.md](docs/scenario-work-plan.md). The code is [Scenario.scala](engine/test/Scenario.scala), proved by [ScenarioSpec.scala](engine/test/ScenarioSpec.scala). A scenario is a test. It is not a command, and it is not a second language.

## License

Apache License 2.0. See [LICENSE](LICENSE).
