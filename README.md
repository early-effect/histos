# Histos

Histos is a Scala build tool. The build is data. The work is a ZIO effect. A person and an agent ask for the same operation and read the same result.

The terminal prints that result and chooses an exit. An agent receives the result as data and sees each line as it is printed. An editor uses BSP. If the work ran, the result is a report, including a compile or a test that failed. If the request never started, the result is a fault.

This is pre-release. The API may break when that makes it more honest. Other repositories take snapshots.

What we still intend is [next](docs/next.md).

## A build

A build file is a Scala program that returns a graph. A module names its platforms (`jvm`, `js`, `native`), its Scala versions, and its dependencies. A version, a dependency, a path, a url, and a name are typed literals. A bad one does not compile the build file. This repository is built by [histos.scala](histos.scala). The literals are [Literals.scala](api/src/histos/Literals.scala).

## This repository

Histos builds itself. [histos](histos) and [histos.bat](histos.bat) come from `workflow --update`. Each script finds a JVM and starts one jar.

`HISTOS_ASSEMBLY`, when set, is that jar. Otherwise `dist/histos.jar` is that jar when the file exists. Either jar has to match `.histos/assembly.sha256`. A missing file or a different checksum fails, and the script does not fall through to the lock. With neither jar, the script downloads the snapshot named in [.histos/launcher.lock](.histos/launcher.lock) and checks its sha256. `dist/` and `.histos/assembly.sha256` stay uncommitted, so CI has neither jar and every job runs the pin. A local jar can compile [histos.scala](histos.scala) while CI still runs that pin. `workflow --check` compares the files this jar renders with the files in the tree, so the two jars can disagree. Do not hand-edit the lock.

[histos.scala](histos.scala) compiles against `bootstrap/histos-api.jar` packed inside the running jar. `.histos/classes/api` is the api compiled for the modules after that.

`assembly` compiles with the running jar and writes `dist/histos.jar` unless a path is given. It packs the api it just compiled at `bootstrap/histos-api.jar` inside that jar. Only the default path writes `.histos/assembly.sha256`. The next `./histos` in this checkout runs `dist/histos.jar` when that checksum matches.

`launcher --update` is the only command that writes the lock. It reads the repository already named there, takes the latest snapshot revision of `rocks.earlyeffect:histos` from Maven metadata, and writes that revision and the published jar's sha256. It does not hash `dist/histos.jar`. Commit the lock after it changes. The next CI run uses that jar, and [histos.scala](histos.scala) compiles against the api packed inside it.

The script version is the ship number, or the lock cannot boot. The lock version is that number, or that number with `-SNAPSHOT` appended. This checkout's script stays `0.0.1` and the lock stays `0.0.1-SNAPSHOT`, which is the pin that contains `ScalacFlag.MaxInlines`. The ship number is `0.0.2`. A script of `0.0.2` waits until the pin is `0.0.2-SNAPSHOT`.

## Publishing

The decision is [publishing.md](docs/publishing.md). `publishLocal`, `publish snapshots`, and `publish release` are three commands. The last two close the build's `sona` layer. The staged revision chooses the Sonatype branch. The command name does not.

`publishLocal`, `publish snapshots`, and `publish release` stage the same files. A library module stages its jar. An assembly stages the packaged program. Each also stages the pom, the sources jar, and the javadoc jar. `publishLocal` writes those files where Coursier resolves `ivy2Local` and does not sign. A snapshot coordinate is a PUT of those files and does not sign. A release signs those files, then uploads one portal bundle.

- A snapshot needs `SONATYPE_USERNAME` and `SONATYPE_PASSWORD`.
- A release also needs `PGP_SECRET`, `PGP_PASSPHRASE`, and `PGP_KEY_HEX`.
- A `-ci` coordinate is not a Sonatype publish. `publish snapshots` and `publish release` fail that session before staging. `publishLocal` writes it.

This catalog is `manual("histos", manual"0.0.2")`. That number is a release, so `publish snapshots` and `publish release` both sign it and portal it. `0.0.1` is already on Central. A new launcher snapshot is a publish of a snapshot coordinate, then `launcher --update`.

The default `sona` is `Sona.layer(ReleaseMode.Automatic)`. That upload sends `publishingType=AUTOMATIC` and polls until `PUBLISHED` or `FAILED`. The deployment file is written as `PUBLISHED`. A build that overrides `sona` with `Sona.layer(ReleaseMode.Hold)` sends `USER_MANAGED` and polls until `VALIDATED` or `FAILED`. `--automatic` forces Automatic for that one run. There is no `--hold`. The snapshot branch ignores the mode.

`publish release` does not stage a snapshot coordinate. It reports that ship and continues with the others. A ship that depends on a snapshot is reported and not uploaded. One ship's upload failure does not fail the others. Missing credentials, a missing signing key, or a portal base that cannot be read fails the whole invocation before compile, when the layer's secret names are the five below. `publish snapshots` checks the username and password only. A layer whose secret names differ skips the check.

Unset, `PGP_SECRET` is the key and `PGP_PASSPHRASE` unlocks it. `PGP_KEY_HEX`, when set, selects that fingerprint. `HISTOS_SIGNER=gpg` signs with gpg, and then the fingerprint is required. The release workflow lists `SONATYPE_USERNAME`, `SONATYPE_PASSWORD`, `PGP_SECRET`, `PGP_PASSPHRASE`, and `PGP_KEY_HEX`.

A token is a ship, an aggregate, or a module. No token publishes every ship the command can write. `publishLocal` does not take a directory and it does not write `~/.m2`. It accepts a snapshot, a release, and a `-ci` coordinate.

`publish promote <deployment>` publishes a `VALIDATED` deployment. `publish drop <deployment>` drops the deployment first, then deletes the GitHub asset when that ship names one. A failed delete leaves the deployment dropped.

`publish assets` uploads the file named on an assembly when `.histos/deployments/<ship>` says `VALIDATED` or `PUBLISHED`, and a GitHub release for that tag already exists. A named ship in another state fails. No name skips a ship with no file, or a file in another state. The command does not create the release. The release workflow does not run it.

The workflow is dispatch only. A tag does not publish. Each ship runs `./histos publish release <ship>`, which signs the staged files and uploads that bundle. The file does not pass `--automatic`. This build is Automatic anyway, because that is the default layer. Hold is the build that also renders `histos-promote.yml`. That file's only input is the deployment id, and its command is `./histos publish promote`.

Rehearse with `./histos portal`. It binds loopback and prints a base URL. Set `HISTOS_PORTAL` to that URL. Unset, the client uses `https://central.sonatype.com`. [ReleaseSpec](engine/test/ReleaseSpec.scala) runs that client against [PortalServer](central/src/histos/central/PortalServer.scala) and does not call Central.

The ship `histos` publishes `api`, a library, and `app`, an assembly. `engine` and `central` are not members. The three publish commands package `app` the same way. A release signs that jar. `assembly` writes `dist/histos.jar` for this checkout. That file is the local launcher, not a second publication. The pom carries the organization description and url, which Central requires. A release also needs a license, a developer, and an scm, or the command fails before upload. Histos does not create a tag or a GitHub release. [AssetSpec](engine/test/AssetSpec.scala) is a loopback stand-in and does not call GitHub.

`ci.yml` does not publish and does not run on a tag. The release job does not run the suite again. After `0.0.2` is on Central, the next development catalog is a snapshot. The lock stays a snapshot of the ship number so `./histos` still boots.

## An operation

`version` prints the ship number. `graph` prints each project with its platforms and Scala versions, then each plugin. Environment values are left out. `toolchain` prints the JDK a compile would use, Node when a module is JavaScript, and clang when a module is native.

`compile` takes project names. No name compiles every module. `clean` takes project names too. No name wipes every product root under `.histos` and does not read the build. A name wipes that module. An aggregate expands to its modules, and upstream stays. Locks, the assembly, and the server stay. `cleanFull` takes no project. It wipes those products, then drops this project's content-store entries. `assembly` takes an optional path.

`test` runs the suites. The default `testStore` is `Off`, so a full run launches every suite. `testQuick` skips a suite whose stored fingerprint still matches. `testOnly` takes `--module` and at least one pattern. `testQuick` takes the same flags, and its patterns may be empty. `Local` skips on `test` as well. `Remote` does not skip and does not upload. `run <name> [jvm|js|native]` runs that module. `run inspect` prints the project names. `run versions` prints the version decision. `run <plugin>/<task>` runs a plugin task.

`publishLocal`, `publish snapshots`, `publish release`, `publish assets`, `publish promote`, and `publish drop` are the commands in Publishing. `preview` of `compile`, `test`, `publishLocal`, `publish snapshots`, `clean`, `cleanFull`, or `run` writes nothing. Compile and test name the sources the last analysis says are stale. A test preview names the suites a quick run would skip and the ones it would run. When the analysis cannot be read, the line says the staleness is unknown. When a digest cannot be read, the line says the suite is not known to be skippable.

`~` runs a command again after a source save. The server does that when one is up. This process does it otherwise. `mcp`, `bsp`, `server`, `portal`, `observe`, and a nested `~` are usage and do not start that loop. `observe` reads the stored tally and does not load the graph. `cancel` names the run id the command minted. `server` keeps a warm process. `reload` asks that process to load the build again, and fails when no server is up. `reboot` replaces a running server, or starts one. `mcp` and `bsp` open a transport.

`cache path` prints the store. `cache list` prints each digest and its size. `cache clean` removes an entry older than 30 days that no project keeps. `--dry-run` names those entries and leaves them. `cache promote --from` and `--to` take `main`, `pr/<n>`, or `dev/<name>`. The remote cache is not configured, so that command reports `remote cache is not configured` and does not open a connection.

`resolve` fetches what `.histos/dependencies.lock` recorded and checks each sha256. `resolve --update` solves and writes that file. `resolve --check` solves again and fails when the result is not the lock. `workflow` compares the rendered workflow files and the launcher scripts with the tree. `workflow --update` writes them. Neither publishes, and neither moves the launcher pin. `launcher --update` is the only command that writes [.histos/launcher.lock](.histos/launcher.lock).

A word the grammar does not own is a plugin command, when the build registered it. A reserved root word cannot be one.

The tally names files compiled, files skipped, modules, and classes written. A count that is unknown is omitted. A failure count is added when one happened.

## Tools already on the machine

Histos does not install a JDK or Node. The running JVM must be JDK 21 or newer. A JavaScript module also needs Node 25 or newer on `PATH`. A newer install meets the floor. `.histos/toolchain.local` and `.histos/node.local` name a home you already chose. A file that points at a missing or too-old home fails. Histos does not pick a different one.

## Plugins

A plugin is a Scala 3 library with one [HistosPlugin](api/src/histos/plugin/HistosPlugin.scala). The build file names it in the header and registers it with `use`. It compiles against `histos.api`. It does not see the engine. A command name the grammar owns is reserved. `acp` is reserved and has no command. [PluginSpec](api/test/PluginSpec.scala) locks that contract.

## Scenarios

A scenario checks a build by standing it up. One content store, one or more checkouts, sources written by the test, and an assertion on the result. That is how Histos is checked against itself: two copies of a project share a cache, a clean restores, and a skip that hides a change fails. The same shape is the Histos side when the other side is sbt and zipx. That comparison is the bench in [the cache plan](docs/cache-work-plan.md).

The decisions are [scenario-work-plan.md](docs/scenario-work-plan.md). The code is [Scenario.scala](engine/test/Scenario.scala), proved by [ScenarioSpec.scala](engine/test/ScenarioSpec.scala). A scenario is a test. It is not a command, and it is not a second language.

## License

Apache License 2.0. See [LICENSE](LICENSE).
