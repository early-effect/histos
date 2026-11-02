# Cache

## Why

This is the reason Histos exists. Do not trade it away for a smaller diff.

Compile, package, and link are expensive. sbt runs them again when it should not, and it sometimes skips them when the code changed. zipx made the sbt 2 cache usable in CI. That combination is the bar to clear, not the design to copy. Histos has to be more reliable than sbt, and faster than sbt plus zipx on the work a change should not repeat.

Russ's want, which every item below serves:

- He picks local or remote, and he picks how a result is pinned. The tool does not pick for him and then hide the choice in a restore-key prefix.
- His machine and CI share the same idea of a hit. A pull request warms itself. Main warms the next pull request. A label can drop a bad namespace. A pull request cannot poison main.
- The expensive work runs only when the code it depends on changed. A comment does not relink the graph. A public signature rebuilds the cone and leaves an unrelated module alone.
- Unchanged tests run only when he asks for the full run. `testQuick` is that ask. A remote pass record is opt-in, per module, and a suite that needs the network does not get one.
- The generated workflow caches the tools (Node, browsers, the dependency resolve) the way those tools are actually keyed, and a docs job does not wait on a product restore.
- None of this counts until a bench builds the same graph with sbt and zipx and with Histos and shows the win. The must is the incremental case: a no-op compiles nothing, a comment does not rebuild dependents, a signature change rebuilds only the cone. Cold may tie. Winning the tool-install time is the want and is not the gate. Mill is not in the comparison.
- A scenario is how that claim is checked inside this repository, and how Histos is checked against itself, before the other repository exists. One store, several checkouts, assertions on the feeds and the files. The bench is that shape with sbt and zipx on the other side. The decisions are in [scenario-work-plan.md](scenario-work-plan.md).

A task that makes a command faster while skipping a real change has failed. A task that copies zipx's epoch, or its one-blob snapshot, has failed. A task that checks a box before the proof is green has failed.

Histos caches compile, package, and link as content-addressed actions. A hit means the declared inputs and the toolchain identity matched. A miss names the input that changed. GitHub `actions/cache` stores tools. It does not store products.

This file is the decision. The checklist below is the work. Top to bottom. Finish one item and prove it before starting the next. The proof is the suite that item names, run with `./histos test` for the modules it touches. Tests bind loopback. They do not call Central, GitHub, or a keyserver. No item cuts a release.

sbt 2 plus zipx is the thing this has to beat, and the bench is where that is shown. Two habits of that setup stay out of Histos. An epoch plus a restore-key prefix, where the last prefix drops the epoch and can restore a stale snapshot. And one blob of `~/.sbt`, `~/.cache/sbt`, and Coursier under a branch-scoped quota, which cannot say that a single link is unchanged.

## Stores

Two stores, one key.

The blob store is global. A blob's name is its digest. Anyone may write. A bad blob has a different name, so an honest index entry never fetches it.

The action index is namespaced. The namespace is a prefix, not part of the digest.

- `main/` is what the next pull request reads.
- `pr/<n>/` is what later pushes of that pull request read and write.
- `dev/<name>` is a laptop that opted in. CI does not read it.

A pull request reads `pr/<n>/`, then `main/`, and writes `pr/<n>/` only. A push to `main` reads and writes `main/`. A laptop reads `main/` and does not write unless a `dev/<name>` namespace is set.

The index path is `/{namespace}/ac/{digest}`, where the namespace is `main`, `pr/<n>`, or `dev/<name>`. The blob path stays `/cas/{digest}`. A miss is a 404. A hit, a bad entry, or any other status stops the walk and does not try the next prefix.

The key is the declared inputs plus the toolchain identity: OS, arch, JDK, Scala, and the linker. The lock digest is an input. There is no epoch and no restore-key that drops an input.

`RemoteMode` stays `Off`, `Read`, and `Write`. The client speaks the Bazel HTTP API through heddle. An action-cache value is an `ActionResult` whose stdout digest names the blob: bazel-remote rejects any other body unless validation is turned off. The URI is configuration. gRPC can follow. It is not this work.

## What a compile publishes

A compile publishes two digests. Downstream tasks pick one. They do not key on "the task ran".

The API digest is Zinc's public-signature hash, written beside `inc.zip`. A downstream compile's key includes it. A comment or a method body does not change that key. A public signature does. A hit restores that compile and does not start the compiler. A miss still runs it.

The product digest is the class bytes after normalization: jar timestamps fixed, line-number tables stripped, the `TASTY` attribute dropped, no absolute path in the hashed bytes. `TASTY` is a hash of positions and comments, so a trailing comment changes those bytes while the bytecode stays the same. `.tasty` files are not hashed. Link and package key on the digest. A trailing comment that does not move member positions keeps it, so the link hits. A method body changes it, so the link runs.

[CompileKey](../engine/src/histos/zinc/JvmCompile.scala) hashes sources, flags, the JDK, jar checksums, and each upstream classes directory's API digest. It does not hash that directory's class bytes. [GraphCompile](../engine/src/histos/zinc/GraphCompile.scala) owns the analysis directory and reads `api.sha256` from it. [ContentStore](../engine/src/histos/cache/ContentStore.scala) is the local CAS.

`./histos clean` deletes the work tree and leaves the CAS. The next compile of an unchanged key restores the analysis and the class files and does not start the compiler. `cleanFull` stays the product wipe. `cache clean` stays the aged sweep. Neither deletes a remote index.

On a miss, the previous input map for that action is diffed and the changed input names are printed. No previous map prints `no prior key`. [CompileInputs](../engine/src/histos/zinc/CompileInputs.scala) stores those named rows under the analysis directory, which `clean` deletes and the CAS keeps. [CompileKey](../engine/src/histos/zinc/JvmCompile.scala) hashes the same rows. The `compiler` row is the jar fingerprints `CompileKey.withTools` adds. A hit prints nothing.

## Who writes main

`Promote` lives on the build. The default is `Rebuild`. The main build writes `main/` as a side effect of the work it already does. It does not copy the pull-request index, so a pull request cannot poison the next one. `CopyIndex` copies `pr/<n>/` into `main/` without recompiling. That trusts the pull request. The owner picks it. The copy reads `GET /{namespace}/ac`, one action digest per line, and writes those ActionResult bytes onto the destination index. It does not write a blob. `Rebuild` does not run it. bazel-remote does not implement that list. The loopback store does. A status other than 200 is a failure, not an empty index. The build file writes `promote"rebuild"` or `promote"copy-index"`. Omission is `Rebuild`. Any other token does not compile, so load fails.

Neither strategy is a `needs` of docs, deploy, or any job that does not compile. There is no snapshot restore in front of those jobs. A hit downloads that action's outputs. A miss compiles.

`purge` on a pull request deletes `pr/<n>/` and does not read it. It still reads `main/`. A pull-request label cannot delete `main/`. `purge` on a push to `main` deletes `main/` before that run writes it back. Purge deletes index entries. It does not delete a blob that another namespace still names. The request is `DELETE /{namespace}/ac` for the caller's own namespace. A blob is never in that request. bazel-remote does not implement this delete. The loopback store does.

## Tests

`test` runs the selected suites. Generated CI runs `test`, not `testQuick`.

`testQuick` is local. Its stamp includes the upstream API digest, so a signature change cannot skip the suite.

`TestStore` on the module is `Off` (the default), `Local`, or `Remote`. `Local` skips `test` when the stored suite digest still matches. `Remote` together with a `Need` fails at load. That is the same veto [Sendable](../engine/src/histos/schedule/Sendable.scala) already applies to network and docker. The remote URI is configuration, so `Remote` does not upload and does not skip. The bench modules stay `Off`. The bench does not need remote test results in order to win.

The existing CI planner already limits a pull request to the changed modules and their dependents. The action key is the finer cut inside that set.

## Workflow

[Workflow](../engine/src/histos/workflow/Workflow.scala) generates one test job until a split pays for a second tool restore. Tool caches use an exact key and no prefix fallback. `setup-node` does not set `cache: npm`. Playwright keys its directory and the browser directory by revision, OS, and arch. LLVM keys the pinned revision. Coursier keys the lock digest. A JDK the workflow fetches keys the locked revision.

The remote mode and the namespace come from the event. `CopyIndex` adds a sibling job on `main`. That job does not precede docs or deploy. The proof command is `test`.

## Bench

`early-effect/histos-bench` is a separate repository. One graph, two builds: `histos.scala`, and `build.sbt` on sbt 2 with zipx. No Mill. No Native.

[Scenarios](scenario-work-plan.md) are the building block under this bench. [Scenario](../engine/test/Scenario.scala) opens one store and any number of checkouts. The test writes the sources and asserts the compile feed, the test feed, the digests, and the compiler starts. Two checkouts are two instances of one project sharing a cache. That is Histos against itself, and it is the Histos column of the comparison below. sbt and zipx, the clock, and the published table stay in `early-effect/histos-bench`. A scenario does not shell `./histos`, does not copy this checkout, and does not run sbt. Dogfood of this repository stays `./histos test`.

The modules are `core`, `service` (`core`), `app` (`service`), one Scala.js module that depends on `core` and has a real link, one unrelated JVM module, and a Node or Firefox test so the tool cache is on the clock.

The local clock comes before Compose. Compose answers the remote cache. It does not answer compile speed.

sbt 2, Mill, and scala-cli compile with Zinc, the same compiler Histos drives. None of them make scalac faster. What they skip is process startup, and, when asked, the wait for an upstream module to finish its method bodies.

- sbt 2 keeps one JVM in server mode, so the compiler classloader stays warm between commands. Independent projects compile together inside that JVM. Zinc incremental analysis is on. Pipelining (`usePipelining`, `-Ypickle-write`) and `turbo` are off unless `-Dsbt.pipelining` or `-Dsbt.turbo` is set. zipx then caches the task outputs.
- Mill is the same idea with a resident server and Zinc workers. It is not a different compiler. It stays out of the table.
- scala-cli starts fast and keeps a compile server warm. It does not build this graph. It stays out of the table. Its lesson is the warm server, which the sbt column already records.
- Histos has Zinc incremental analysis, the API digest, CAS replay after `clean`, and a pool of `4 * cores` compilers. `IncOptions.of()` leaves pipelining off, which is sbt's default too. `./histos compile` starts a server when none is listening and leaves it up. Batch commands exec `dist/histos-client` when that binary is present. The generated test job runs `./histos client` before `./histos test` when the graph has that native module, so the suite uses the binary. `server`, `bsp`, and `mcp` stay on the jar. A chain still compiles one module after another, because a downstream cell waits for the upstream compile to finish.

A cold chain is scalac. A second command, and a one-file change, is where a resident sbt server can win. The local clock below measured a new Histos JVM against a warm sbt server. The resident compiler item measured a warm Histos server on the same graph. The first no-op there compiled nothing and still took about 1.25s, against sbt's 0.30s. That remainder was the fat client. Batch commands now go through `dist/histos-client`. The latest no-op on that graph is 0.10s, under sbt's 0.30s. Pipelining stays unbuilt.

The shared store is bazel-remote, with no hosted account. The image is `buchgr/bazel-remote-cache` at the multi-arch index the spec pins. The container is started with `--enable_ac_key_instance_mangling`. Without that flag the server stores the action cache by hash, so a write to `pr/1` is visible to `main`. Locally it is compose. On GitHub the same container listens on localhost, and only its data directory is saved with `actions/cache`, keyed by OS plus namespace. Histos and the sbt remote column both speak HTTP to that localhost. A hosted URI replaces the disk later without a client change. The Actions quota applies to that directory. A docs job never restores it.

Same runner class. The table is a median of three. A wrong skip fails the run.

These three have to beat both sbt columns (zipx `LocalDir`, and zipx pointed at the same HTTP cache):

- No-op. The same commit again. Histos compiles nothing.
- A trailing comment in `core`. `service`, `app`, and the Scala.js link are hits.
- A public signature change in `core`. The cone rebuilds. The unrelated module stays a hit.

Cold may tie. Tool restore is in the table and is not a gate.

A `workflow_dispatch` runs those three, cold, and tool warm, three times, and publishes the table. A pull request runs one no-op and one signature change.

## Checklist

### Digests

- [x] **API digest.** After a successful compile, write Zinc's public-signature hash beside `inc.zip`. One spec, three fixtures: a trailing comment keeps the digest, a new public method moves it, a method-body change does not. Proof: `./histos test engine` for that spec.

- [x] **Product digest.** Hash the class files after stripping line-number tables, dropping the `TASTY` attribute, and fixing jar timestamps. No absolute path is in the hashed bytes. `.tasty` files are not hashed. The same three fixtures: the comment keeps the digest, the body change moves it. The signature fixture records the digest it produces. A path leaf that contains `$` still hashes. Proof: `./histos testOnly --module engine 'histos.zinc.ZincSpec'`. The `$` leaf is in `./histos test engine` (535 passed, 1 ignored).

- [x] **Downstream compile key.** When the classpath entry is an upstream classes directory, [CompileKey](../engine/src/histos/zinc/JvmCompile.scala) mixes in that upstream's API digest instead of a hash of the class bytes. A comment in the upstream does not change the dependent key. A new public method does. The spec calls the key function. It does not need a second compiler run. Proof: `./histos testOnly --module engine 'histos.zinc.ZincSpec'`.

- [x] **Link and package key.** [JsKey](../engine/src/histos/js/JsKey.scala) mixes the product digest of each classes directory on the link classpath. [PackageKey](../engine/src/histos/pack/PackageKey.scala) mixes the product digest of each packed classes directory. A comment keeps the product digest, so it keeps these keys. A method body moves the product digest, so it moves them. The specs call the key functions. The linker and the publisher still run. Native is out of this item. Proof: `./histos testOnly --module engine 'histos.js.JsLinkSpec'`, `./histos testOnly --module engine 'histos.pack.JarSpec'`, and `./histos test engine`.

- [x] **Replay after clean.** The local CAS stores `inc.zip` and the class files under the compile key. `./histos clean` deletes the product tree and leaves the CAS. The next compile of that key restores both and does not start the compiler. The spec counts compiler starts. `cleanFull` and `cache clean` keep their current jobs. Proof: `./histos testOnly --module engine 'histos.zinc.CompileReplaySpec'` and `./histos test engine`.

- [x] **Miss names the input.** Store the previous input map for the action id. On a miss, print the names whose values changed. With no stored map, print `no prior key`. The spec asserts that line. Proof: `./histos testOnly --module engine 'histos.zinc.CompileMissSpec'`, `./histos test engine`, and `./histos test app`.

### Remote

- [x] **HTTP client.** A heddle client for `/ac/` and `/cas/`. A loopback server is the store. Put then get returns the same bytes. An index entry that names a missing blob is `RemoteError.Dangling`. `Off` and `Read` refuse `store`. Proof: `./histos testOnly --module engine 'histos.schedule.HttpCacheSpec'` and `./histos test engine` (505 passed, 1 ignored). No docker.

- [x] **Namespaces.** The prefix is on the index only. A write to `pr/1` is invisible to a read of `main`. A read of `pr/1` falls through to `main` on a miss. A pull-request writer cannot write `main`. Proof: the same loopback store. `./histos testOnly --module engine 'histos.schedule.HttpCacheSpec'` and `./histos test engine` (515 passed, 1 ignored).

- [x] **purge.** `purge` deletes the index entries of one named namespace and does not delete a blob another namespace still names. A pull-request invocation cannot name `main`. Proof: the remote spec. `./histos testOnly --module engine 'histos.schedule.HttpCacheSpec'` and `./histos test engine` (522 passed, 1 ignored).

- [x] **bazel-remote, when docker is present.** One spec starts the pinned container, writes as `pr/1`, reads as `main` and misses, writes as `main`, and then reads a hit. The spec is ignored when docker is absent, so the default suite stays green without a daemon. The image pin lives next to the spec. The run passes `--enable_ac_key_instance_mangling`. Proof: `./histos testOnly --module engine 'histos.schedule.BazelRemoteSpec'` (2 passed) and `./histos test engine` (524 passed, 1 ignored).

### Promote and workflow

- [x] **Promote on the build.** `Rebuild` is the default. `CopyIndex` is the other value. An unknown token fails at load. A one-module graph spec reads the default and the explicit value. Proof: `./histos testOnly --module api 'histos.GraphSpec'`, `./histos testOnly --module engine 'histos.load.PromoteSpec'`, `./histos test api` (116 passed), and `./histos test engine` (526 passed, 1 ignored).

- [x] **Copy the index.** `HttpCache.copy` copies index entries and does not compile. The copied entry names the blob the pull request stored. `Rebuild` does not run the copy. `./histos cache promote --from pr/<n> --to main` parses those namespaces. The remote URI is not configured, so that command reports that and does not open a connection. Proof: the loopback store. `./histos testOnly --module engine 'histos.schedule.HttpCacheSpec'` (34 passed), `./histos test engine` (533 passed, 1 ignored), and `./histos test app` (193 passed).

- [x] **Namespace from the event.** The test step sets `HISTOS_CACHE_NAMESPACE`. A pull request is `pr/<n>` (that prefix, then `main`). A push is `main`, because this file starts `push` only for `main`. Docs, deploy, versions, workflow, and release do not set it, do not `needs` a cache job, and do not restore a remote disk. This render does not emit that job: the remote URI is configuration, and the process does not read the variable yet. Proof: `./histos test engine` (535 passed, 1 ignored), `./histos test app` (193 passed), and `./histos workflow --check`.

- [x] **Tool keys.** The test job caches Playwright, LLVM, and Coursier with `actions/cache@v6` and no `restore-keys`. Playwright keys `~/.cache/chekhov/playwright/<revision>` and `~/.cache/ms-playwright` by revision, OS, and arch. LLVM keys `${{ runner.temp }}/llvm` by the pinned revision, OS, and arch, and passes `cache-hit` as `cached` so a hit does not download again. Coursier keys `~/.cache/coursier/v1` by `hashFiles('histos.scala')` while the build resolves, and by `hashFiles('dependencies.lock')` while it is locked, plus OS and arch. `setup-node` does not set `cache: npm`. This workflow does not fetch a JDK, so it emits no JDK key and no `setup-java`. Versions, docs, deploy, workflow, and release do not restore these and do not `needs` a cache job. This is not the remote-disk restore. Proof: `./histos testOnly --module engine 'histos.workflow.WorkflowSpec'` (22 passed), `./histos assembly`, and `./histos workflow --check`. This item is not a bench gate.

### Test results

- [x] **TestStore.** A module is `Off` unless it says `Local` or `Remote`. `Remote` together with a `Need` fails at load. `test` on `Off` launches the suite. `test` on `Local` skips when the stored suite digest matches. That digest is the one a passing run already stores. `test` on `Remote` launches and does not upload, because the remote URI is configuration. Bench modules stay `Off`. Proof: `./histos testOnly --module engine 'histos.testrun.ModuleTestQuickSpec'` (11 passed) and `./histos testOnly --module app 'histos.report.SchemaSpec'` (2 passed).

- [x] **testQuick stamp.** The quick stamp includes each upstream `api.sha256` that is on disk, as a sorted `module digest` line. A missing file adds no line. An unreadable file is `ModuleTestError.Api` and does not skip. A new public method on an unreferenced upstream class moves that digest and reruns the suite. A trailing comment keeps the digest and stays cached. `TestStore.Local` consults the same cover. Proof: `./histos testOnly --module engine 'histos.testrun.ModuleTestQuickSpec'` (12 passed) and `./histos testOnly --module engine 'histos.testrun.QuickSpec'` (20 passed).

### Bench

These items are the other repository. They start after the Histos items they call are checked. The Histos column of each gate is a scenario. The sbt and zipx columns, the clock, and the table stay here.

- [x] **The graph.** Create `early-effect/histos-bench` with `core`, `service`, `app`, a Scala.js module that depends on `core`, an unrelated JVM module, and a Node or Firefox test. `histos.scala` and `build.sbt` (sbt 2, zipx) name that same graph. No Mill. No Native. `core` builds JVM and JS. `web` depends on `core`, links `bench.Web`, and its test runs on Node. `idle` is the unrelated JVM module. Scala is 3.9.0, because Scala.js 1.22 publishes no 3.10 compiler. Proof: https://github.com/early-effect/histos-bench (`bee580a`). `./histos compile`, `./histos test`, and `./histos run web` passed. `sbt testFull` and `web/fastLinkJS` passed.

- [x] **Local clock.** On `early-effect/histos-bench`, pinned at `0.0.2-SNAPSHOT` (`0.0.2-20261101.164722-3`). Coursier stayed. Each sample wiped products and Zinc analysis with `cleanFull`. Three samples, the median, wall clock of the command. Cold `compile` includes process start: Histos 4.38s (4.39, 4.38, 4.36), sbt 4.73s (4.75, 4.73, 4.70). Both compiled all six cells. The no-op is a second `./histos compile`, a new JVM, at 2.45s (2.48, 2.40, 2.45), and a second `compile` in the same sbt server at 0.30s (0.33, 0.30, 0.30, cache 100%). A trailing comment in `core` recompiled only `core` JVM and `core` JS. `service`, `app`, `web`, and `idle` stayed hits. Histos 3.43s (3.43, 3.43, 3.44), sbt 0.43s (0.43, 0.41, 0.43). The core product digest did not move, on all three Histos samples, so the link key holds. A new public method on `Core` (`extra`) recompiled only `core`. `idle` stayed a hit. `service`, `web`, and `app` did not recompile their sources: nothing calls `extra`. Histos still missed those action keys (`apis`) and Zinc then compiled 0 files, and the core product digest moved, so a later link misses. sbt did not schedule the dependents. Histos 3.51s (3.47, 3.52, 3.51), sbt 0.42s (0.42, 0.44, 0.41). One warm `web/fastLinkJS` after a comment, and one after the new method, were cache 100%. Pipelining and a resident compiler stay unbuilt. This is not the remote column.

- [x] **Resident compiler.** The daemon keeps one Scala 3 pool and one Scala 2.13 pool. A compile checks a compiler out and checks it back in. `./histos compile` with no server starts one, attaches, and leaves it running. A second compile of the same cell opens no compiler (`./histos test engine`, 529 passed, 1 ignored, and `./histos test app`, 198 passed). The same four rows on `early-effect/histos-bench`, this assembly, median of three, against the sbt bars above. Cold `compile` starts the server: 4.96s (4.95, 4.96, 5.06), over the 4.73s bar. The no-op is a second compile on that server, all six cells skipped: 1.25s (1.24, 1.25, 1.25), over the 0.30s bar. A trailing comment recompiled only `core` JVM and `core` JS. The core product digest did not move. 1.90s (1.85, 1.90, 1.94), over the 0.43s bar. A new public method recompiled only `core`. `idle` stayed a hit. `service` and `web` saw `apis` and compiled 0 files. `app` stayed a hit. The core product digest moved. 1.90s (1.90, 1.91, 1.90). The warm server is not the remainder. The no-op still pays a new client JVM. A native client stays unbuilt. A later assembly keeps each compiler closed until a miss, remembers source lists, source digests, the lock, and classpaths, and drops that memo together with the compiler on `clean`, `cleanFull`, and reload. A deleted source is not a hit. Proof: `./histos test engine` (535 passed, 1 ignored) and `./histos test app` (198 passed). The same four rows, `java -jar` of that assembly, median of three. Cold 4.44s (4.44, 4.58, 4.30), under the 4.73s bar. No-op 0.54s (0.53, 0.95, 0.54), over the 0.30s bar. A trailing comment, its own three samples, recompiled only `core` JVM and `core` JS. The core product digest did not move. 0.98s (0.95, 0.98, 0.98), over the 0.43s bar. A new public method recompiled only `core`. `idle` stayed a hit. `service` and `web` saw `apis` and compiled 0 files. `app` stayed a hit. The core product digest moved. 0.97s (0.98, 0.97, 0.97), over the 0.42s bar. The no-op is still a new client JVM. A socket hit no longer builds the loader, resolve, the toolchain, or the compiler. Proof: `./histos test app` (202 passed). The same four rows, `java -jar` of that assembly, median of three. Cold 4.48s (4.48, 4.30, 4.62), under the 4.73s bar. No-op 0.51s (0.49, 0.51, 0.55), over the 0.30s bar. A trailing comment recompiled only `core` JVM and `core` JS. The core product digest did not move. 0.90s (0.90, 0.91, 0.89), over the 0.43s bar. A new public method recompiled only `core`. `idle` stayed a hit. `service`, `web`, and `app` saw `apis` and compiled 0 files. The core product digest moved. 0.89s (0.89, 0.89, 0.88), over the 0.42s bar. A repeat of that `java -jar` clock: cold 4.26s (4.68, 4.26, 4.21), under the 4.73s bar. No-op 0.48s (0.48, 0.47, 0.48), over the 0.30s bar. Signature 0.92s (0.91, 0.94, 0.92), over the 0.42s bar. The signature recompiled only `core`. `idle` stayed a hit. `service`, `web`, and `app` saw `apis` and compiled 0 files. The core product digest moved. A hit now forces the miss only when the socket is missing or dead, so describing that miss no longer initializes `HistosBuild` or `Machine`. Proof: `./histos test app` (202 passed). The same four rows, `java -jar` of that assembly, median of three. Cold 4.23s (4.32, 4.19, 4.23), under the 4.73s bar. No-op 0.47s (0.47, 0.47, 0.47), over the 0.30s bar. A trailing comment recompiled only `core` JVM and `core` JS. The core product digest did not move. 0.90s (0.88, 0.90, 0.90), over the 0.43s bar. A new public method recompiled only `core`. `idle` stayed a hit. `service`, `web`, and `app` saw `apis` and compiled 0 files. The core product digest moved. 0.93s (0.94, 0.92, 0.93), over the 0.42s bar. Batch commands exec `dist/histos-client`, and that client starts the JVM server from this assembly when the socket is missing. The same four rows, through that script, median of three. Cold 1.88s (1.86, 1.88, 1.88), under the 4.73s bar. No-op 0.10s (0.10, 0.10, 0.10), under the 0.30s bar. A trailing comment recompiled only `core` JVM and `core` JS. `service`, `app`, `web`, and `idle` stayed hits. The core product digest stayed `097bba7e979037635d84eca15b4025e9fba1418f718466c215f8fd02843bd355`. 0.23s (0.22, 0.23, 0.24), under the 0.43s bar. A new public method recompiled only `core`. `idle` stayed a hit. `service`, `web`, and `app` saw `apis` and compiled 0 files. The core product digest moved to `a035eaabc0e6749271825db2335d21892e55f08490d059bd4b195e0e153fc2c4` and the api digest to `a2a0e6b3b00df271f65937b805e8149a7a451bfcc4350e5ee1ba5b1149c17fdb`. 0.24s (0.24, 0.27, 0.24), under the 0.42s bar. Proof: `./histos test engine` (535 passed, 1 ignored) and `./histos test app` (203 passed).

- [ ] **Compose.** bazel-remote on localhost. Histos and the sbt remote column both use that URI. The image is pinned.

- [ ] **Three gates.** A scenario of this graph is the Histos column. The sbt and zipx columns run the same three changes. A wrong skip fails the run: the no-op compiles nothing, the comment does not relink `service`, `app`, or the Scala.js module, and the signature rebuilds that cone while the unrelated module stays a hit.

- [ ] **Both columns.** The sbt side runs as zipx `LocalDir` and as zipx against the same HTTP cache. Histos beats both on the three gates. Cold is recorded and may tie.

- [ ] **GitHub table.** `workflow_dispatch` runs the three gates, cold, and tool warm, three times, and publishes the median. A pull request runs one no-op and one signature change. The remote data directory is `actions/cache`, keyed by OS plus namespace, and only a job that compiles restores it.

- [ ] **Tool warm is recorded.** The second run's Node, browser, and Coursier times are in the table. They do not fail the bench.

## Out of scope

Native. Mill. scala-cli. A hosted cache account. gRPC. Caching `test` by default. A non-snapshot release. Histos installing a JDK or Node. The workflow restores those tools. Histos still refuses to install a toolchain.
