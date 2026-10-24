# Histos audit ledger

**Snapshot:** `main` at `7eeacb3`, 2026-10-22
**Scope:** `api`, `central`, `engine`, `app`, the launcher scripts, and `docs/`
**Owner of each row:** a step in [roadmap.md](roadmap.md)

Every row was found by reading the code. No build or test ran during the audit. Rows marked *verified* were re-read against the source after the first pass. Rows marked *miniature first* question a ZIO or JVM mechanism, so the owning PR starts with a small spec that proves the claim before it changes product code. Every other bug row starts with a failing test.

The PR that fixes a row checks its box in the same PR. A row is never dropped. If a row turns out to be wrong, the PR strikes it through and says why.

Paths are `<module>:<path under src/histos>`. Test paths are `<module>-test:<file>`. Line numbers are at `7eeacb3`, including those in `design.md` and `plan.md`, which M0 retired.

Category tags: `correctness`, `resources`, `concurrency`, `typed-errors`, `stringly`, `booleanly`, `totality`, `cast`, `services`, `duplication`, `reproducibility`, `security`, `performance`, `api-shape`, `tests`, `docs`.

## M0.1 Docs tell the truth

- [x] **D1** (docs) `docs/design.md` "Proposed Design" and "Commits" describe commits 1 to 9 as unbuilt. The code has them: `api:Builder.scala:20`, `engine:select/Selection.scala`, `app:bsp/Bsp.scala:978`, `api:ProjectDef.scala:103`, `app:server/Daemon.scala:94`, commit `6dd46d3`. `docs/plan.md:57` is the accurate one.
- [x] **D2** (docs) `design.md:27` says the repo has no GitHub Actions workflow. `.github/workflows/ci.yml` exists.
- [x] **D3** (docs) `design.md:22-23,87` say an aggregate still fails as `NotModule` and that MCP loads its own graph. Both are stale.
- [x] **D4** (docs) `design.md:191,215` say the server shares Zinc analysis and that the analysis file is the incrementality. Compile is never incremental (S8).
- [x] **D5** (docs) `design.md:207,484` say the test child sees `HISTOS_REPO` and `PATH` only. It also gets `HOME` and `XDG_CACHE_HOME` (`engine:testrun/ModuleTest.scala:1155-1157`).
- [x] **D6** (docs) `design.md:207` says `compiledTests` passes `ScalacFlags.empty` and that four helpers are private. It passes `module.flags` (`ModuleTest.scala:1333`), and the helpers are `private[histos]`.
- [x] **D7** (docs) `design.md:202` shows `.scalac(EncodingUtf8)`, which does not compile. The call is `ScalacFlag.EncodingUtf8`. The flag text is produced in `api:ScalacFlag.scala:21-23`, not in `JvmCompile`, which `plan.md:45` also gets wrong.
- [x] **D8** (docs) `design.md:228` says production `ScalacFlags` stay empty in BSP. `app:bsp/Bsp.scala:476` passes `flags.argv`.
- [x] **D9** (docs) `design.md:211` says `cache clean` drops JDKs and launchers. It walks `cas`, `plugins`, and old `tmp` files. B37 keeps JDKs and launchers out of `clean`: they are shared toolchains, and nothing records which project last used one.
- [x] **D10** (docs) `design.md:213` and `plan.md:9` describe a configurable, read-only-until-opened remote cache. No configuration path exists.
- [x] **D11** (docs) `design.md:329,374` say `BspSpec` covers the mixed compile status and `ServerSpec` covers reload and reboot. Neither test exists (A22).
- [x] **D12** (docs) `design.md:420,621` say `ci.yml` is `test` plus `workflow` with an empty catalog. It has a `versions` job and tool steps, and `design.md:256` already says the catalog is non-empty.
- [x] **D13** (docs) `design.md:69,482` say the launcher refuses a mismatched sha256. The snapshot path skips the check (`histos:80-84`). M0.1 states the current behavior, and L7 fixes the script.
- [x] **D14** (docs) `design.md:69` says the engine re-execs. It starts a child and waits (`engine:jdk/Toolchain.scala:206-210`).
- [x] **D15** (docs) `design.md:217` says Node follows the JDK rules. Node selection was display only. B22 now launches that Node for Js test and run.
- [x] **D16** (docs) `design.md:484` says publish takes no password. `engine:publish/PublishWhere.scala:37-41` reads `SONATYPE_PASSWORD`. `design.md:118` calls `central` a local Maven publisher, but it also uploads snapshots.
- [x] **D17** (docs) `plugins.md` cites retired design section numbers (5.3, 5.7, 5.9, 5.10, 5.11) throughout.
- [x] **D18** (docs) `plugins.md` uses stale names: `PluginIdMacro`, `ScannedBuild.source: Path` (it is a `String`), `closeLoader`, `Loader.classpath`, `PluginRuntime.run` and `bind` signatures, `Policy.Plugin` arity, `CommandSpec.summary: String` (it is `PluginDetail`), and "`LoaderLive.compile` passes the on-disk path".
- [x] **D19** (docs) `plugins.md` examples do not compile: `register` with `try`/`catch` and an `Either`-returning `fromClass` (`:327`), and `ProjectName("docs")`, which has no literal (`:811`).
- [x] **D20** (docs) `plugins.md:448-453` lists the compile classpath without `zio-stacktracer` and `izumi-reflect` (`engine:load/Boundary.scala:39-47`).
- [x] **D21** (docs) `design.md` narrates code line by line, with file names and "do not" warnings. That style rots: this audit found more than 20 stale claims. It is replaced by an as-built `architecture.md` and this roadmap.

## M0.2 Tests assert typed values

- [x] **T1** (tests) `api-test:GraphSpec.scala:12-13,205-484` and `VersionSpec.scala:189-279` match errors with `.message.contains`. 29 of the 30 graph `BuildError` cases are never matched as values. `AggregateChild`, `JsEnvWithoutJs`, `EmptyShipName`, and `BadShipName` have no test.
- [x] **T2** (tests) Fixtures fail by comparing made-up strings: `GraphSpec.scala:349`, `Outside.scala:21`, `VersionSpec.scala:48`, `central-test:PublisherSpec.scala:53`.
- [x] **T3** (typed-errors) Tests flatten typed errors with `IllegalStateException(e.message)` or `.left.map(_.message)`: `PublisherSpec`, `SnapshotSpec`, `ModuleTestSpec` (25 sites), `GraphCompileSpec` (18), `PublishSpec` (7), `LauncherSpec` (5), `QuickSpec`, `PluginRunSpec:380,465`, `ProgramSpec` (7), `ModuleTestSpec:200-203`.
- [x] **T4** (tests) Shape-only assertions: `GraphSpec.scala:332` (`isRight`), `GraphCompileSpec.scala:248`, `PluginRunSpec.scala:505` (`isLeft`), `case _ => false` in `JdkSpec`, and 18-field positional `Module` patterns (`api-test:Outside.scala:15`).
- [x] **T5** (totality) `engine-test:SelectionSpec.scala:27` uses `.head`.

## M0.3 Test environment as aspects

- [x] **T6** (tests) Tests that need the network, clang, Firefox, or npm run ungated: `NativeSpec.scala:143-188`, `ModuleTestSpec.scala:213-218`, `ProgramSpec.scala:120-124`.
- [x] **T7** (tests) Tests read `sys.env`: `LoadSpec.scala:25`, `SpecularFixtureSpec.scala:326`, `ProgramSpec.scala:147`. The same reads in `NativeSpec`, `WorkflowSpec`, `TestProcessSpec`, `BspSpec`, and `AssemblySpec` now go through `zio.System` with `withLiveSystem`, and the two dead `rooted` helpers are deleted. `BootstrapSpec` moves to T13.
- [x] **T8** (tests) `NativeSpec.scala:55` fallback is a tautology, `:144-154` passes on failure, and `:134-140` proves only the classpath.
- [x] **T9** (hygiene) About 13 untracked `*.scala.semanticdb` files sit in `engine/src`, including `jdk/`. Find the writer. All 13 date from 2026-10-17, before scala-cli left the repo, and a full `./histos test` writes none, so the writer was that setup. They are deleted.

## M0.4 Drop ACP

- [x] **A11** (typed-errors) ACP returns both success and failure as `result.value`, invalid params succeed, a parse error is -32601, JSON is hand-built, and it is not the Agent Client Protocol (`session/new`, `session/prompt` are absent). Closed by deleting `app:acp/` (decision 4) in `histos/drop-acp`. `acp` stays in `CommandName.reserved`, and `histos acp` is `CliParseError.UnknownCommand` without loading the build.

## M1.1 Publish one coordinate per platform

- [x] **B1** (correctness, *verified*) `engine:publish/LocalPublish.scala:137-145,300-308,432-440` compiles every platform, and `jobs` filters cells by module only. A Jvm+Js module gets two parallel writers on one `jvm.jar` under one `_3` coordinate.

## M1.2 Server lifecycle

- [x] **B2** (correctness, *verified*) `app:cli/Cli.scala:425-441,642-657`: `compile` and `test` call `Loader.load` before `attach`. A live server still gets a client-side load, a broken `histos.scala` fails in the client, and `~ compile` loads every iteration.
- [x] **B3** (concurrency) `app:server/Daemon.scala:368-383,400-451`: `run` is `forkScoped` and unobserved. A failure after bind leaves `listening` incomplete, so `histos server` hangs.
- [x] **B4** (resources) `Daemon.scala:805-830`: reboot closes the channel, but `serve` waits on `ZIO.never`, so the old JVM never exits. The order differs from the design, and no test covers it.
- [x] **B5** (concurrency, *miniature first*) `app:bsp/Frame.scala:27-28` and `Daemon.scala:316-328,566-570,863-867` read sockets and stdin with `attemptBlocking`, which interrupt does not reach. A miniature showed that interrupting `attemptBlocking` on a socket stream waits for the read, `attemptBlockingInterrupt` frees a socket read but not a pipe, and `attemptBlockingCancelable` with a close frees both. `Frame.read` closes its stream on interrupt.
- [x] **B6** (resources) `Daemon.scala:402-413` registers the socket `remove` finalizer after `Record.write`.
- [x] **B7** (correctness) `Daemon.scala:863-882`: BSP requests never check the fingerprint, so a Metals-only session never reloads or sends `buildTarget/didChange`.
- [x] **B8** (concurrency) `Daemon.scala:421,883-885`: `Ref[Option[Bsp.Session]]` lets a second splice overwrite the first, and the first exit clears both.
- [x] **B9** (dead code) `Decision.Stale`, `Drift`, `ServerError.Stale`, and `Message.Stale` are never produced and are still matched (`Cli.scala:995,1150,1164`).

## M1.3 Plugin isolation

- [x] **B10** (concurrency, *miniature first*) `engine:load/PluginRuntime.scala:130-134,253-261` and `Loader.scala:243-250` set the thread context class loader on the current thread and then run plugin code that may move the fiber to another thread. Parallel plugin nodes overwrite each other. A blocking step in `run` landed on a default blocking thread whose context loader was the app loader, so `loadClass("dev.example.Echo$")` failed with `ClassNotFoundException`, and `graph` and `run` now execute on `PluginThreads`, whose threads carry `BuildLoader`.
- [x] **B11** (correctness) `engine:load/BuildLoader.scala:28-31`: `takeRefused` never clears, so one refused `histos.*` lookup fails every later run on that loader with `EngineLeak`. An unrelated task run after a refused lookup failed with `EngineLeak("histos.schedule.Sendable")`, and `takeRefused` now clears.
- [x] **B12** (typed-errors) `Loader.scala:220,263,283-293` reads `MODULE$`, which runs the plugin's static initializer outside `attempt`. A throw is a defect, not `PluginLoadError.Defect`. The load died with `ExceptionInInitializerError`, and it is now `Defect(coordinate, "java.lang.ExceptionInInitializerError")`.
- [x] **B13** (typed-errors) `Loader.scala:236-241,269-281` blames `matched.head` for leaks and defects, not the plugin whose class failed. A leak from `tools/b`'s `contribute` named `tools/a`, and `BuildLoader` now records the requesting class, with the new `PluginLoadError.BuildLeak` when no plugin class asked.
- [x] **B14** (reproducibility) `engine:load/PluginInputs.scala:173,176` puts the absolute Coursier path of each plugin jar in the digest, so no remote hit can happen on another machine. The same jar bytes at two paths stored two digests, and each jar input is now named by its content hash, with exact duplicates dropped.
- [x] **B38** (correctness) `LoaderLive.reportBuild`, which BSP uses for build-file diagnostics, compiles the raw `histos.scala` with the header lines still in and no plugin jars, so diagnostics fail for any build with a plugin header. Found while correcting `plugins.md`. A build with `plugin(path"tools/docs")` reported `Illegal start of toplevel definition` and `Not found: dev`, and it now reports no problems.
- [x] **B39** (correctness, *miniature first*) `izumi.reflect.*` is not a boundary name, and the izumi jar is dropped from plugin classpaths, so plugin code that materializes a `Tag` may fail with `PluginRunError.Linkage`. Found while correcting `plugins.md`. A `LoadSpec` plugin that summons `Tag[Greeting]` died with `NoClassDefFoundError: izumi/reflect/Tag$` (`izumi.reflect.Tag$`), a die rather than `Linkage` because `run` threw while building its effect, before `catchSomeDefect` applies, and `izumi.reflect.` is now a boundary name.
- [x] **B41** (typed-errors) `engine:load/PluginRuntime.scala` `invoke`: a plugin whose `run` throws before it returns an effect escaped `catchSomeDefect` and the run died with the raw throwable (`java.lang.IllegalStateException`), and `ZIO.suspendSucceed(plugin.run(request))` now turns that throw into `PluginRunError.Defect`. Found while proving B39.

## M1.4 Child processes

- [x] **B16** (resources) `engine:testrun/TestProcess.scala:126-127,144-145`: `start` then `addFinalizer` is not atomic, so an interrupt between them leaks the child. The gap is a race no deterministic test can hit, and `start` is now the acquire of `ZIO.acquireRelease`, which is uninterruptible.
- [x] **B17** (resources) `TestProcess.scala:266-279,316-320`: non-JVM children (node, native binaries) never get their own process group, and `stop` skips the group kill once the leader exited. The `sleep` started by `sh -c` and the `sleep` a JVM test left behind after its main returned both survived. This branch kills every descendant of a live child, taken with `ProcessHandle.descendants()` before anything dies, and a JVM child's whole group even after its leader exited (`process/ProcessTree.scala`). B42 holds the rest: a descendant that outlives a node or native child.
- [x] **B18** (concurrency) `TestProcess.scala:180-199` starts a new `Thread` per line read, outside ZIO supervision. Each pipe is now `ZStream.fromInputStream` read by one fiber, in a scope that closes after the tree is killed, because a blocked pipe read ignores interrupt.
- [x] **B19** (performance) `ModuleTest.scala:69-70,1070` and `TestProcess.scala:164` build output with `s"$have$line\n"` (quadratic), and `TestStatus` keeps a second copy of streamed output. 200,000 lines took 13.1 s through `TestProcess.run` and now take about 0.4 s. `TestStatus` carries no output, and `Transcript` (one `Chunk` per stream) is the one accumulator, for `TestProcess.run` and `ModuleTest.run`.
- [x] **B20** (resources) `engine:jdk/Toolchain.scala:206-210`: the JDK hand-off is a raw `ProcessBuilder` plus `waitFor`, and the child survives an interrupt. An interrupted hand-off left the child and the `sleep` it started running, and the child is now an `acquireRelease` resource whose release kills its tree.
- [x] **B21** (resources) `engine:pack/Assembly.scala:268-270` opens a `URLClassLoader` over every jar and never closes it. The loader still served `demo/Lib.class` after its scope closed, and it is now closed by `acquireRelease`.
- [x] **B44** (resources) `engine:jdk/Toolchain.scala` `replace`: an interrupted hand-off force-killed the child before its TERM handler could run (a child that traps TERM never exited 130), and the release now sends SIGTERM to the tree, waits up to 2 seconds, then force-kills what is left.
- [x] **B45** (resources) `engine:testrun/TestProcess.scala` `bootstrap`: an interrupt waited out the `javac` compile and left `javac` running (it did not finish within 3 seconds against a `javac` that sleeps), and `javac` is now an `acquireRelease` child whose release kills its tree.

## M1.5 Toolchain decisions reach execution

- [x] **B22** (correctness, *verified*) `ModuleTest.scala:461`, `js/JsSuite.scala:106`, and `js/JsRun.scala:128` run `node` from `PATH`. The Node lock only feeds `histos toolchain` (`toolchain/Survey.scala:34`). A named Js main ran the parent's `node` and never launched the `node.local` choice, and Js test and run now launch the Node the selection chooses, once per command, or fail with `NoNode`.
- [x] **B23** (correctness) `native/NativeCompile.scala:21` ignores `HISTOS_NATIVE` at compile, `ModuleTest.scala:1154-1158` drops it for test links, and `native/NativeLink.scala:191` builds the linker env from `sys.env`. With `HISTOS_NATIVE` naming an empty local repository, compile, the test compile, and the link all fetched from Central, and the origin is now read once from `Machine` and passed to compile, the test compile, link, and runner, and the run link, with the linker env built from the machine's.
- [x] **B24** (correctness, *verified*) `jdk/JdkStage.scala:126-136,143-166` stages zip only. Temurin ships tar.gz on Linux and macOS, and unzip loses exec bits. `JdkSpec:301-316` checks only `isRegularFile`. A tar.gz JDK was refused as `NotZip`, and the system `tar` now unpacks it as a scoped child, so `bin/java` keeps its exec bit and runs.
- [x] **B25** (correctness) `jdk/Jdk.scala:163` and `Toolchain.scala:112,118` count Coursier as installed when its cache directory exists. Histos's own `FileCache()` creates that directory. A cache without a JDK and no `cs` on `PATH` turned provisioning into a `cs java-home` hint, and that cache now counts only when it holds a JDK.
- [x] **B26** (resources) `JdkStage.scala:79-85,138-166` and `node/NodeStage.scala:91-98,154-181` use one fixed `archive.partial` name and unpack in place, so concurrent or interrupted stages corrupt the cache. A failed unpack left a partial `unpack` beside `archive`, and both stagers now copy to a per-attempt scratch file and unpack into a scratch directory under a scope, then move into place atomically and keep a complete unpack another stager placed first.
- [x] **B27** (correctness) `node/NodeProbe.scala:194-196`: the read-only probe runs `setExecutable(true)` on user binaries. Probing made an `rw-r--r--` node executable and chose it, and the probe now leaves modes alone and rejects that node as `NotExecutable`.
- [x] **B47** (correctness) `node/NodeStage.scala` stages zip only, but nodejs.org ships tar.gz and tar.xz for Linux and macOS. Found in M1.5. A tar.gz node was refused as `NotZip`, and tar.gz and tar.xz now unpack through the same scoped system `tar` as B24, with `bin/node` executable.
- [x] **B48** (resources) `node/NodeProbe.scala` `versionOf` starts `node --version` with a raw `ProcessBuilder` and a `destroy` in `finally`. Found in M1.5. Interrupting a probe of a node whose `sleep` held stdout did not finish within 5 seconds, and the probe is now a scoped child whose release, and any interrupt, kills its tree.

## M1.6 Snapshot publisher

- [x] **B28** (resources) `central:central/Snapshots.scala:97,230-232` creates an `HttpClient` in a constructor and never closes it. The layer is `ZLayer.succeed`. The publisher now takes heddle's `Client` from `Client.layer`, which is scoped and closes its pool. Heddle tests that lifetime. Histos does not.
- [x] **B29** (concurrency) `Snapshots.scala:208-210` sends with `attemptBlocking`, so a cancel waits out each PUT. Uploads go through `Client.batched`. Heddle's socket reads are interruptible, and Histos does not retest the client.
- [x] **B30** (security) `Snapshots.scala:165-168` accepts `http://` with Basic auth, and a bad URL surfaces only at publish time. `SnapshotRepository.from` accepts `https`, or `http` on loopback, and a bad url is `SnapshotRepository.Error` before a publisher exists.
- [x] **B31** (security, stringly) `Snapshots.scala:91-96`, `Publisher.scala:151`, and `engine:publish/PublishWhere.scala:37-41`: URL, user, and password are `String`s read from the environment directly. A `toString` override hides the password. The password is a `Config.Secret`, the user is a `RepositoryUser`, and neither the secret nor `PublishWhere.Snapshots` renders the password.
- [x] **B32** (tests) `Snapshots.scala:111,151` reads a `java.time.Clock` inside a pure binding, so `TestClock` cannot drive it. The unique version is stamped with `ZIO.clockWith(_.instant)`, and `TestClock.setTime` fixes the build in `SnapshotSpec`.
- [x] **B33** (api-shape) `Snapshots.scala:126-131` and `Publisher.scala:68`: `Published.jar` is a fake relative `Path` for a remote publish, and `jarSha256` is a `String`. `Published.Local` carries the files and `Published.Snapshot` carries the unique version. The digest is a `Checksum`.
- [x] **B34** (typed-errors) `Snapshots.scala:210` and `Publisher.scala:48-50`: `Transport(getMessage)`, `Status(detail)`, and `Metadata(detail: String)` carry raw text. `TransportFault` names the target, `Status` keeps the code, and `MetadataFault` names why the build number could not advance. A trust failure and an HTTP 500 both drop the body text.
- [x] **B40** (typed-errors) `ArtifactId.from`, `DependencyVersion.from`, and `SnapshotMetadata.base` fail with the whole `PublishError`, so parse errors share an enum with HTTP failures, and a malformed base URL is `PublishError.Transport`. `SnapshotMetadata.render` takes 7 positional parameters. Found in M0.2. The parsers return `ArtifactIdError`, `DependencyVersionError`, and `SnapshotRevisionError`. A bad repository url is `SnapshotRepository.Error`. `render` takes a `SnapshotDocument`.

## M1.7 Cache hygiene

- [x] **B35** (resources) `engine:cache/ContentStore.scala:46-51` leaks the temp file on failure or interrupt, and `clean` never sweeps `tmp/`. `put` releases the temp on failure and interrupt. `clean` deletes temp files older than the retention and leaves a fresh one.
- [x] **B36** (correctness) `ContentStore.scala:67-81`: the keep file only grows, is global across projects, and is read-modify-write with no lock. Each project has its own file, a load replaces that file, and the write holds a lock. A legacy `keep` file is still honored.
- [x] **B37** (correctness) `cache clean` does not cover JDKs or launchers. Either clean them or change the documented contract (D9). The contract is that `clean` does not remove them.

## M1.8 Exit codes

- [x] **B46** (correctness) `histos compile` exits 0 when a cell fails to compile: `Cli.compileLocal` fails only on a stream error, and a failed cell is a `CompileFeed.Failed` event. `GraphCompile.strict` fails the compile command's stream after its last event, locally and through the server. Test runs keep the lenient stream so passing cells still test.

## M2 Steps, executor, and aspects

- [ ] **K1** (services) Compile cells and plugin tasks run on `Histos`. Test (`testrun/ModuleTest`), publish (`publish/LocalPublish`), and run (`program/Program`) are still hand-wired pipelines.
- [ ] **K2** (api-shape) Scheduler outputs are `String`, dependents never receive upstream values, and dispatch is `if action.kind == ...` (`PluginRuntime.scala:104`).
- [x] **K3** (concurrency) `schedule/Scheduler.scala:128,144`: parallelism is unbounded, and `foreachParDiscard` fails fast, which contradicts design Key Decision 9. `Histos` takes one permit per resource class, and a failed side of a zip does not cancel the other side. `HistosSpec` holds a cpu limit of one and a dependent that does not run.
- [ ] **K4** (services) `Scheduler.scala:146-214` hardcodes four aspects inline: local cache, manifest check, remote lookup, and upload by verdict.
- [x] **K5** (concurrency, *verified*) `GraphCompile.scala:459-486`: compile folded over topological layers, so a cell waited for its whole previous layer. A cell is now a step whose upstreams are the cells it depends on. `GraphCompileSpec` holds one cell and sees an unrelated cell start.
- [x] **K6** (concurrency, *verified*) `GraphCompile.scala:330-334,404-446`: a `LinkedBlockingQueue` and `AtomicBoolean` side channel, side effects inside `ZIO.succeed`, `attemptBlockingInterrupt(take).orDie`, and `closeAll` twice. Feeds go through the stream the compile is already producing.
- [x] **K7** (api-shape) `GraphCompile.scala:448,488,552` took 9, 10, and 12 parameters. `CompilePlan` and `CellRun` carry them.
- [x] **K8** (dead code) `CompileKey` had only `ZincSpec` as a caller, and nothing called `GraphCompile.runCells`. The compile path keeps the key, and `runCells` is gone.
- [x] **K9** (performance) `JvmCompile.scala:256,288-294,338`: `hashSources(..) *> invoke` read every source into one `Chunk` and discarded the digest. The digest is `CompileOutput.key`, and a compiled cell keeps it.
- [x] **K10** (correctness) `PluginRuntime.scala:104,196-205`: compile dependencies were stub actions that returned `"compiled"` with no inputs. A `TaskDep.Compile` is now a compile step, and a plugin failure from that step is `PluginFault.Compiled`.
- [x] **K11** (performance) `PluginRuntime.scala:123,169-177` planned and hashed every task of every plugin on each run. The step graph is only the task being run and the tasks it depends on.
- [ ] **K12** (api-shape) `task/Task.scala:53,56,70`: `TaskResult.text` is a `String`, the doc says a cancelled run dies (it is interrupted), and the registry finalizer does nothing.
- [ ] **K13** (api-shape) `TaskKind` is closed, and nothing in main code uses `ZIOAspect` or `@@`. There is no decoration seam.
- [ ] **K14** (minor) `Scheduler.scala:180,185` computes the verdict twice, `:25` `Failed.message` drops the inner error, `Action.scala:91` has `getOrElse(id, "")`, `:112` has a no-op `put` default, and `RemoteCache.scala:48-75` keeps test helpers in main code.
- [ ] **K15** (tests) `SchedulerSpec` lacks: a failure stops only its dependents, `DuplicateAction`, `MissingDependency`, `DuplicateInput`, the context loader under parallel plugins, the sticky refusal, and temp cleanup on interrupt.
- [ ] **K16** (typed-errors) A plugin compile failure is `PluginFault.Compiled`. `load/PluginCells.scala` still flattens `ResolveError`, `LockError`, and `ModuleTestError` into `PluginRunError.Failed(PluginDetail(text))`.
- [ ] **K17** (api-shape) `GraphCompileError.Cells(first, rest)` is a hand-rolled non-empty list where `NonEmptyChunk` fits. Found in M0.2.
- [ ] **K18** (correctness) `PluginRuntime.bind` drops a task whose `TaskId.from` fails (`.toOption.toList`) without reporting it. Found in M0.2.

## M3 Services that own resources

- [x] **S1** (services) `Machine.load` reads the fiber's `zio.System` once. `HISTOS_NATIVE` and `HISTOS_JDK_SETTLED` are `zio.Config` over that map. Action stamps and `cache clean` use the fiber clock. Plugin env inputs use the captured map. `ApiJar` reads `HISTOS_REPO` through `zio.System`. `ModuleTest.suiteEnv` and `NativeLink.linkerEnv` already took a map. Generated bridge sources still call `sys.env` inside the child (P1). `BootstrapSpec` still reads `sys.env` (T13).
- [ ] **S2** (typed-errors) A failed dependency-lock read is `ResolveError.Io(IoFailure)`. Compile and load filesystem failures are `IoFailure` too: the message names the path (`could not read …`), and those reads and writes run on the blocking pool. Still open: `ContentStore`, `CoursierResolve`, `ChildProcess`, `Publisher`, and the JDK and Node stages, which still drop the path.
- [ ] **S3** (duplication) The dependency lock has one reader, `Resolve.readLock`, and one tree delete, `Workspace.delete`. Still open: two `Driver` invocations, two reserved-package checks, nine `write`/`empty`/`fresh` helpers, `platformToken` in `ModuleTest`, four `nativeVersion.getOrElse` calls, and test helpers that still walk a temp directory themselves.
- [ ] **S4** (concurrency) Blocking file IO and dotty compiles run under `ZIO.attempt` on the default pool: `ContentStore`, `Loader`, `PluginCompile`, `PluginInputs`, `PluginRuntime`, `PluginManifest`, 39 sites in `jdk` and `node`, `zinc/Sources.scala:75`, `JvmCompile.scala:268,290`, `assembly/Assemble.scala:157`.
- [x] **S5** (services) `Loader` stays a trait because a reload test supplies another implementation. The live value is a class that takes `Resolve` and `ContentStore`. `Scheduler` is a case class of the executor and the two caches. A loaded plugin runtime builds `Histos` once, on the load scope, and each task builds its `Scheduler` from that executor. `Daemon` and `Session` extend the caller's scope instead of providing one as a layer.
- [x] **S6** (services) The resolve model, lock tokens, and `ResolveError` sit beside the resolve use cases. The build-file compiler is `BuildFile`. Publish types and the maven layout sit beside `Publisher`. Test reports, suite launch, and test compile are separate from `ModuleTest`. The use cases still return the text the command prints (K2).
- [x] **S7** (services) `PluginRequest` lives in `histos.resolve`. `requirePlugins` returns `ResolveError`. `resolve` does not import `load`.
- [x] **S8** (performance, *verified*) Zinc analysis is `inc.zip` under `.histos/analysis`. The next compile passes that previous result. An unchanged class keeps its timestamp, a changed neighbor is rebuilt, and a removed source deletes its class. A compiler is opened once per toolchain and reused. Stale class files from a deleted source used to survive a clean checkout's opposite: a dirty `.histos/classes` could still hold them, and this compile now removes the ones Zinc recorded.
- [x] **S9** (performance) The provider runner source does not change with the suite pattern. One install is shared by the cells that compile against the same classpath, and one cell writes it. The child receives the framework, the class directory, and the patterns in its own environment.
- [x] **S10** (services) `ChildProcess` in `histos.process` is the scoped child. Test, link, run, and the JDK hand-off use it.
- [x] **S11** (resources) `ApiJar.locate` packs into `Workspace.work`, removed with the caller's scope. The loader's class directory and blanked build file register their finalizer on the load scope.
- [x] **S12** (api-shape) Wide calls take a request: `LinkJob`, `CellRun`, `JsLinkRequest`, `JsRunRequest`, `JsSuiteRequest`, `NativeLinkRequest`, `NativeSuiteRequest`, and `TestRun`. `Scheduler.publish` left with the executor. `PluginInputs.hash` takes `HashRequest`.
- [x] **S13** (typed-errors) `EntryPoints.mains` fails with `IoFailure` for the directory it walked. `SourceWatch.take` reports the root whose walk failed.
- [x] **S14** (services) A plugin run reads `Machine.native` before it builds steps and passes that origin into `PluginCells.compile` and the test compile. Unset is Central. A refusal still fails only a Native fetch.
- [x] **T12** (duplication) The ten specs call `GraphCompile.jdkHere`. There is no `21.0.2` fallback for the running JDK. `JdkSpec` still builds version fixtures with `JdkVersion.trusted` for lock cases. `AssemblySpec` still reads `java.home` for a java binary. Found in M0.3.
- [x] **B42** (resources) `ChildProcess.command` starts `subprocess.Exec`, which calls `setpgid(0, 0)` and then `execvp`. The grandchild's group id is its parent's pid, so a kill of that group reaches it after the parent has exited. Found in M1.4.
- [x] **B43** (resources) `stop` kills that group before it closes the pipes. The writer that used to sit outside the tree is in the group, so its end of the pipe closes and a blocked read returns. Found in M1.4.

## M4 The lock covers every jar

- [ ] **L1** (reproducibility, security, *verified*) A raw `coursier.Fetch(FileCache())` bypasses the build's resolvers, the lock, and checksums: `zinc/Scala3.scala:65-74`, `zinc/Scala213.scala:92-101`, `JvmCompile.scala:192-207` (semanticdb), `assembly/Assemble.scala:111-127`, `js/JsRun.scala:65`, `js/JsSuite.scala:283-304`, `js/JsLink.scala:123-142`, `js/ScalaJs.scala:35-67`, `native/NativeSuite.scala:182-201`, `native/NativeLibs.scala:81-149`.
- [ ] **B15** (reproducibility) `"3.10.0"` is hardcoded three times (`PluginCompile.scala:23,83`, `Loader.scala:198`, `resolve/Resolve.scala:494`) and never read from the running compiler.
- [ ] **L2** (reproducibility) Tool versions are literals in engine code: Scala.js 1.22.0, JS envs 1.6.0 and 1.1.1, chekhov 0.1.3 (twice), bridge Scala 2.13.18, semanticdb 4.17.3 (`JvmCompile.scala:186,234`), Native pinned to `JvmCompile.Scala390` (`NativeCompile.scala:17`), and `testrun/SuiteNames.scala:104`.
- [ ] **L3** (duplication) A second artifact-suffix scheme is hand-written in `JsSuite.scala:82-96`, `JsLink.scala:125`, `ScalaJs.scala:37-42`, and `NativeSuite.scala:184`. `resolve/ArtifactName.scala:13-20` duplicates its own binary branch.
- [ ] **L4** (duplication) `jdk/JdkLock.scala` and `node/NodeLock.scala` are the same parser, and the launcher's awk at `histos:118` is a third.
- [ ] **L5** (duplication) `JdkStage` and `NodeStage` are about 180 identical lines each, and `JdkStage.unpackEntries` and `hash` use `var`, `null` checks, `while` loops, and `try`/`catch`.
- [ ] **L6** (duplication) `Jdk` and `Node` repeat the resolution fold, the location scans, and the override and lock reads.
- [ ] **L7** (security) `histos:80-84` skips the pin on the snapshot path, `:128-140` downloads and unpacks non-atomically, and `histos.bat` has no snapshot path, uses fixed `%TEMP%` names, and re-untars every time.
- [ ] **L8** (correctness) `Toolchain.scala:187-188`: the engine refuses https locks that the script itself fetches.
- [ ] **T13** (tests) `BootstrapSpec` builds and runs the launcher with plain-Scala file and process side effects outside ZIO, and reads `sys.env`. The launcher rewrite converts it. Found in M0.3.

## M5 Platforms as backends

- [ ] **P1** (stringly) About 450 lines of Scala live as strings filled in with `replace("__ENV__")`: `JsSuite.scala:111-279`, `JsLink.scala:68-121`, `JsRun.scala:87-131`, `NativeSuite.scala:67-180`, `native/NativeBridge.scala:9-49`. They pass positional `true, false` to Chekhov and use `Await.result`, the global execution context, `System.exit`, `null`, `var`, and `sys.env("JS_OUT")`. The build never typechecks them.
- [ ] **P2** (performance) `fresh(linkDir)` wipes the bridge classes, their analysis, and the Scala Native link cache on every run (`Program.scala:104,116`, `ModuleTest.scala:450,493,648`, `NativeLink.scala:183`).
- [ ] **P3** (duplication) `ModuleTest.scala:413-992` has six near-copies (`runJs`, `runJsProvider`, `runNative`, `runNativeProvider`, `execute`, `provide`) with 10 or 11 parameters. The platform dispatch is repeated in `GraphCompile.scala:635-647` and `ModuleTest.scala:1348-1365`.
- [ ] **P4** (correctness) `ModuleTest.scala:235,250`: `foreachPar(prepare)` fails fast and `concatAll` drops later cells. The test half of design commit 4 is missing.
- [ ] **P5** (correctness) `ModuleTest.scala:1319`: none of the seven callers passes `hear`, so test-compile warnings are dropped, and a failed test compile prints its messages with no file or line.
- [ ] **P6** (correctness) `testrun/SuiteStamp.scala:19-28` and `ModuleTest.scala:790,1293`: the `testQuick` stamp omits upstream locked jars, the JDK, and flags. It does blocking IO outside an effect, swallows `NonFatal`, and uses `Map.apply` (`SuiteStamp.scala:26`).
- [ ] **P7** (typed-errors) Errors become strings: `LinkFailed(detail: String)` (`ModuleTest.scala:116-121,453,506,604,660,672,733`), `Program` `Link`, `Process`, and `Library` (`Program.scala:106-220`), `NativeCompile.scala:21-22`, `NativeLink.scala:184,192`, `JsLink.scala:62`. `JvmCompile.scala:94,384-388` turns a `Throwable` message plus 8 frames into a diagnostic. `jdk/Toolchain` `Startup.Refuse(report: String)` and `CompileError.NativeLibrary(text: String)` carry text.
- [ ] **P8** (api-shape) `assembly/Assemble.scala:35,40-44,53` hardcodes `app`, `api`, `histos.cli.Main`, and `versions.first`, ignores `graph.assembly`, flattens `NoModule(error.message)`, and has no test.
- [ ] **P9** (dead code) `NativeLink.link` and `NativeKey` have test callers only. `NodeStage` has had a production caller, `NodeToolchain.choose`, since M1.5. `LinkError.Interrupted` and `Unexpected` and `NativeClasspath.entries` are unused.
- [ ] **P10** (stringly, booleanly) `framework: String` (`JsSuite.scala:30`, `NativeSuite.scala:24`), `Map[String, String]` child env, `Machine.userHome: String`, `hasJavac` and `hasBinary`, `slot(stop: Boolean)` (`Jdk.scala:199`, `Node.scala:114`), `Node.required: Boolean`, the module name `"hello"` (`NativeLink.scala:174`), and `HISTOS_JDK_SETTLED` written twice (`Toolchain.scala:26,209`).
- [ ] **P11** (cast, totality) `pack/LazyVals.scala:197-198` uses `asInstanceOf` after a guard, and `:109,114,160` index lists with `window(n)` and `window.head`.
- [ ] **P12** (correctness) Windows gaps: `native/Clang.scala` never looks for `clang.exe`, `ChildProcess` looks for `bin/java` beside `java`, and the entry point exits 3 when `setpgid` is missing.
- [ ] **P13** (correctness) `ScalaJs.scala:63-65` matches file names with `startsWith`. `native/ClassShapes.scala:22-30` appends with `:+` (quadratic) and holds every class's bytes.
- [ ] **P14** (concurrency) Effects run eagerly outside ZIO: `native/Clang.scala:121,127`, `NodeProbe.scala:163-169`.
- [ ] **P15** (typed-errors) `testrun/QuickRecord`: `save` and `clear` `.ignore` IO failures, `read` uses `try`/`catch NonFatal`, `write` checks `parent != null`, and `suite` and `digest` are `String`s. Found in M0.2.
- [ ] **T10** (tests) Value assertions written as `x match { case ... => ...; case _ => false }` inside `assertTrue` hide the failing value: about 9 in `ModuleTestSpec`, 3 in `ProgramSpec`, 8 in `GraphCompileSpec`, 2 in `PluginRunSpec`, 1 in `PublishSpec`, and 3 in `LauncherSpec`. Each spec converts to `is(...)` lenses when its step rewrites it. Found in M0.2.
- [ ] **T11** (duplication) `temp`, `write`, `entries`, and the `jdk` fixture are copied across `ModuleTestSpec`, `ProgramSpec`, and `QuickSpec`, and `jsProvider` takes `installJsdom: Boolean`. Found in M0.2.

## M6 One application service, four codecs

- [ ] **A1** (typed-errors) `app:server/Calls.scala:68-655` and `Daemon.scala:103-109` turn every domain error into `Message.Exit(code: Int, err: String)`, and the effect succeeds.
- [ ] **A2** (typed-errors) `app:client/Session.scala:55-59,98,129,252` and `mcp/HistosMcp.scala:76-105` are `ZIO[..., String, String]`. This is design decision 12 written as code.
- [ ] **A3** (stringly, booleanly) `Call` carries `List[String]`, `Run(id: String, platform: Option[String])`, `Resolve(action: String)`, `Workflow(action: String)`, and `quick: Boolean`. The CLI's enums are rendered to strings (`Cli.scala:821,904`) and parsed again on the server.
- [ ] **A4** (typed-errors) `Calls.scala:384-398` reports a bad suite pattern as `GraphCompileError.Target(CiError.BadName(...))`.
- [ ] **A5** (duplication) Compile-then-test continuation is written twice (`Cli.scala:659-744`, `Calls.scala:352-451`), and the engine's `ModuleTest.console` goes unused.
- [ ] **A6** (services) `bsp/Bsp.scala:504-572,586-676,978-1002` builds classpaths, parses the lock, matches source jars by file-name prefix, and parses the allow-list inside the handler.
- [ ] **A7** (stringly) `Bsp.scala:234-274,369,1144-1177` and `acp/Acp.scala:48-58` dispatch on method strings. Target URIs are parsed by hand in two places, and `"main"`/`"test"` are strings. The ACP half goes away with A11.
- [ ] **A8** (concurrency) `Calls.scala:453-494` keys the run registry by task id, so two runs of one id overwrite each other, and `Tasks.cancel` is bypassed. MCP's `Stdio.run` and the ACP loop serve one request at a time, so an agent cannot cancel its own run.
- [ ] **A9** (concurrency) `Daemon.scala:796-803` holds one global permit for a whole run or suite, so a Metals save waits behind `histos test`.
- [ ] **A10** (concurrency) BSP ignores `$/cancelRequest`, and one in-flight compile blocks every other request (`Daemon.scala:863-882`, `Bsp.scala:271`).
- [ ] **A12** (typed-errors) `Either[String, _]` and `LoadError.message` flattening: `Bsp.scala:994`, `Calls.scala:518,575`, `Cli.scala:529` (`pin: IO[String, Unit]`), `Daemon.scala:696,718,736`.
- [ ] **A13** (typed-errors) `Bsp.scala:792,842,936` use `.orDie` on file IO, so a defect kills the splice.
- [ ] **A14** (concurrency) `Bsp.scala:388-416` feeds a `ConcurrentLinkedQueue` from a `CompileProblem => Unit` callback.
- [ ] **A15** (dead code) `mcp/ToolchainView.scala` (174 lines) is unused.
- [ ] **A16** (duplication) `pin`, `placed` (twice in `Calls`), `relative` (also in `Lines`), the program exit mapping, the compile renderer (four copies), and `Cli.failedCell` and `Calls.failedSuite` are repeated between `Cli.scala` and `Calls.scala`.
- [ ] **A17** (correctness) A missing lock exits 8 from `compile` and 5 from `test` and `run` (`Cli.scala:448-459,749-758`), and `CliSpec.scala:201-219` asserts the split.
- [ ] **A18** (services) `server` and `client` import `CliExit`, `Lines`, and `Cli.testFailure`, and `cli` imports `server`. That is a package cycle.
- [ ] **A19** (booleanly) `dryRun`, `update`/`check`, and `test: Boolean` (`Cli.scala:68`, `Grammar.scala:24-25`, `Bsp.scala:599`), `ContentStore.clean(dryRun: Boolean)`, and `HeaderScan.scala:56` (`dependency: Boolean`).
- [ ] **A20** (services) `Daemon.scala:414-417,754-755` keeps `graphs` and `live` separately. `Bsp.open` takes the graph by value and by ref, and the first build classloader is never retired. `Daemon.handle` takes 13 parameters, `Bsp.open` 8.
- [ ] **A21** (stringly) The version string is hardcoded twice (`Bsp.scala:44-45`).
- [ ] **A22** (tests) `ServerSpec` has no reload, `NoServer`, reboot, `didChange`, concurrent-splice, permit-on-interrupt, or `testOnly`-through-server case. The test name at `:26` contradicts its assertion at `:55-57`. `ProtocolSpec` tests only the local path. `CliSpec.scala:61-67` reports fixture IO as `LoadError.Io`.
- [ ] **A23** (api-shape) `Session.perform` collects all output with `runCollect`, so an agent sees no progress.
- [ ] **A24** (api-shape) `GraphCompile.scala:16` and `ModuleTest.scala:15` reuse `CiError` from `workflow` as the selection error. `Selection` parses each name twice and reports a nested aggregate as `Unknown`.
- [ ] **A25** (correctness) A plugin command run through the server or MCP loses its arguments, because `Call.Task` carries only the task id. Found while correcting `plugins.md`.

## M7 The api grows up

- [ ] **G1** (api-shape) `api:ProjectDef.scala:31,33`: `test` and `provider` are both optional, and `MixedTest` is caught only at link time (`Graph.scala:258-259`).
- [ ] **G2** (api-shape) `ProjectDef.scala:30,34-37`: `clang`, `nativeVersion`, `jsEnv`, and `jsModule` sit on every module, backed by six runtime "X without Y" errors (`Graph.scala:290-316`).
- [ ] **G3** (api-shape) `Graph.scala:19-38,89-115,129-151,164-172,528-550`: `Module` has 18 fields, with 5 `trusted` overloads, 16 `apply` overloads, and a 7-parameter `build`.
- [ ] **G4** (stringly) The module name is a `String` checked at load. `ProjectName`, `ShipName`, `PullRequestNumber`, and `ProviderLibrary` have no compile-time literal.
- [ ] **G5** (stringly) `BuildError.DuplicateResolver(token: String)`, `DuplicateDirectory(directory: String)`, `baseDirectory: String`, `Dep` organization, name, and revision as `String` (an `Organization` type exists), `TestProvider.apply(String, ...)` returning `Either`, `PluginLoadError` coordinates as `String`, `ExtraOutput(path: String)`, `ModuleView.baseDirectory: String`, and raw-string coordinates in `engine:resolve/DependencyLock.scala:8-17`.
- [ ] **G6** (typed-errors) `Graph.scala:404-411` reports an `EntryNameError` as `PluginDefect`, with a `try`/`catch` in main code.
- [ ] **G7** (typed-errors, duplication) `api:Macros.scala:162` and `plugin/Macros.scala:91-92` use `Either[String, _]` in macro helpers, and the core keeps 12 hand-copied literal macros.
- [ ] **G8** (api-shape) `Literals.scala:5-12`: every interpolator takes `inline args: Any*`.
- [ ] **G9** (house rule) `api:Clang.scala:15`: the factory the macro splices lacks `@publicInBinary`.
- [ ] **G10** (typed-errors) `version/Session.scala:162-163` drops the `ShipNameError`, and `Session.scala:55-61` parses the session from a raw `Map[String, String]`.
- [ ] **G11** (services) `plugin/Names.scala:35-55` hardcodes the app's CLI words in the api, `ScalacFlag.scala:21-23` produces argv there, and loader types live in `plugin/Contribution.scala:62-72`.
- [ ] **G12** (api-shape) `BuildError` is a flat 47-case enum, and load reports the first error only.
- [ ] **G13** (api-shape) `ScalacFlag` has one case, so every new flag is an api change. Ascent is blocked on this.
- [ ] **G14** (api-shape) Settings are anonymous `ModuleDef => ModuleDef` functions with no owner, name, or conflict check.
- [ ] **G15** (stringly) Module notes are one-line strings that every plugin parses itself.
- [ ] **G16** (api-shape) `TaskDep` is closed to `Compile` and `Same`, so a plugin task cannot depend on link, test, or package.
- [ ] **G17** (tests) `PluginFixture` cannot set `RunRequest.compiled`.
- [ ] **G18** (duplication) `central`: `ArtifactId` duplicates `ArtifactBase` with no literal, `Digest` duplicates `ContentHash`, and `central.Revision` shadows `version.Revision`.
- [ ] **G19** (correctness) `BuildError.MissingProject.referencedFrom` holds the position of the missing module's own `module(...)` call, where it should point at the `dependsOn` or `ship` call that references it, and its `name` is a `String`. Found in M0.2.
- [ ] **G20** (correctness) `ShipLink.asBuild` maps `CatalogError.DuplicateName` to `BadShipName` with a made-up `SourcePos("<catalog>", 1, 1)`, and `Catalog.from` repeats checks `ShipLink` already made, so those branches look unreachable. Found in M0.2.
- [ ] **G21** (api-shape) Aggregates and edges find their children by name, not by the definition passed in, which is the only reason `BuildError.AggregateChild` can happen. Found in M0.2.
- [ ] **T14** (tests) `plugins.md` describes three things no test shows: a plugin `run` through `PluginFixture` (only a request is built today), one complete plugin lock cell with its `plugin`, `direct`, `artifact`, and `source` lines, and one header that combines a dependency, a path, and a link strategy. The doc links the nearest specs until these exist. Found when the doc moved to links.

## M8 CI from what the graph needs

- [ ] **C1** (booleanly) `engine:workflow/Workflow.scala:107,112,219-223,290-304`: `setup: Boolean` comes from the presence of `.github/ci-tools` and emits hardcoded Node 22, LLVM 21.1.8, Playwright 1.62.1, and Firefox steps, including Chekhov's private cache path. No test passes `true`. The steps run uncached on every job, so on 2026-10-22 the `test` job took 12 to 31 minutes, depending on the Ubuntu mirror serving the Firefox packages. Closing this row deletes the marker and restores each install the graph still needs from a cache keyed by tool revision and package set. Seeding that cache once is expected. Repeating `install-deps` or the browser download on a later job is not.
- [ ] **C2** (api-shape) Split axes render `--platform` and `--scala`, which the CLI rejects.
