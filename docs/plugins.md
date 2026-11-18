# Writing a plugin

Compile, package, and link are expensive. A result that is wrong, or a skip that hides a real change, is a failure even when the command was fast. Histos does that work only when the code it depends on changed. A plugin is how a build grows a capability without putting that capability in the histos binary.

That capability has to install itself honestly: the right classes, the right host compiler, a rebuild when the host moves, and a cache that misses when the inputs changed. Specular is the producer this is aimed at. It turns a Scala test into a documentation site. Terret, and any other library, takes that as a dependency.

The previous shape could not do that. The build named plugins on the graph, and `histos.wiring.scala` installed them after main had already been compiled, so main could not see the plugin's types. A coordinate on the classpath was treated as the plugin being on. A published jar was a binary bound to the histos that compiled it. When the host API moved, that binary was a guess. The cache keyed on the wrong bytes, so a skip could hide a stamp, a generated source, or a swapped host API. Failures came out as strings.

## The strategy

Three files, and most builds use two.

`histos.plugins.scala` is read before any plugin exists. It names coordinates, resolvers, and whether the build locks. It imports no plugin class. The host can prepare plugin classes because the request for them does not depend on them.

`histos.scala` is the build. Once the classes exist, `services` turns a capability on by its interface. A task is a separate activation, and only when the plugin has a command. A coordinate does not install a service. Registering a concrete class, or registering the same interface twice, is refused before the layer runs.

`histos.strategies.scala` is optional. It exists when the policy that loads the remaining plugins is itself a plugin. It runs after a small core seed and before the rest. An ordinary consumer never writes it. There is no required wiring file.

Lock intent lives on the catalog, before plugins load. Main inherits it. A main file that picks a different resolution is refused. A lock file on disk does not decide.

A service is a constructor-injected ZLayer. Its setup error is a typed enum. A resource scoped to the generation outlives one task and ends when that generation ends. The host owns the Scala, ZIO, and compiler identity. The plugin sees the exported API. It does not reach into the engine.

Publication defaults to a rebuildable bundle: sources, generated sources, resources, flags, compile-only inputs, and the helper closure. The jar records the host API it was built against. An exact match uses the published binary. Drift recompiles that bundle against this host and caches the result. Frozen CI does that from the pinned inputs and does not solve. Binary-only is explicit: binding metadata, no recipe, and drift is a refusal.

## Why this wins

The host can compile a build that uses a plugin, because the request for the plugin does not import the plugin.

Turning a capability on is a typed line in the build, keyed by the interface the consumer programs against. Two plugins that claim that interface fail before either layer runs. A classpath entry cannot become behavior on its own.

A host upgrade does not strand the plugin and does not replay the producer's build. The recipe is the inputs. The host compiler is the compiler. The cache key is those bytes, so a skip means those bytes did not change.

Frozen CI stays frozen. A locked build keeps its snapshot bytes until someone asks to refresh. An unlocked build solves.

## How a producer uses it

The plugin is a normal JVM module. It depends on `histos-api` as `compileOnly`. Its public capability is a trait. The implementation is a ZLayer. The module is marked `.pluginLibrary`, which publishes the rebuildable bundle bound to the host API that compiled it. A helper is an ordinary module the plugin `dependsOn`. Compile-only dependencies stay off the consumer POM. A compile-only snapshot does not refuse that publication. A snapshot the POM would fetch still does. The proof is `a compile-only snapshot does not refuse the ship` and `a pom snapshot dependency refuses the ship` in `PublishSelectSpec`.

The producer build's catalog names that module. Its `services` registers the layer by the trait. There is no `histos.wiring.scala`. Publishing is `./histos publish snapshots` of that module. The consumer does not need the producer's repository.

## How a consumer uses it

The catalog names the published coordinate. That line does not import the plugin. Lock intent, when the build wants a lock, is set on the catalog and the graph inherits it. An unlocked build keeps solving.

`histos.scala` imports the trait and turns the service on with the consumer's own configuration. That is the file that imports the plugin. `histos run <plugin>/<task>` runs a command when the plugin has one. A service with no command does not need a dummy task.

On an exact host match, the consumer runs the published binary. When the host API has moved, Histos recompiles the published sources against the consumer's host and caches that compile.

The sections below are the contracts. The compiler fixtures they name are the examples. This repository's `histos.scala` declares no plugins.

## Files

`histos.plugins.scala` is optional and core-safe. `object plugins extends HistosPlugins` names coordinates, `resolution`, resolvers, `bootstrapLocks`, and an optional strategy file. It does not import plugin classes.

`histos.strategies.scala` is optional. The catalog names it with `BootstrapSource.File`. `object strategies extends HistosStrategies` wires early lock and loading strategies, after the core seed and before the remaining plugins.

`histos.scala` is `object build extends HistosBuild`. `services` activates graph-phase locks, services, and tasks, and it may import plugin types. This path has no `histos.wiring.scala`.

No catalog still compiles `histos.scala` and keeps that graph's resolution. `Projects.locked` selects `Resolution.Lock`. Omission is `Resolution.Solve`. A lock file on disk does not select either. Main inherits the catalog resolution. An explicit main selection that differs is `BootstrapError.ResolutionConflict`. That refusal is the test `an explicit main resolution that conflicts with the catalog is refused` in [PluginBootstrapSpec.scala](../engine/test/histos/load/PluginBootstrapSpec.scala).

## Examples

The compiler reads these strings. This guide does not copy them.

Positive text, compiled by `the spec author fixtures compile` in [PluginBootstrapSpec.scala](../engine/test/histos/load/PluginBootstrapSpec.scala):

| Example | Fixture |
| --- | --- |
| Locked catalog | [locked catalog](../engine/test/histos/load/PluginBootstrapSpec.scala#L1126-L1137) |
| Main services and tasks | [consumer](../engine/test/histos/load/PluginBootstrapSpec.scala#L1208-L1223) |
| Early policy, then another plugin | [advanced catalog](../engine/test/histos/load/PluginBootstrapSpec.scala#L1138-L1159), [strategies](../engine/test/histos/load/PluginBootstrapSpec.scala#L1197-L1207) |
| Local module, helper, external library | [local catalog](../engine/test/histos/load/PluginBootstrapSpec.scala#L1160-L1179) |
| Producer services | [ReviewedLocks.scala](../engine/test/histos/load/PluginBootstrapSpec.scala#L1016-L1060) |
| Producer task | [ReviewTasks.scala](../engine/test/histos/load/PluginBootstrapSpec.scala#L1062-L1097) |
| Typed setup error | [CheckedReviewLabel.scala](../engine/test/histos/load/PluginBootstrapSpec.scala#L1099-L1123) |
| Producer build | [producer](../engine/test/histos/load/PluginBootstrapSpec.scala#L1180-L1193) |

`.pluginLibrary` and `.pluginLibrary(PluginPublication.BinaryOnly)` also typecheck as the [publication snippet](../api/test/histos/plugin/PluginContractsSpec.scala#L110-L115).

`BuildServices.default.add[ReviewLabel](CheckedReviewLabel.layer("reviewed"))` is passed to a helper whose argument is `Plan[NoExtraServices & ReviewLabel, LabelSetupError]`. The call site has no ascription. The helpers are [PluginContractsSpec.scala](../api/test/histos/plugin/PluginContractsSpec.scala#L61-L63) and [BuildServicesSpec.scala](../api/test/histos/plugin/BuildServicesSpec.scala#L12-L14). The calls are [PluginContractsSpec.scala](../api/test/histos/plugin/PluginContractsSpec.scala#L193-L197) and [BuildServicesSpec.scala](../api/test/histos/plugin/BuildServicesSpec.scala#L136-L137). The label types those specs compile are smaller than the producer strings above: [PluginContractsSpec.scala](../api/test/histos/plugin/PluginContractsSpec.scala#L14-L53). `ReviewTasks.contribute` there is empty. The task body the load compiles is the `ReviewTasks` string in the table.

A services-only plan is `withLocks` and nothing else. [exactNothing](../api/test/histos/plugin/PluginContractsSpec.scala#L59) expects `Plan[NoExtraServices, Nothing]`, and [the call](../api/test/histos/plugin/PluginContractsSpec.scala#L197) is `BuildServices.default.withLocks(Reviewed.layer)`. The strategies fixture is the services-only file. Omit `.add` and `.tasks` unless a capability or a task should actually run.

The local catalog names a helper with `dependsOn` and one external library. The load proofs are `a local module and an external jar load` in `PluginBootstrapSpec` and `a local module compile sees its external jar` in [PluginClosureSpec.scala](../engine/test/histos/load/PluginClosureSpec.scala).

A resolver on the catalog is where that module's libraries are resolved. Main is compiled after the module, so a resolver written only on the graph is too late. `PluginResolvers.Central` is Central. A listed set is that set, with Central last when the author names it. The proof is `a catalog resolver reaches a local plugin dependency` in `PluginBootstrapSpec`.

A resolved `scala-library` or `scala3-library` is dropped from that classpath. The compiler already supplies the runtime inside the assembly, and a second copy is a different `scala` package. The proof is `a resolved standard library is not a second copy beside the assembly` in `PluginBootstrapSpec`.

These strings do not compile. They are `typeChecks` fixtures in [BuildServicesSpec.scala](../api/test/histos/plugin/BuildServicesSpec.scala):

| Refusal | Fixture | Boundary that does compile |
| --- | --- | --- |
| `tasks` without `ReviewLabel` | [withoutLabel](../api/test/histos/plugin/BuildServicesSpec.scala#L41-L53) | [withLabel](../api/test/histos/plugin/BuildServicesSpec.scala#L27-L39) |
| `withPluginLoading` on main | [lateLoading](../api/test/histos/plugin/BuildServicesSpec.scala#L61-L65) | [earlyLoading](../api/test/histos/plugin/BuildServicesSpec.scala#L55-L59) |
| Generation layer needs `InvocationContext` | [registration](../api/test/histos/plugin/BuildServicesSpec.scala#L77-L84) | [layer type](../api/test/histos/plugin/BuildServicesSpec.scala#L67-L74) |
| Generation layer needs `PluginEvents.Invocation` | [registration](../api/test/histos/plugin/BuildServicesSpec.scala#L86-L94) | [layer type](../api/test/histos/plugin/BuildServicesSpec.scala#L96-L103) |
| Intersection registration key | [intersectionKey](../api/test/histos/plugin/BuildServicesSpec.scala#L114-L123) | [one capability](../api/test/histos/plugin/BuildServicesSpec.scala#L125-L133), and a `Tag` of an intersection ([intersectionTag](../api/test/histos/plugin/BuildServicesSpec.scala#L105-L111)) |

The loader also refuses a late main override: `a late main withPluginLoading is refused`.

The reviewed-locks spec is [ReviewedLocksSpec.scala](../examples/reviewed-locks/test/com/example/reviewed/ReviewedLocksSpec.scala). `PluginBootstrapSpec` compiles it. The lock-repair transcript is [expected-lock-repair.txt](../examples/expected-lock-repair.txt). The compiled kit use is [PluginHarnessSpec.scala](../plugin-testkit/test/histos/plugin/testkit/PluginHarnessSpec.scala).

## Services

`BuildServices.default` is `Plan[NoExtraServices, Nothing]`.

`inline def add[A](using Tag[A])` returns `AddPartiallyApplied`. Its `apply` takes `ZLayer[GenerationAmbient & CoreServices & S, Nothing, A]`, or a layer whose error is `E1 <: ServiceSetupError`. The result is `Plan[S & A, E]` or `Plan[S & A, E | E1]`. The call is `add[ReviewLabel](layer)`. `ReviewLabel` is the interface.

`.withLocks` replaces the core lock slot once. A second call is `ServiceActivationError.DuplicateReplacement`, and the new layer does not run. A second `.add` of the same key is `DuplicateService`, and neither layer runs. Those tests are `a second withLocks is refused before its layer runs` and `a duplicate service is refused before its layer runs`.

`.add[P].tasks[P]` builds one instance. The task slot reuses it. [PluginHarnessSpec.scala](../plugin-testkit/test/histos/plugin/testkit/PluginHarnessSpec.scala#L47) is that activation.

A setup failure is an enum that extends `ServiceSetupError` and implements `diagnosis: Noted`. It does not extend `Exception`. An empty label fails before execution and keeps the previous generation: `an empty label keeps the previous generation and surfaces the diagnosis`.

`GenerationAmbient` is `BuildContext & PluginEvents.Generation`. It has no command, caller, or consent channel. `CoreServices` is `LockStrategy & PluginLoadingStrategy`.

## Tasks

`inline def tasks[P <: HistosPlugin](using Tag[P])` has the same layer shape as `add`. `contribute` is data, not an effect. `run` is `ZIO[Scope & PluginScopes, PluginRunError, PluginOutput]`. That `Scope` closes when `run` returns, including on interrupt. `scopes(ResourceLife.Generation)` is the generation scope, and it outlives one run.

The reviewed task records `label.value` as a declared text input because that value changes the output. The string is the `ReviewTasks` fixture above.

A root command uses `CommandName`. A name in `CommandName.reserved` is refused. `acp` is reserved and has no command. `histos run <plugin>/<task>` runs a task and does not take program arguments.

## Publication

`.pluginLibrary` means `PluginPublication.Rebuildable`. `.pluginLibrary(PluginPublication.BinaryOnly)` keeps binding metadata and publishes no rebuild recipe. Authors do not hand-write that metadata. That jar carries `META-INF/histos/version`, the sha256 of the host api jar. A module the graph names as a plugin carries the same line. The proof is `a plugin library is stamped when the graph names no coordinate` in `VersionStampSpec`.

A helper is an ordinary module the plugin module `dependsOn`. Compile-only dependencies stay off the consumer POM. Binary-only publication is `binary-only keeps binding metadata and makes no rebuild claim` in [PluginPublicationSpec.scala](../engine/test/histos/publish/PluginPublicationSpec.scala). Drift of a binary-only helper is `binary-only helper drift is refused` in `PluginBootstrapSpec`. Drift of a rebuildable helper rebuilds: `helper drift rebuilds when the publication carries sources`.

## Loading

`PluginLoading.exactOrRebuild`, `rebuildAlways`, and `exactBinaryOnly` are layers of `PluginLoadingStrategy`. Only the strategy file installs one. Main has no `withPluginLoading`.

`exactOrRebuild` keeps an exact host binding. Drift of a rebuildable publication rebuilds. Drift of a binary-only publication is refused. A missing recipe is refused. The strategy cannot waive an integrity or boundary check.

## Lock

`LockDefaults.core` is local `Offer` and CI `Frozen`. `bootstrapLocks` on the catalog replaces that pair. A `LockStrategy` reads `LockContext` in `decide` and returns a `LockDecision`. It does not commit the lock. The caller is an argument of `decide`, not a constructor input.

On this loader, `Frozen` and `UsePinned` materialize pins and do not solve. `OfferRepair`, `Update`, and `LocalSolve` may solve and rebuild, and they take the same path. `Reject` fails the load. Nothing in that path prompts. The daemon session's `ConsentDesk` holds one `LockCoordinator`. An offer is a lock event on the build feed, and a reply accepts or declines it.

`dependencies.lock` is the file `DependencyLock.render` writes.

- `cell` or `test`, then the module, the Scala version, and the platform.
- `direct`, then organization, name, revision, and `java`, `binary`, or `full`.
- `artifact`, then organization, name, revision, sha256, and repository. A classifier is a trailing field when present.
- `evict`, then organization, name, requested revision, and selected revision.
- `plugin`, then organization, name, revision, and `exact-binary`, `source`, or `binary-or-source`.
- `source`, the source jar of a plugin, with the same checksum fields as `artifact`.
- `tool`, then that tool's roots, resolvers, artifacts, and evictions.

A published plugin coordinate in Scala is a `dep"..."` literal. The lock stores it as a `plugin` line. There is no other published-pin syntax.

`histos resolve` fetches the lock and checks each sha256. `resolve --update` solves and writes it. `resolve --check` fails when a fresh solve differs. `resolve --refresh` revalidates retained snapshot locators and does not fetch. Offer is the default and `--lock=update` commits. No selector means every retained locator. A dependency lock is not rewritten.

## Test kit

`plugin-testkit` depends on `api` and ZIO Test. It is in this build and it is not a member of `Ship("histos")`. This guide does not name a published coordinate for it.

`PluginHarness.activate` builds one plan in a `Scope`. `PluginHarness.caller` and `PluginHarness.run` give that caller its own command and its own event queue. `LockHarness.missingLock` is locked graph discovery with no revision. The suite is [PluginHarnessSpec.scala](../plugin-testkit/test/histos/plugin/testkit/PluginHarnessSpec.scala).

## Migration

Scaffold does not write `histos.wiring.scala`. A file with that name is not a load input, a BSP build source, or a watch root. A consumer moves coordinates off `Projects.plugins` onto the catalog, sets `resolution` there when the build should lock, and moves activation onto `services` with interface keys. Early policy stays on the strategy file, and main does not repeat it unless it means to replace the graph lock strategy. A producer marks the module `.pluginLibrary`, or passes `PluginPublication.BinaryOnly`. This repository's `histos.scala` does not do that move.

## Still to prove

The contracts above typecheck. A checked line names the command, the head, and the pull request whose CI ran it. A local re-run of an old spec does not close a line.

Already proven on `0.5.0-36642bcc3ec5-SNAPSHOT`, packed from `36642bcc`. CI run 44338066046 on `09ccb7e` (#240), squash `e96c5d32`: consent on the installed jar, the native client host task, the version line against the pin, the compiler payload staying out of the project lock, and a lone `histos.wiring.scala` not being compiled. `Ship.next` is still 0.5.0. Nothing has been released. The pointer `0.5.0-SNAPSHOT` is not this pin.

Proved on #241, head `a363b38`, squash `6c0100d2`. CI run 44354664859, test job 134999536356, command `./histos test --affected '["app"]'`. `BspMetalsSpec` passed 9. The app suite passed 75, including `InstalledHostProofSpec`. No test-clock warning.

Proved on #242, head `1432671a`, squash `104f9aca`. CI run 44366420546, test job 135025861460, command `./histos test --affected '["engine","app"]'`. The engine suite passed 148, including the frozen load, the binary-only helper refusal, the catalog resolver, and the unsupported publication stage.

Proved on #244, head `0319ba8e`, squash `a91069ce`. CI run 44372823054, test job 135055870275, command `./histos test --affected '["engine","app"]'`. The engine suite passed 149, including the standard-library filter.

Proved on #246, head `dd27cb09`, squash `6e005f23`. CI run 44379976909, test job 135073537704, command `./histos test --affected '["engine","app"]'`. The engine suite passed 151, including both `PublishSelectSpec` tests.

### Load a plugin correctly

- [x] BSP does not treat a source file as the host build because its text contains `trait HistosBuild`. A comment with that text still compiles. `BspMetalsSpec`, "a comment that names HistosBuild stays a build source".
- [x] Main inherits the catalog resolution. A conflicting main selection is refused. `InstalledHostProofSpec`, "A1 a catalog intent is what main inherits and a conflicting main resolution is ResolutionConflict".
- [x] A frozen load of a locked catalog does not solve. `PluginBootstrapSpec`, "a frozen miss does not solve". `./histos test --affected '["engine","app"]'` on `1432671a` (#242), run 44366420546, test job 135025861460.
- [x] The first run loads `histos.strategies.scala` after the core seed and before the other plugins, and that policy runs. `InstalledHostProofSpec`, "A2 histos.strategies.scala runs after the core seed and before the remaining solve".
- [x] Registration is the capability interface. An intersection key is refused. `InstalledHostProofSpec`, "A3 registration is the capability interface and an intersection key is refused".
- [ ] A duplicate service, a second lock replacement, and a setup failure are refused before the layer runs. `BuildServicesSpec`, "a duplicate service is refused before its layer runs", "a second withLocks is refused before its layer runs", and "an empty label keeps the previous generation and surfaces the diagnosis".
- [x] Helper-only drift is `BindingDrift` while the root stamp matches. `InstalledHostProofSpec`, "R6 a helper-only stamp mismatch is BindingDrift and the root stamp matches".
- [x] Binary-only helper drift is refused. `PluginBootstrapSpec`, "binary-only helper drift is refused". `./histos test --affected '["engine","app"]'` on `1432671a` (#242), run 44366420546, test job 135025861460.
- [x] A local plugin compile sees the external jars that module resolved. `InstalledHostProofSpec`, "R7 a local plugin compile sees its external jar".
- [x] A resolver declared on the catalog is where a local plugin module resolves its libraries. `PluginBootstrapSpec`, "a catalog resolver reaches a local plugin dependency". `./histos test --affected '["engine","app"]'` on `1432671a` (#242), run 44366420546, test job 135025861460.
- [x] A resolved standard library is not a second copy beside the assembly. `PluginBootstrapSpec`, "a resolved standard library is not a second copy beside the assembly". `./histos test --affected '["engine","app"]'` on `0319ba8e` (#244), run 44372823054, test job 135055870275.
- [x] A service built for the generation stays alive through the task and is released when that generation ends. `InstalledHostProofSpec`, "R10 a generation service is alive for the task and closed after the last lease".

### Cache what the inputs actually are

- [x] Old plugin bytes reuse their task output. New bytes change the key and the output. `InstalledHostProofSpec`, "R1 old plugin bytes reuse a stale task output and new bytes change the key and the output".
- [ ] An empty artifact or mixed provenance does not return a stale task result. `PluginTaskIdentitySpec`, "an unverified closure is not an empty artifact list" and "mixed provenance does not return a stale task result".
- [x] Replacing the host API jar misses the compile cache unless the input hash is of those bytes. `InstalledHostProofSpec`, "R3 a host-API byte change misses the compile cache".
- [x] A second load of the same Java sources, resources, and compiler flags starts no compiler. `InstalledHostProofSpec`, "R8 settings and resources reach the compile and a second identical load starts the compiler zero times".
- [x] `CompileInputs.remember` follows the test clock. `InstalledHostProofSpec`, "R12 CompileInputs.remember stamps the analysis with the test clock".

### Publish a recipe a consumer can rebuild

- [x] The package key changes when the only difference is the API stamp bytes a local publish injects. `InstalledHostProofSpec`, "R2 stamp bytes change the package key and leave the product digest".
- [x] The published source jar includes generated replacements. The recipe includes compile-only inputs, and those inputs stay off the consumer POM. `InstalledHostProofSpec`, "R4 a generated replacement from Sources.prefer is the published rebuild entry" and "R5 a compile-only input is in the recipe and absent from the consumer POM".
- [x] A publication the recipe does not support uploads nothing. `PluginPublicationSpec`, "an unknown stage refuses publication before an upload". `./histos test --affected '["engine","app"]'` on `1432671a` (#242), run 44366420546, test job 135025861460.
- [x] A compile-only snapshot does not refuse the ship. A snapshot the POM would fetch does. `PublishSelectSpec`, "a compile-only snapshot does not refuse the ship" and "a pom snapshot dependency refuses the ship". `./histos test --affected '["engine","app"]'` on `dd27cb09` (#246), run 44379976909, test job 135073537704.
- [x] The export manifest includes the theme and step packages. An engine reference is refused. `InstalledHostProofSpec`, "R11 theme and step are exported and an engine package is refused".
- [ ] A duplicate export fails. `PluginBootstrapSpec`, "a duplicate export fails".
- [x] A compile failure and a scheduler failure stay distinct from a task failure after bind. `InstalledHostProofSpec`, "R13 a compile failure and a scheduler failure stay distinct through bind".
- [x] A TASTy entry-byte change changes the seal, and the consumer sees it. `InstalledHostProofSpec`, "A4 a TASTy entry-byte change changes the seal and the consumer sees it".

### A real plugin

- [ ] Specular publishes `specular-histos` from the catalog and `.pluginLibrary`, with no `histos.wiring.scala`, on this histos pin. The coordinate stays `0.0.3-SNAPSHOT`.
- [ ] Terret loads that snapshot from the catalog and runs the service or task it registers. Terret stays unlocked.
