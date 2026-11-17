# Writing a plugin

These contracts are the ones the compiler fixtures typecheck. The assembled-host proof is recorded in [plugin-architecture-work-plan.md](plugin-architecture-work-plan.md). `histos.version` in `build.properties` is `0.5.0-9e4a872c9265-SNAPSHOT`, the assembled host packed from `9e4a872`. This repository's `histos.scala` declares no plugins.

A plugin is a constructor-injected ZIO service. A coordinate makes its types available. It does not install a service or a task. The registration key is the capability interface. A task is a separate `HistosPlugin` activation, even when it is the same class.

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
| Locked catalog | [locked catalog](../engine/test/histos/load/PluginBootstrapSpec.scala#L1033-L1044) |
| Main services and tasks | [consumer](../engine/test/histos/load/PluginBootstrapSpec.scala#L1115-L1130) |
| Early policy, then another plugin | [advanced catalog](../engine/test/histos/load/PluginBootstrapSpec.scala#L1045-L1066), [strategies](../engine/test/histos/load/PluginBootstrapSpec.scala#L1104-L1114) |
| Local module, helper, external library | [local catalog](../engine/test/histos/load/PluginBootstrapSpec.scala#L1067-L1086) |
| Producer services | [ReviewedLocks.scala](../engine/test/histos/load/PluginBootstrapSpec.scala#L923-L967) |
| Producer task | [ReviewTasks.scala](../engine/test/histos/load/PluginBootstrapSpec.scala#L969-L1004) |
| Typed setup error | [CheckedReviewLabel.scala](../engine/test/histos/load/PluginBootstrapSpec.scala#L1006-L1030) |
| Producer build | [producer](../engine/test/histos/load/PluginBootstrapSpec.scala#L1087-L1100) |

`.pluginLibrary` and `.pluginLibrary(PluginPublication.BinaryOnly)` also typecheck as the [publication snippet](../api/test/histos/plugin/PluginContractsSpec.scala#L110-L115).

`BuildServices.default.add[ReviewLabel](CheckedReviewLabel.layer("reviewed"))` is passed to a helper whose argument is `Plan[NoExtraServices & ReviewLabel, LabelSetupError]`. The call site has no ascription. The helpers are [PluginContractsSpec.scala](../api/test/histos/plugin/PluginContractsSpec.scala#L61-L63) and [BuildServicesSpec.scala](../api/test/histos/plugin/BuildServicesSpec.scala#L12-L14). The calls are [PluginContractsSpec.scala](../api/test/histos/plugin/PluginContractsSpec.scala#L193-L197) and [BuildServicesSpec.scala](../api/test/histos/plugin/BuildServicesSpec.scala#L136-L137). The label types those specs compile are smaller than the producer strings above: [PluginContractsSpec.scala](../api/test/histos/plugin/PluginContractsSpec.scala#L14-L53). `ReviewTasks.contribute` there is empty. The task body the load compiles is the `ReviewTasks` string in the table.

A services-only plan is `withLocks` and nothing else. [exactNothing](../api/test/histos/plugin/PluginContractsSpec.scala#L59) expects `Plan[NoExtraServices, Nothing]`, and [the call](../api/test/histos/plugin/PluginContractsSpec.scala#L197) is `BuildServices.default.withLocks(Reviewed.layer)`. The strategies fixture is the services-only file. Omit `.add` and `.tasks` unless a capability or a task should actually run.

The local catalog names a helper with `dependsOn` and one external library. The load proofs are `a local module and an external jar load` in `PluginBootstrapSpec` and `a local module compile sees its external jar` in [PluginClosureSpec.scala](../engine/test/histos/load/PluginClosureSpec.scala).

These strings do not compile. They are `typeChecks` fixtures in [BuildServicesSpec.scala](../api/test/histos/plugin/BuildServicesSpec.scala):

| Refusal | Fixture | Boundary that does compile |
| --- | --- | --- |
| `tasks` without `ReviewLabel` | [withoutLabel](../api/test/histos/plugin/BuildServicesSpec.scala#L41-L53) | [withLabel](../api/test/histos/plugin/BuildServicesSpec.scala#L27-L39) |
| `withPluginLoading` on main | [lateLoading](../api/test/histos/plugin/BuildServicesSpec.scala#L61-L65) | [earlyLoading](../api/test/histos/plugin/BuildServicesSpec.scala#L55-L59) |
| Generation layer needs `InvocationContext` | [registration](../api/test/histos/plugin/BuildServicesSpec.scala#L77-L84) | [layer type](../api/test/histos/plugin/BuildServicesSpec.scala#L67-L74) |
| Generation layer needs `PluginEvents.Invocation` | [registration](../api/test/histos/plugin/BuildServicesSpec.scala#L86-L94) | [layer type](../api/test/histos/plugin/BuildServicesSpec.scala#L96-L103) |
| Intersection registration key | [intersectionKey](../api/test/histos/plugin/BuildServicesSpec.scala#L114-L123) | [one capability](../api/test/histos/plugin/BuildServicesSpec.scala#L125-L133), and a `Tag` of an intersection ([intersectionTag](../api/test/histos/plugin/BuildServicesSpec.scala#L105-L111)) |

The loader also refuses a late main override: `a late main withPluginLoading is refused`.

The `ReviewedLocksSpec` fence in the architecture doc is not a compiler fixture. The compiled kit use is [PluginHarnessSpec.scala](../plugin-testkit/test/histos/plugin/testkit/PluginHarnessSpec.scala). The lock-repair transcript in the architecture doc is not a file in this tree.

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

`.pluginLibrary` means `PluginPublication.Rebuildable`. `.pluginLibrary(PluginPublication.BinaryOnly)` keeps binding metadata and publishes no rebuild recipe. Authors do not hand-write that metadata. A module the graph names as a plugin carries `META-INF/histos/version`, the sha256 of the host api jar.

A helper is an ordinary module the plugin module `dependsOn`. Compile-only dependencies stay off the consumer POM. Binary-only publication is `binary-only keeps binding metadata and makes no rebuild claim` in [PluginPublicationSpec.scala](../engine/test/histos/publish/PluginPublicationSpec.scala). Drift of a binary-only helper is `binary-only helper drift is refused` in `PluginBootstrapSpec`. Drift of a rebuildable helper rebuilds: `helper drift rebuilds when the publication carries sources`.

## Loading

`PluginLoading.exactOrRebuild`, `rebuildAlways`, and `exactBinaryOnly` are layers of `PluginLoadingStrategy`. Only the strategy file installs one. Main has no `withPluginLoading`.

`exactOrRebuild` keeps an exact host binding. Drift of a rebuildable publication rebuilds. Drift of a binary-only publication is refused. A missing recipe is refused. The strategy cannot waive an integrity or boundary check.

## Lock

`LockDefaults.core` is local `Offer` and CI `Frozen`. `bootstrapLocks` on the catalog replaces that pair. A `LockStrategy` reads `LockContext` in `decide` and returns a `LockDecision`. It does not commit the lock. The caller is an argument of `decide`, not a constructor input.

On this loader, `Frozen` and `UsePinned` materialize pins and do not solve. `OfferRepair`, `Update`, and `LocalSolve` may solve and rebuild, and they take the same path. `Reject` fails the load. Nothing in that path prompts. `LockCoordinator` is not wired to a command.

`dependencies.lock` is the file `DependencyLock.render` writes.

- `cell` or `test`, then the module, the Scala version, and the platform.
- `direct`, then organization, name, revision, and `java`, `binary`, or `full`.
- `artifact`, then organization, name, revision, sha256, and repository. A classifier is a trailing field when present.
- `evict`, then organization, name, requested revision, and selected revision.
- `plugin`, then organization, name, revision, and `exact-binary`, `source`, or `binary-or-source`.
- `source`, the source jar of a plugin, with the same checksum fields as `artifact`.
- `tool`, then that tool's roots, resolvers, artifacts, and evictions.

A published plugin coordinate in Scala is a `dep"..."` literal. The lock stores it as a `plugin` line. There is no other published-pin syntax.

`histos resolve` fetches the lock and checks each sha256. `resolve --update` solves and writes it. `resolve --check` fails when a fresh solve differs.

## Test kit

`plugin-testkit` depends on `api` and ZIO Test. It is in this build and it is not a member of `Ship("histos")`. This guide does not name a published coordinate for it.

`PluginHarness.activate` builds one plan in a `Scope`. `PluginHarness.caller` and `PluginHarness.run` give that caller its own command and its own event queue. `LockHarness.missingLock` is locked graph discovery with no revision. The suite is [PluginHarnessSpec.scala](../plugin-testkit/test/histos/plugin/testkit/PluginHarnessSpec.scala).

## Migration

Scaffold does not write `histos.wiring.scala`. A file with that name is not a load input, a BSP build source, or a watch root. A consumer moves coordinates off `Projects.plugins` onto the catalog, sets `resolution` there when the build should lock, and moves activation onto `services` with interface keys. Early policy stays on the strategy file, and main does not repeat it unless it means to replace the graph lock strategy. A producer marks the module `.pluginLibrary`, or passes `PluginPublication.BinaryOnly`. This repository's `histos.scala` does not do that move.
