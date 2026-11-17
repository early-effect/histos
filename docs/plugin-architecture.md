# Histos plugin architecture and dependency-lock transactions

**Status:** Reviewed proposed architecture, approved documentation handoff. Two review rounds closed six design findings (four major, two minor), with zero open design issues. D1-D15 remain final; D16 clarifies relevant test cleanup without changing architecture. R9 is the assembled-host row this diff closes. A5-A8 stay open. The other ledger rows stay open.

**Document:** `docs/plugin-architecture.md`. Docs-only approval covers saving this handoff and document checks, not feature implementation, commit, push, PR, publishing, or release.

**Historical baseline:** Clean `main`, `e0027a30926add6103700cba41e9ac7dc79e665e`, inspected on 2026-11-16. Findings describe that baseline, not a newly audited implementation.

**Owner:** Russ. R9 is the row this diff closes. A5-A8 stay open. Local jars are recorded in [plugin-architecture-work-plan.md](plugin-architecture-work-plan.md).

## Handoff Summary

Make a plugin an ordinary constructor-injected ZIO service. A dependency declaration makes its exported types available; explicit typed activation installs capabilities and, separately, tasks. Compile core-safe `histos.plugins.scala` first, load plugin inputs, then compile `histos.scala` against those types. The normal model has no required `histos.wiring.scala`.

Published fallback uses a complete rebuild bundle, not a source jar. Keep immutable published inputs in `dependencies.lock`; keep derived host-specific binaries in the content store. The default loader uses an exact host-API binary when its linking closure is valid, otherwise rebuilds against the host boundary/compiler with a notice. Frozen CI can rebuild pinned inputs, without solving new inputs or writing the lock.

A repair is a preparation transaction before command side effects: discover the necessary closure, solve a candidate, show its full diff, obtain revision-bound consent, atomically replace the lock, adopt the generation, and execute the suspended command once. Decline, solving failure, and pre-commit interruption retain the previous committed lock.

**First action for the next architect:** Apply owner-supplied local instructions, read the settled handoff and Design Review Ledger, and compare the checkout with the historical baseline. Obtain separate implementation authorization before starting a work unit. B1/B2 and AR1-AR6 are settled; implementation findings remain open until their owning work has failing-first reproduction and green executable proof.

## Overview

Installation, typed build configuration, service composition, and reliable host-upgrade reuse meet in bootstrap. The host cannot compile a build importing plugin classes until those classes exist. Preparing those classes can itself require a custom lock/loading policy.

Policies choose actions. Infrastructure verifies bytes, builds classpaths, owns scopes, and commits transactions. Publication supplies immutable compilation inputs without replaying the producer build. Plugins retain exported API access; engine internals and shared runtime ownership remain controlled.

Use the existing Scala 3, ZIO, ZIO Test, compiler, Coursier, content-store, and wire zio-json stack. No new external dependency or compiler plugin is approved. Slayer is already a test dependency for appropriate scoped external-service tests.

## Current State

These are static findings, not runtime reproductions.

| Seam | Current behavior |
| --- | --- |
| [Resolution.scala](../api/src/histos/Resolution.scala), [Graph.scala](../api/src/histos/Graph.scala) | `Solve` is default; `Projects.locked` selects `Lock`. `HistosBuild` has no general services contract. Plugin coordinates live on the graph. |
| [Loader.scala](../engine/src/histos/load/Loader.scala) | `open` compiles core-only main build, reads graph, compiles local plugins, loads published plugins, then compiles `histos.wiring.scala`. Main imports of newly declared plugin types cannot work. `loadLocked` ignores its supplied lock and uses `ShipSource.Solve`. |
| [PluginShips.scala](../engine/src/histos/load/PluginShips.scala) | `Source`, `BinaryOrSource`, and `ExactBinary` all use binaries and exact API jar-byte SHA-256. `stamped` checks the direct artifact, not the transitive closure. No source fallback. |
| [Resolve.scala](../engine/src/histos/resolve/Resolve.scala), [DependencyLock.scala](../engine/src/histos/resolve/DependencyLock.scala) | Source artifacts are fetched/checksummed for two link modes. No complete recipe. `verify` materializes pins; `check`/`checkPlugins` freshly solve. Atomic write currently falls back to ordinary replacement. |
| [ResolutionService.scala](../engine/src/histos/resolve/ResolutionService.scala) | Unlocked results persist in `.histos/resolution`; locked drift fails. Module/tool/plugin paths are separate. `adopt` updates mode, lock, and memory via separate refs. |
| [Calls.scala](../app/src/histos/server/Calls.scala) | `resolveWork(Update)` loads plugins before planning replacement locks. An incompatible plugin can block its own repair. Update replaces module/tool sections rather than a bounded necessary closure. |
| [PluginModules.scala](../engine/src/histos/load/PluginModules.scala) | Temporary Scala-only compilation uses boundary/upstream directories, without external module jars, complete flags/resources, or persisted compile reuse. |
| [Plugins.scala](../api/src/histos/plugin/Plugins.scala), [WiringBuilt.scala](../api/src/histos/plugin/WiringBuilt.scala) | Activation leads with task plugins. Macro declarations expose `ZLayer[Nothing, Any, Plugins]`. `WiringBuilt.Failure` is unconstrained and `Loader.readBuilt` converts it with `toString`. |
| [Boundary.scala](../engine/src/histos/load/Boundary.scala), [BuildLoader.scala](../engine/src/histos/load/BuildLoader.scala) | One host Scala/ZIO/Izumi/API identity; engine packages refused. The handwritten export whitelist omits API packages such as theme/step, requiring focused export review. |
| [LocalPublish.scala](../engine/src/histos/publish/LocalPublish.scala) | `versionStamp` stamps modules named as plugin coordinates, not a publication contract. `sourcesOf` excludes generated replacements. Compile-only dependencies are intentionally absent from the normal POM. |
| [PackageKey.scala](../engine/src/histos/pack/PackageKey.scala), [ProductDigest.scala](../engine/src/histos/zinc/ProductDigest.scala), [ClassJar.scala](../engine/src/histos/pack/ClassJar.scala), [ScalacClasspath.scala](../engine/src/histos/zinc/ScalacClasspath.scala) | Package key uses semantic class products, excluding TASTy files and injected metadata that the jar includes. Substituted host API paths lack a corresponding artifact input hash. |
| [Protocol.scala](../wire/src/histos/server/Protocol.scala), [Build.scala](../app/src/histos/build/Build.scala) | No common invocation lock/CI/interaction envelope or structured lock offer/reply. Program stdin frames exist. Server environment/stdin cannot represent each caller. |

At the historical baseline, README Plugins was minimal, `docs/` was empty, and `docs/plugins.md` was absent. Actual tests live in `api/test`, `wire/test`, `engine/test`, and `app/test`. Relevant existing suites are `BspMetalsSpec`, `RunCallSpec`, `NativeLibraryClasspathSpec`, and `CellPulseSpec`. Comprehensive plugin-load, lock-transaction, and publication-rebuild suites are planned, not historical coverage.

## Goals and Non-Goals

### Goals

- Opt-in locks; unlocked builds solve automatically.
- Local locked offer/update/local/frozen strategies, complete necessary repair, atomic consent, and command resumption.
- Replaceable local/published ZLayer policies; typed main-build plugin configuration and module helpers.
- Complete default rebuildable publication and explicit binary-only mode.
- Host-bound transitive helper rebuilding, including frozen/offline with available pinned inputs.
- Correct service keys/error channels/scopes, compiler/task identity, and per-plugin provenance.
- Focused producer testing kit and durable proofs for discovered regressions.

### Non-goals

- Stable compatibility series/shims or an ABI oracle. Pre-release APIs can break.
- Speculative bootstrap modes, Scala 2/JS/Native plugin execution, or producer-build replay.
- Automatic source compilation of ordinary third-party binaries, undeclared compiler extensions, arbitrary processors/post-processing.
- A JVM sandbox for untrusted plugins.
- Unrelated test rewrites and standalone broader test-hardening passes. Relevant test cleanup/refactoring is permitted within implementation units under D16 when needed, with the reason explained and meaningful regression protection retained or improved.
- Unapproved external dependencies/compiler plugins or release work.
- Feature implementation under this documentation approval. Branch, commit, push, PR, merge, tag, publishing, and release actions require separate authorization.

## Key Decisions

### Confirmed owner decisions

| ID | Decision |
| --- | --- |
| D1 | Locking remains opt-in (`Projects.locked` or approved redesign); unlocked builds automatically solve. |
| D2 | Local locked declaration drift offers repair with proposed diff, atomically updates affected classpaths/necessary closure on acceptance, preserves unrelated entries, then resumes the original command. |
| D3 | Built-ins also support automatic update with notice, local solving without tracked-lock modification, and frozen. Configure strategies in the build. Strategies are extensible ZLayer services from local or published plugins. |
| D4 | Locked CI defaults frozen, with explicit configuration/invocation override possible and visible. Ordinary locked builds retain SNAPSHOT bytes until explicit refresh; unlocked refresh policy stays separate. |
| D5 | Plugin default is binary on exact API match, otherwise compile the published bundle against host API/boundary/runtime, announce it, and cache reliably. Loading strategy is configurable. |
| D6 | Preserve full exported API access and control engine internals. Host owns shared runtime identity. |
| D7 | Core defaults bootstrap a strategy and prerequisite closure; custom strategy governs remaining work. Bootstrap sequencing is a separate replaceable construction seam. Implement today's core-first mode only. |
| D8 | Publish validated complete rebuildable bundles by default; explicit binary-only allowed. Frozen CI can derive host-bound binaries from pinned inputs without lock writes/new solves. |
| D9 | Scope includes installation/wiring, typed configuration/module helpers, service composition, producer ergonomics, and focused test kit. Ordinary services, constructor injection, and ZLayers lead; tasks are additional capabilities. |
| D10 | Normal layout is core-safe `histos.plugins.scala`, then typed `histos.scala`; no required `histos.wiring.scala`. Coordinates make code available; explicit typed activation installs behavior. |
| D11 | Reviewed-locks examples are approved in principle, uncompiled. Keep `.withLocks(ReviewedLocks.layer(ReviewConfig(local = ReviewMode.Ask)))` and producer `.pluginLibrary`, automatically bound to producer host API and complete rebuild bundle. |
| D12 | Standalone architecture/handoff goes to `docs/plugin-architecture.md`. Docs-only approval authorizes document creation/checks, not feature implementation. |
| D13 | Core-safe bootstrap catalog can declare locking before plugins load. Main inherits it without duplicate opt-in; normal typed lock-strategy configuration stays in `histos.scala`. |
| D14 | Catalog marks early-strategy prerequisite closure. Optional `histos.strategies.scala` imports/wires services after core bootstrap and before remaining loading. Ordinary two-file layout remains. |
| D15 | Core defaults load that closure, typed early layer governs remaining loading, then typed main configures normal graph/services. Sequencing stays replaceable; unselected alternatives are not implementation work. |
| D16 | Each implementation unit may clean up/refactor relevant tests when implementation calls for it, explain the need, and retain or improve meaningful regression protection. Unrelated rewrites and standalone broader test-hardening remain separate scope. This owner clarification changes no architectural mechanism or proof gate. |

### Proposed mechanisms

The owner selected B1/B2 startup UX. `ResolutionIntent`, `BootstrapSource`, `HistosStrategies`, and the method shapes below are proposed mechanisms implementing it. A pinned recipe describes helper/stage DAGs. `BuildServices` is a typed construction plan with interface-keyed core replacements. A revisioned coordinator owns repair/consent/adoption. Exact API jar digest and conservative activation identity remain first-release choices, measured before optimization.

## Startup Decisions

The owner resolved B1/B2 during drafting. These decisions supersede the earlier main-only opt-in example. They are recorded here so a new architect does not reopen a settled bootstrap cycle.

### B1, resolved: core-safe opt-in, inherited by main

`histos.plugins.scala` can declare `Resolution.Lock` before loading plugins. The main build inherits it and normally uses `Projects(app)`, without repeating `Projects.locked`. Normal typed lock-strategy configuration remains in `histos.scala`.

Proposed model: a pure `ResolutionIntent` ADT records `Inherit` or `Select(Resolution)` on the declaration graph. Main omission is `Inherit`, not an explicit request for Solve. Loader normalizes the declared graph to an effective Resolution using the catalog intent before any graph resolution; execution services consume that normalized graph/snapshot. An explicit conflicting main intent fails `BootstrapError.ResolutionConflict`. With no plugin catalog, Inherit means Solve and a core-only main `Projects.locked` can choose Lock before any resolver call. A lock file's mere presence never selects intent.

A separate property/file would split configuration; forcing locked plugin builds would change opt-in. Both are rejected for this design. The initially approved `Projects.locked(app)` example remains a valid explicit assertion when it agrees, but repetition is not required.

### B2, resolved: optional early typed strategy file

The catalog marks the prerequisite closure for early strategy services. Optional `histos.strategies.scala` imports and wires those types after core bootstrap and before remaining plugin loading. Ordinary consumers keep only catalog and main. Advanced early-strategy users add this optional wiring source, not a required `histos.wiring.scala`.

Core defaults load the declared strategy closure. The typed early layer governs remaining loading; the fully typed main configures normal services/graph. Startup sequencing remains a replaceable service for future pre-release evolution; no unselected modes are implemented.

Late main `.services.withLocks` governs graph preparation. Loading configuration belongs in the early strategy file because main is too late to configure its own inputs. Precompiled configuration outside the build or partial main-source extraction is unnecessary. Core-safe defaults still govern the strategy's own first missing prerequisite.

**No blocking product decision remains.** Independent architecture review is complete; compiler/runtime proof is still required. Escalate implementation discoveries that would change owner-visible behavior rather than guessing. Late loading configuration must fail precisely rather than be ignored or silently deferred.

## Design

### Responsibility and Public Interface Map

| Responsibility | Proposed type/namespace | Interface |
| --- | --- | --- |
| Source declarations | `histos.plugin.HistosPlugins`, `PluginCatalog` | Core-only source/module/resolver data, early resolution intent, seed defaults, optional bootstrap source/prerequisites. |
| Bootstrap sequence | `histos.plugin.BootstrapSequencer` | `plan(catalog: BootstrapCatalog): Either[BootstrapPlanError, BootstrapSteps]`; host-injected construction seam. |
| Typed early policies | `HistosStrategies`, `StrategyServices` | Scope-owned interface-keyed policy layers after seed classes exist. |
| Lock strategy | `histos.lock.LockStrategy` | `decide(context: LockContext): IO[LockStrategyError, LockDecision]`. No file commits/generation ownership. |
| Loading strategy | `PluginLoadingStrategy` | `plan(context: PluginLoadContext): IO[PluginStrategyError, PluginLoadPlan]`. Cannot waive integrity/boundary constraints. |
| Main activation | `BuildServices`, `HistosBuild.services` | Typed additional-service/task layers and named core lock replacement. Graph stays `Either[BuildError, ProjectGraph]`. |
| Task capability | Existing `HistosPlugin`, `Contribution`, `RunRequest` | Commands/tasks/jobs/checks; services-only plugins need no dummy task plugin. |
| Infrastructure | Existing `histos.load/resolve/zinc/cache/publish` | Solve/fetch, compile/recipe/integrity/classloader/cache/scope, transaction. |
| Invocation | `app` and `wire` | Client context, structured offers/replies, generation leases, rendering/codecs. |

New textual IDs use opaque values with compile-time literals and typed runtime `from`: source ID, service key, lock revision, proposal ID, recipe node ID, generation ID. Policy/execution/interaction use enums, not string keys or booleans. API does not depend on engine resolver types or wire JSON. JSON decode errors are adapter-boundary text; domain parsers retain ADTs.

The catalog contract is `sources: PluginCatalog`, `resolution: Resolution` (default Solve), `resolvers: PluginResolvers` (default Central), `bootstrapLocks: LockDefaults`, and `bootstrap: BootstrapSource` (default Core). Catalog resolvers govern pre-main plugin/seed inputs; main-only resolver declarations arrive too late for those inputs and apply only to graph modules/tools. Freeze resolver identity with the declaration revision. Local source selection uses the host-matching declared Scala cell or one unambiguous declared Scala 3 JVM cell; otherwise fail a typed ambiguous-selection error. It selects source variants as data, then compiles with the host compiler. Host-owned local API/runtime inputs use verified HostProvided identity, not source-text detection or duplicate compilation.

### Exact First-Load Protocol

Preparation begins before daemon generation adoption. It does not execute the original command.

1. **Capture caller context.** Client supplies invocation ID, original `Call`, execution kind (`Local`/`Ci`), interaction (`Offers`/`Unattended`), overrides, offline selection, and client identity. Server never infers policy from its stdin/tty/env. MCP/BSP advertise client capabilities explicitly.
2. **Read revisions.** Snapshot catalog/bootstrap/main bytes, local source/resource trees, and raw lock revision. Hash bytes, not mtimes. Absent lock is an explicit revision; invalid syntax is a hard error.
3. **Compile core-safe catalog.** Compile with host boundary only, then open the candidate's shared BuildLoader with the catalog output directory and parent ApiBoundary. Select exactly one `HistosPlugins` entrypoint there and retain that same object/output through all later phases. Catalog provides complete available sources plus `BootstrapSource.Core` or `File(path, requiredCatalog)`. Seed roots must be a subset; infer full local/published prerequisite closure from declarations/recipes. Catalog classes/TASTy remain on later compiler classpaths so main can reference `plugins.helper` without reevaluating a second catalog object.
4. **Determine initial intent.** B1 catalog intent is authoritative before main. Seed `bootstrapLocks` defaults local offer/CI frozen; invocation override wins. Lock existence never chooses mode. With no catalog/plugin imports, compile core-only main graph first, before resolver calls, and synthesize this state.
5. **Prepare seed under core defaults.** Frozen materializes recorded binary/recipe/helper/compiler-stage inputs only; missing/drifted pins fail with zero fresh solves. Other modes may solve drift into an uncommitted candidate view. This is discovery, not tracked-lock update or consent. Announce that the strategy's own prerequisites use core defaults. Integrity failure stops.
6. **Compile/load seed closure.** Core loading uses exact binary or host rebuild. Local modules/prerequisites are declared entirely in catalog, not unevaluated main graph. Resolve external jars in candidate/frozen view and freeze selected source/resource bytes. No producer/main generator runs here.
7. **Compile typed early configuration.** Compile optional `histos.strategies.scala` against core, catalog output, and verified seed closure; append its output to the same loader and select one `HistosStrategies`. Build its layers once in candidate generation `Scope`. Plugin-defined `ReviewConfig` is now available on the first run. Imports outside `requiredCatalog` fail with missing-prerequisite diagnosis.
8. **Apply custom strategies.** Selected lock/loading services govern every remaining plugin and necessary closure before a new solve. Seed used core defaults as D7 requires. A strategy cannot recursively bootstrap itself.
9. **Complete verified shared classpath.** Validate all helper/runtime/ordinary dependencies; append verified URLs through an engine-only operation on the same generation loader. Never replace parent-owned classes or already loaded seed versions. Reject conflicting dependency versions/binary names with both origins; identical artifact hashes may deduplicate.
10. **Compile/evaluate main.** Compile main against catalog, optional strategy output, and all verified plugin classes, append main output, and select one HistosBuild in the same loader. All declared plugin imports/configuration/extensions work. Normalize inherited intent and check explicit conflicts. Main services inherit early policy instances. `.withLocks` deliberately replaces graph-phase policy. A main-only loading override is too late and is rejected.
11. **Discover complete necessary repair.** Main graph supplies requested module/test/tool cells and changes/consumer closure. Construct its lock capability and call its discovery decision before any new graph-phase solve. Frozen/Reject stops that phase without fresh resolution. No task/run/publish command has started. Service construction contributes capabilities, not command execution.
12. **Validate candidate.** Finish only policy-authorized solves/fetches, recipe/compiler/graph checks, and scoped activation under candidate snapshot. Collect full diff and evaluate acceptance using the phase-decision aggregation below. A final strategy can narrow permission, not waive earlier frozen/local/consent constraints. No lock is committed to make initialization succeed.
13. **Commit/adopt/select.** Offer/update follows transaction below. Local uses an ephemeral generation recorded in `.histos/resolution`, without tracked-lock writes. Frozen validates existing pins. Execute the saved command once on the selected generation.

Discovery may execute trusted Scala initialization, compiler macros, and layer setup. UI identifies new policy code. It never executes producer builds or task commands. No prior compiled build or provisional previous-run cache is required.

One shared dependency namespace means immutable recipes with incompatible class versions fail `PluginLoadError.ClasspathConflict`; they do not trigger an unbounded solve/reload loop. Changed declarations start a new preparation/generation.

### Policy Data and Error Channels

`LockContext` contains phase (`Seed`, `Plugins`, `Graph`), stage (`Discovery` or `Acceptance`), caller execution, early intent, current revision, declaration drift, necessary scope, and optional complete proposal summary. Call the phase policy before new solving and again with the complete proposal for acceptance. Seed is handled by core defaults. `LockDecision` is `UsePinned`, `OfferRepair`, `Update`, `LocalSolve`, `Frozen`, or `Reject(LockRefusal)`. `UsePinned` is valid only when the needed declarations and pins already agree.

Aggregate decisions only for phases requiring repair: any Reject or Frozen drift blocks; any LocalSolve makes the whole invocation ephemeral; otherwise any OfferRepair requires one consent for the complete diff; automatic commit requires all repaired phases to permit Update. Later configuration cannot auto-accept a seed that required consent. An explicit invocation override selects the built-in disposition in every phase before discovery, so `--lock=update` can deliberately auto-update the seed too. This prevents partial tracked/ephemeral transactions and preserves each earlier policy constraint.

Public `InvocationContext` contains only API data/capabilities, including a typed command identity, never wire `Call`. The original Call lives in the wire/app invocation envelope. This keeps API dependency direction intact.

`LockStrategyError` has typed configuration, adapter, and policy-failure variants. Plugin implementations map their own domain failures at the capability adapter to structured policy data, never `toString` or exceptions. `LockDefaults(local = Offer, ci = Frozen)` is core-safe; custom policies use plugin-defined typed configuration.

`PluginLoadContext` contains validated publication/binding/compiler/recipe/closure/offline facts. `PluginLoadPlan` is `UseExactBinary`, `Rebuild`, or `Reject(PluginRefusal)`. Built-ins are exact-or-rebuild (default), rebuild-always, and exact-binary-only. Engine refuses invalid plans. Unchecked compatible-binary reuse is not a first-release feature.

`BootstrapSteps` is today's core-first seed/optional-source/remaining-roots plan. The host constructs `BootstrapSequencer` before project loading. Replacing that seam does not let project code run before itself. No speculative future mode enum or compatibility adapter.

### Service Typing, Keys, Composition, and Scopes

Add `def services: BuildServices = BuildServices.default` to `HistosBuild`.

`BuildServices` packages an associated-type plan: `type Extra`, `type Error <: ServiceSetupError`. Concrete `Plan[S,E]` retains inference through fluent calls. Host-facing activation returns core values and task instances, not an erased user-service environment.

- `GenerationAmbient = BuildContext & PluginEvents.Generation`. BuildContext contains stable generation/repository/declaration/host data only. Generation constructors receive no command identity, caller, CI/local selection, consent channel, or invocation sink. Engine/Coursier/raw env/stdin are not ambient plugin inputs.
- `CoreServices = LockStrategy & PluginLoadingStrategy`, inherited from early policy construction.
- `ServiceSetupError` is an open error capability with `def diagnosis: histos.doctor.Noted`. Plugin error enums implement it, without extending `Exception`.
- Setup errors stay typed until loader maps them to `ServiceActivationError.Setup(Noted)` at the adapter boundary. Structured diagnostics travel to clients; no `Either[String,_]`, generic `Task`, or private error enum reconstruction is required.

| Operation on `Plan[S,E]` | Proposed concrete contract |
| --- | --- |
| `BuildServices.default` | `Plan[NoExtraServices, Nothing]`, with explicit empty marker and genuinely infallible default. |
| `.withLocks` | `def withLocks[E1 <: ServiceSetupError](layer: ZLayer[GenerationAmbient,E1,LockStrategy]): Plan[S,E \| E1]`; replaces one named core slot. |
| `.add[A]` | `inline def add[A](using Tag[A]): AddPartiallyApplied[S, E, A]`. The partially applied `apply` takes the layer. |
| `.tasks[P]` | `def tasks[P <: HistosPlugin: Tag]: TasksPartiallyApplied[S,E,P]`; same typed layer contract returning `Plan[S & P,E \| E1]`, exporting `P` and registering that exact instance as task capability. |
| `Plan.activate` | `ZIO[GenerationAmbient & CoreServices & Scope, ServiceActivationError \| E, ActivatedBuildServices]`. It stays in candidate generation scope. |
| `StrategyServices.withLocks` | Same interface-keyed input/error contract before remaining plugins load. |
| `StrategyServices.withPluginLoading` | Same interface-keyed contract for early loading strategy. No late main equivalent pretending to control initial loading. |

`Any` in `ZLayer[Any,Nothing,LockStrategy]` intentionally means no inputs; `Nothing` means no construction failures. These producer layers conform to a builder expecting named ambient inputs. They are not erased outputs/errors.

Register under capability interface `Tag`: `ZLayer.succeed[LockStrategy](ReviewedLocks(config))`. A concrete output key is not the registration contract. A concrete layer can explicitly project with `.project[LockStrategy](reviewed => reviewed)`; the layer pins lambda input already. Do not demand lambda/result ascription from users.

Builder registration also normalizes each output under the requested `Tag[A]` or named core-interface Tag by projecting its typed output to a fresh `ZEnvironment[A]`. Scala covariance of a layer is not proof of its original runtime registration key. Validate malformed compiled output with total `getDynamic[A]` and map absence to MissingCapability at the entrypoint adapter, never an unchecked cast/get defect. Extra registration is one capability key at a time; reject composite intersection keys. Multi-service producers compose ordinary layers and explicitly project/register their public capabilities.

`.add[LockStrategy]` conflicting with core is rejected; use `.withLocks`. Two `.add[ReviewLabel]` registrations fail `DuplicateService` before any layers construct, rather than ZEnvironment overwrite. Repeated `.withLocks` in one plan is duplicate replacement, not last-writer-wins. Existing duplicate plugin ID/task/command/job checks remain typed errors.

Layers consume the current builder environment. An absent required `ReviewLabel` is a compiler error. Malformed/bootstrap entrypoint missing core exports yields `MissingCapability(key, phase)` before dynamic lookup. No public unchecked dynamic service registry.

Implementation retains typed `ZEnvironment[S]`, `EnvironmentTag[S]` evidence, and selector closures inside associated-type results; successive layers build under the same generation `Scope` and combine via standard `ZEnvironment.union`/`ZLayer.build`. Reduce to core values and task instances at host boundary. No casts/Unsafe/object maps. P1/P2 compiler-proves the contract before registration implementation. Base builders need no macro. Registered services construct once per generation; shared acquisitions should enter as one producer service consumed by downstream projection/task layers, not repeated independent layer graphs with assumed cross-build memoization.

### Generation Construction Versus Invocation Execution (AR2)

Retain service construction once per generation, but pass caller context explicitly for each use:

- LockContext and PluginLoadContext carry this call's InvocationContext plus an invocation event handle. Their existing IO method signatures remain valid; policies choose local/CI behavior from that method argument, not a constructor-captured initiating caller.
- Extend RunRequest with `invocation: InvocationContext` and `events: PluginEvents.Invocation`. HistosPlugin.run keeps its Scope/PluginScopes contract. Other service methods needing caller data take an explicit request/context argument in the same shape. No global current invocation, FiberRef accessor, mutable environment swap, or implicit sink rebinding.
- `PluginEvents.Generation.emit(event: GenerationPluginEvent): IO[PluginEventError, Unit]` emits generation-tagged construction/lifecycle diagnostics to a host-owned generation topic. Its event schema has no invocation/caller payload; per-request data belongs on the invocation channel. It is not the first client's Say/transport. Coordinator subscribes/replays those diagnostics explicitly to interested leases, labeled as generation events. Background generation work can use that channel without claiming an invocation owner.
- `PluginEvents.Invocation` is a request-owned capability with `emit(event): IO[PluginEventError, Unit]`. The coordinator creates a separate immutable handle bound to `(GenerationId, InvocationId, ClientId)` for each invocation scope. It writes only that invocation's queue/transport. Closing the scope closes the handle; later writes fail typed. A request cannot redirect another handle, and no event routing depends on daemon stdin or environment.

Candidate preparation itself has an invocation handle, passed to loading/lock method requests. The service being constructed may emit a generation diagnostic but cannot retain that preparation handle through GenerationAmbient. The host decides how preparation events and generation diagnostics appear in the preparing client's transcript.

Compatible committed generations are reusable across different commands/callers and local/CI policy selections. Validate each new invocation against its own policy; generation identity depends on declarations, host, configuration, and resolution selection, not an initiating command or client. Ephemeral and committed snapshots remain distinct so frozen callers cannot inherit local solving.

**Lifetime:** Build seed/main services once per candidate generation. Do not construct inside a short-lived inner `ZIO.scoped` and retain the values, as current `Loader.readBuilt` risks. Failed/discarded candidate closes resources; adopted generation stays alive through invocations/watch; retired generation closes after last lease. Task child scope/PluginScopes and invocation event handle close on every outcome. Use acquireRelease and supervised/scoped fibers. Clock comes from effect environment, never ClockLive/test real-time substitution.

**Gate:** P2 compiler fixtures reject a generation layer requiring InvocationContext or PluginEvents.Invocation. P2/P8 run two different commands from different callers concurrently through one retained generation, using deterministic Promises: construction count stays one, each service method observes its own command/execution kind, and each client receives only its own invocation-tagged events. P8 proves this across actual coordinator/daemon leases and disconnect cleanup.

### Foundation Contracts and Parallel Landing Gates (AR5)

P1 lands these named, buildable contracts before parallel implementation:

| Contract and owner path | Required content and fixture seam |
| --- | --- |
| `SealedArtifactManifest` / `ArtifactIdentity`, new `api/src/histos/plugin/ArtifactManifest.scala` | Exact entry paths/blob hashes/lengths, canonical validation and identity; no semantic digest substitution. |
| `StageContract` / `StageInputs`, new `api/src/histos/plugin/StageContract.scala` | Known/producer-declared assurance, required bindings, frozen public file/env data, compiler constraints, offline admission. |
| `CompilationSnapshot` / `CompilationCapture`, new `wire/src/histos/zinc/CompilationSnapshot.scala` and existing `CompileFeed.scala` | Cell identity, frozen selected sources/resources/settings/stages, ordered dependency/compiler/runtime inputs, JDK identity, and sealed outputs. CompiledCell gains an explicit Unrecorded/Recorded capture state; an unrecorded cell cannot publish a rebuildable recipe. P5 produces real snapshots. |
| `HostCompilerManifest` / `HostCompilerInputs` / `HostCompilerSession` | Data/error contract in wire toolchain model, provider contract in `engine/src/histos/toolchain/HostCompilerInputs.scala`; complete ordered host tools, no Resolve requirement, scoped materialization result. |
| `ActivationManifest` / `VerifiedPluginManifest`, API provenance data plus `engine/src/histos/load/PluginManifests.scala` | Source/activation IDs, actual defining artifact, complete sealed helper/service closure, host binding, derived identities. Shared loader/runtime result shapes carry explicit Unverified/Verified provenance; P7 rejects Unverified rather than reconstructing empty artifact lists. P6 produces verified manifests. |
| Foundation contract fixtures, `engine/test/histos/contracts/ContractFixtures.scala` | Pure constructors/validators and tiny real blob fixtures for snapshots/provenance. P4/P5/P7 tests use these published fixture seams from the same engine test tree, not a sibling worktree's unpublished implementation. |

P1 also makes the necessary caller/result-shape adaptations so these contracts compile on its own branch. Transitional Unrecorded/Unverified states are explicit typed absence, not successful fake captures, compatibility promises, or permission for lossy publication/cache reuse.

**Development versus landing:** P2/P3/P4/P5 develop concurrently from landed P1. P4 tests its capture/seal/publish logic using P1 snapshots and asserts Unrecorded refusal, but its production PR lands only after P5 real snapshot capture lands and P4 rebases/runs a real publication check. P6/P7 develop concurrently from the declared P1/P2/P3/P4/P5 base using foundation provenance fixtures. P7 lands after P6's producer is available and reruns actual loader-to-task manifest proof. This preserves parallel coding with explicit producer-before-consumer landing gates, no dependency on an unpublished sibling branch, and the final P10 installed integration gate.

### Author-Facing Examples

Fences use portable Scala and text Markdown. The nine Scala fences from the locked catalog through the producer build are the strings `PluginBootstrapSpec` compiles in `the spec author fixtures compile`. [plugins.md](plugins.md) links to those strings and does not copy them. The `ReviewedLocksSpec` fence and the lock-repair transcript are still proposed text. They are not compiler fixtures, and the transcript is not a tracked file. They were not added. The landed `add` method is `inline def add[A](using Tag[A])`. The call sites in those strings are unchanged. Assembled-host proof is in the work plan. Open ledger rows stay proposed.

#### Normal locked consumer: two files

B1 adds early intent to the accepted catalog shape:

Planned fixture: `histos.plugins.scala`.

```scala
import histos.*
import histos.plugin.*

object plugins extends HistosPlugins:
  override def resolution = Resolution.Lock

  def sources =
    PluginCatalog(
      PluginSources.ship(dep"com.example::reviewed-locks:1.0.0")
    )
```

Planned consumer fixture: `histos.scala`.

```scala
import histos.*
import histos.plugin.*
import com.example.reviewed.*

object build extends HistosBuild:
  def graph =
    val app = module("app", Jvm)(scala"3.10.1").reviewed
    Projects(app)

  override def services =
    BuildServices.default
      .withLocks(ReviewedLocks.layer(ReviewConfig(local = ReviewMode.Ask)))
      .add[ReviewLabel](ReviewLabel.layer("reviewed"))
      .tasks[ReviewTasks](ReviewTasks.layer)
```

Approved `.withLocks` UX controls graph-phase policy here. Core defaults handle pre-main plugin loading. Omit `.add`/`.tasks` for a services-only policy plugin; merely declaring the coordinate installs no behavior.

#### Custom policies before a missing remaining plugin

Planned advanced consumer fixture: `histos.plugins.scala`.

```scala
import histos.*
import histos.lock.*
import histos.plugin.*

object plugins extends HistosPlugins:
  private val reviewed =
    PluginSources.ship(dep"com.example::reviewed-locks:1.0.0")

  override def resolution = Resolution.Lock
  override def bootstrapLocks =
    LockDefaults(local = LockMode.Offer, ci = LockMode.Frozen)

  def sources =
    PluginCatalog(reviewed, PluginSources.ship(dep"com.example::module-tools:1.0.0"))

  override def bootstrap =
    BootstrapSource.File(
      RepoPath("histos.strategies.scala"),
      required = PluginCatalog(reviewed),
    )
```

Planned fixture: `histos.strategies.scala`.

```scala
import histos.lock.*
import histos.plugin.*
import com.example.reviewed.*

object strategies extends HistosStrategies:
  def services =
    StrategyServices.default
      .withLocks(ReviewedLocks.layer(ReviewConfig(local = ReviewMode.Ask)))
      .withPluginLoading(PluginLoading.exactOrRebuild)
```

Main imports `com.example.moduletools.*` and uses `BuildServices.default` to inherit these instances, omitting `.withLocks` unless graph replacement is intentional. Core prepares reviewed-locks/prerequisites; typed bootstrap config constructs custom policies; custom locking sees missing module-tools before its first solve. This works on an empty machine under selected policy/network conditions. A custom loading provider is added to `required` and wired here identically.

#### Local strategy with helper and external jar

Planned local-strategy fixture: `histos.plugins.scala`.

```scala
import histos.*
import histos.plugin.*

object plugins extends HistosPlugins:
  val helper = module("review-core", Jvm)(scala"3.10.1")
    .in(ProjectPath("build-support/review-core"))
  val locks = module("reviewed-locks", Jvm)(scala"3.10.1")
    .in(ProjectPath("build-support/reviewed-locks"))
    .dependsOn(helper)
    .libraryDependencies(dep"org.scala-lang.modules::scala-parser-combinators:2.5.0")

  override def resolution = Resolution.Lock
  def sources = PluginCatalog(PluginSources.module(locks))
  override def bootstrap =
    BootstrapSource.File(
      RepoPath("histos.strategies.scala"),
      required = PluginCatalog(PluginSources.module(locks)),
    )
```

Catalog recursively includes helper; local compile sees host boundary and resolved/pinned parser-combinators closure. Optional bootstrap source constructs local ReviewedLocks normally. Main can reuse `plugins.helper`/`plugins.locks` for ordinary build/publication rather than redeclaring conflicting modules. No main-defined module is needed before main compilation.

#### Producer: ordinary services and typed module helper

Planned fixture: `reviewed-locks/src/com/example/reviewed/ReviewedLocks.scala`.

```scala
package com.example.reviewed

import histos.*
import histos.lock.*
import histos.plugin.*
import zio.{IO, ZIO, ZLayer}

enum ReviewMode:
  case Ask, Update, Local, Frozen

final case class ReviewConfig(
    local: ReviewMode = ReviewMode.Ask,
    ci: ReviewMode = ReviewMode.Frozen,
)

final class ReviewedLocks(config: ReviewConfig) extends LockStrategy:
  def decide(context: LockContext): IO[LockStrategyError, LockDecision] =
    val selected = context.execution match
      case ExecutionKind.Local => config.local
      case ExecutionKind.Ci    => config.ci
    ZIO.succeed:
      selected match
        case ReviewMode.Ask    => LockDecision.OfferRepair
        case ReviewMode.Update => LockDecision.Update
        case ReviewMode.Local  => LockDecision.LocalSolve
        case ReviewMode.Frozen => LockDecision.Frozen

object ReviewedLocks:
  def layer(config: ReviewConfig): ZLayer[Any, Nothing, LockStrategy] =
    ZLayer.succeed[LockStrategy](ReviewedLocks(config))

trait ReviewLabel:
  def value: String

final class LiveReviewLabel(val value: String) extends ReviewLabel

object ReviewLabel:
  def layer(value: String): ZLayer[Any, Nothing, ReviewLabel] =
    ZLayer.succeed[ReviewLabel](LiveReviewLabel(value))

extension [P](module: ModuleDef[P])
  def reviewed: ModuleDef[P] =
    module.note(PluginId("reviewed-locks"), NoteKey("review"), NoteText("required"))
```

Additional task capability in the same library:

Planned fixture: `reviewed-locks/src/com/example/reviewed/ReviewTasks.scala`.

```scala
package com.example.reviewed

import histos.plugin.*
import zio.{Scope, ZIO, ZLayer}

final class ReviewTasks(label: ReviewLabel) extends HistosPlugin:
  val id = PluginId("reviewed-locks")
  private val describe = TaskName("describe")

  def contribute =
    Contribution(
      tasks = List(
        DeclaredTask(
          name = describe,
          seal = Seal.Hermetic,
          needs = Set.empty,
          inputs = List(DeclaredInput.Text(InputKey("label"), label.value)),
          outputs = Nil,
          dependsOn = Nil,
        )
      ),
      commands = Nil,
      jobs = Nil,
    )

  def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
    if request.id == PluginTaskId(id, describe) then
      ZIO.succeed(PluginOutput(label.value, Nil))
    else
      ZIO.fail(PluginRunError.Unknown(request.id))

object ReviewTasks:
  val layer: ZLayer[ReviewLabel, Nothing, ReviewTasks] =
    ZLayer.fromFunction(ReviewTasks(_))
```

Task input includes its constructor value because that changes output. Resourceful services use scoped layers/acquireRelease and error enums extending `ServiceSetupError`; no Task/string escape. Plugins can supply services only or both services/tasks.

Typed setup-error variant, replacing only the label layer at the consumer:

Planned fixture: `reviewed-locks/src/com/example/reviewed/CheckedReviewLabel.scala`.

```scala
package com.example.reviewed

import histos.doctor.Noted
import histos.plugin.*
import zio.{ZIO, ZLayer}

enum LabelSetupError extends ServiceSetupError:
  case Empty

  def diagnosis: Noted = this match
    case Empty =>
      Noted.error(
        "review.config",
        "Review label is empty",
        "A nonempty label is required.",
        Noted.steps("Set the label in histos.scala."),
      )

object CheckedReviewLabel:
  def layer(value: String): ZLayer[Any, LabelSetupError, ReviewLabel] =
    ZLayer.fromZIO:
      if value.isBlank then ZIO.fail(LabelSetupError.Empty)
      else ZIO.succeed[ReviewLabel](LiveReviewLabel(value))
```

`.add[ReviewLabel](CheckedReviewLabel.layer("reviewed"))` infers LabelSetupError without an ascription. Empty label fails before execution and retains the old lock/generation, with the structured diagnosis at the loader adapter. `BuildServices.default.tasks[ReviewTasks](ReviewTasks.layer)` fails compilation because ReviewLabel is absent. Adding ReviewLabel twice fails DuplicateService before either layer executes. P2 retains these exact positive/negative fixtures.

Producer publication fixture:

Planned producer fixture: `histos.scala`.

```scala
import histos.*

object build extends HistosBuild:
  def graph =
    val locks = module("reviewed-locks", Jvm)(scala"3.10.1")
      .artifact(artifact"reviewed-locks")
      .pluginLibrary

    Projects.organization(organization"com.example")(
      ships(Ship("reviewed-locks").next(1, 0, 0)(locks)),
      locks,
    )
```

`.pluginLibrary` supplies producing host API/runtime compile binding and complete rebuild publication, independent of consuming the module as a plugin. Alternative `.pluginLibrary(PluginPublication.BinaryOnly)` is explicit. No manual API stamp or self-registration in `Projects.plugins`.

The consumer `1.0.0` coordinate preserves agreed UX. Executable integration publishes a local/snapshot coordinate and substitutes only that coordinate. No non-snapshot release is verification work.

### Layer Graph and Resource Ownership

Host graph: platform adapters (`Resolve`, `ContentStore`, compiler, workspace/JDK, event transport) plus core policy/`BootstrapSequencer` layers construct Loader by injection. `LockCoordinator` receives preparer, revisioned LockStore, generation manager, and client offer transport. CLI/daemon/MCP/BSP/watch/native clients enter that coordinator with the same envelope.

Per generation: `host boundary + seed pins -> compiler/cache -> shared loader -> StrategyServices -> remaining plugin compiler/cache -> main -> BuildServices -> validated graph/tasks + ResolutionSnapshot`.

Scopes own every resource-bearing stage. Candidate stays unpublished until selected/committed. Invocation leases capture one generation. Existing daemon `adopt`, `loadInto`, `acquire`, `release`, `retire` are the lifecycle starting point, not a competing generation subsystem.

## Rebuild Publication and Compiler Model

### Published Artifact

Embed canonical versioned metadata and rebuild payload in the ordinary binary jar:

- `META-INF/histos/plugin.json`: `PluginDescriptor`, producer API/runtime/compiler binding, mode, root node, helper/stage DAG, entry digests, ordinary dependency descriptors.
- `META-INF/histos/rebuild/...`: normalized Scala/Java/resource payloads for owned rebuildable nodes.
- Retain old API digest resource only while old loader is staged, then retire redundant legacy semantics. This is not a permanent compatibility shim.

The binary jar checksum pins all embedded bytes. Conventional source jar remains useful for IDEs but never authorizes fallback. This uses existing jar publication rather than assuming arbitrary Maven classifier upload support. External helper bundles and ordinary jars are separately pinned. Validate complete staged descriptor/bundle before any upload. Capture source/resource/settings manifests from the actual compilation snapshot, then verify current bytes still match before staging; changed inputs fail publication rather than pair new sources with old products. Normalize archive ordering/timestamps and reject ambiguous duplicate payload names.

Rebuildable is default; binary-only retains binding metadata with no rebuild claim. Consumer exact-only policy is different from producer binary-only availability. Mismatch with required missing recipes fails typed.

### Supported Recipe v1

A finite cycle-checked `RebuildRecipe` DAG contains nodes with:

1. Stable ID/role and ordered prerequisites: root, Histos-linked helper, compile-time stage.
2. Exact selected main Scala AND Java input bytes, including generated/unmanaged replacement semantics from `Sources.prefer`; normalized paths/hashes and main/test separation.
3. Effective unmanaged/generated resources after replacement rules, including service descriptors; bytes/ownership/hash. Regenerate host metadata in derived binary.
4. Semantic scalac/javac options, encoding, release/target, compilation order, source-selection record, language/macro settings, compiler-plugin options. Output/diagnostic paths are controlled placeholders, not producer absolute paths.
5. Ordered full compile closure, including compile-only/provided inputs, exclusions/evictions, and separate runtime roles. Recipe is rebuild authority; normal POM remains normal consumer authority.
6. Frozen compiler plugins/macro prerequisites as exact artifacts or earlier recipe stages. Stage code compiles before consumers; copying sources into one compilation is not equivalent.
7. Exact host-substitution roles for API, Scala runtime, ZIO/stacktracer, Izumi; match full coordinates/validated metadata, not permissive artifact-name prefix.
8. Producer compiler/Scala/runtime/JDK binding, recipe version, supported host constraints, and materialized input hashes.

Freeze generator outputs at publication. Consumer never executes producer build, generators, shell commands, dependency helper methods, or source-layout selection. Compile-time macros/plugins execute only as supported trusted compiler stages with complete input identity.

Initial support is Scala 3 JVM library nodes, representable semantic settings, normal Zinc mixed Scala/Java compile, frozen resources, helper DAGs, and explicit macro stages. Generated/Java/resource support is mandatory v1. Compiler plugins require recorded compatibility with selected compiler or a supported rebuildable stage; frozen never solves another plugin coordinate.

Unknown flags, annotation processor execution, missing helper bytecode/source/stages, absolute paths, cycles, arbitrary post-processing, or incomplete semantics fail `RecipeError.Unsupported(feature,node)`/`Incomplete(input,node)`. Rebuildable publication refuses before uploading. Author may deliberately choose binary-only. Unrepresentable semantics require typed compile-model extension, not log/POM inference or silently stripped flags.

### Compiler-Stage Support and Ambient Inputs (AR3)

Pinning executable stage bytes does not prove reproducibility. Bytecode/TASTy inspection identifies stages and linkage; it cannot infer arbitrary file, environment, clock, cwd, random, or network effects. Recipe v1 admits two explicit contracts:

- **HostKnown:** an exact stage implementation matched to the host-owned KnownStageRegistry by artifact/entrypoint hash and adapter version. Initial registry covers the supported Scala compiler's standard compile-time operations, host ZIO Trace/layer macros, Izumi Tag derivation, and Histos literals/DSL macros. Their reviewed contracts use declared sources/types/settings and canonical source labels, with no external IO, clock, randomness, or network. Source-position/Trace output is an input-sensitive feature, not an ambient exception. A new library version is not admitted by package-name prefix; its exact implementation/adapter needs review and a fixture.
- **ProducerDeclared:** explicit StageContract metadata on every non-host macro/compiler-plugin stage and its helper closure. Producer supplies it through typed plugin-stage recipe declarations; an unrecognized stage without a complete contract fails UnsupportedStageContract before rebuildable upload. The contract is a producer assertion of complete inputs and deterministic behavior, not a proof from static scanning. Its identity and assurance are recorded in provenance.

StageContract v1 names entrypoints/helpers, compiler requirements, required input-binding keys from the stage descriptor, and a complete StageInputs manifest. File inputs are normalized relative paths with bytes frozen into the rebuild bundle; directory inputs are complete file-entry manifests. Public, nonsecret environment bindings are captured as exact values including explicit absence, frozen with the compilation snapshot, and included in the recipe/key. Declared environment inputs are not silently reread from the consuming client. Published fallback uses the frozen producer values; local plugin compilation captures its declared current inputs and revalidates them before storage. Producer metadata must classify bindings as Public or Secret; unclassified or Secret inputs yield UnsupportedStageInput. The tool cannot infer secret content from an arbitrary string, so dishonest Public classification is a producer trust violation, not a secrecy guarantee.

The contract permits cwd only as the root for declared relative inputs. It prohibits embedding absolute cwd/temp paths, reading undeclared files/env, time/randomness, subprocess execution, and network. Every stage must declare that restriction explicitly; there is no v1 network-enabled stage mode. Changed declared file/env values produce different snapshot/recipe/key identity even when executable stage bytes are unchanged.

P5 executes stageful recipe compilation in a scope-owned worker JVM using existing child-process infrastructure. Worker gets the verified HostCompilerSession, recipe/stage closure, immutable StageInputs, and compiler arguments with canonical relative source labels. Its cwd is the private materialized recipe root. Its environment is cleared and rebuilt from declared public bindings plus a fixed, hashed worker bootstrap set; no server/client HOME, CI, PATH, or arbitrary env inheritance. API/runtime loaders and diagnostic/output channels are supplied explicitly. Do not mutate daemon cwd, System properties, or process environment to configure concurrent compiles. Worker interruption terminates the supervised child and closes temporary input/output resources. This isolation normalizes inputs; it is not an OS sandbox.

Producer plugin-library compilation uses the same captured StageInputs/normalized worker contract, with its declared library compiler/toolset from the producing build's normal resolution. Fallback uses HostCompilerSession instead. Capture stage bindings before the worker starts; seal the exact consumed input-manifest identity with outputs afterward and revalidate before publication. Do not run producer stages under arbitrary daemon env and later describe a different frozen environment as their input. Ordinary application cells outside this plugin-publication concern keep their existing compilation policy.

Because there is no approved sandbox, a dishonest ProducerDeclared stage can still violate its assertion by accessing outside the root or opening sockets. HostKnown admission relies on reviewed code; ProducerDeclared admission relies on trusted producer metadata/code. Neither scanning nor child-process isolation claims to prove arbitrary stage hermeticity. Unknown or incomplete contracts are typed unsupported, never silently cached as complete.

**Offline rule:** artifact fetching/solving never uses the network. Stageful plugin compilation (producer or fallback) in offline mode admits HostKnown stages only. ProducerDeclared stages fail UnsupportedOfflineStage before worker startup, even if their metadata asserts no network, because v1 cannot enforce arbitrary stage effects. This is an explicit source-fallback support restriction; an already valid binary can still be reused. Ordinary Scala/ZIO/Histos macro recipes require no author declaration and remain supported offline under their known contracts.

**Gate:** P4/P5 retain one unchanged producer-declared macro that reads a declared file and public env binding. Freeze values A/B in separate snapshots, rebuild online, and assert changed complete key plus decisive output; omission of a required binding listed in that stage descriptor is a typed publication refusal. Detecting an undeclared effect concealed by a dishonest descriptor is outside that proof. For a published recipe, changing caller env must not alter the frozen worker input/output. Offline rejects the declared stage before worker start; the fixture's forbidden network probe is never invoked. A HostKnown-only mixed recipe succeeds offline, with zero download/solve calls. P5 also verifies known source-label output is stable across different scratch roots. No compiler/runtime proof has run for these proposed gates.

### Dependency Classification and Closure

- **HostProvided:** exactly one current API/Scala/ZIO/Izumi universe, substituted by role.
- **HistosLinked:** links API or Histos-linked helper, requiring metadata/recipe or explicit binary-only contract. Rebuild necessary helper DAG on drift and replace compile AND runtime paths.
- **OrdinaryBinary:** no boundary linkage; retain exact locked bytes. No automatic source rebuilding claim.
- **CompilerStage:** executable macro/compiler-plugin input with explicit dependencies/compatibility. Body changes invalidate consumers despite unchanged public API.

Publication scans closure bytecode/metadata plus Scala TASTy/inlined/macro requirements. Unmarked dependency referencing Histos API is unsupported linked helper, not ordinary binary. A binary-only mismatched helper can block rebuildable root with path-specific error. Validate every node, not only root stamp.

Owned local helper nodes can be embedded; separately published helper bundles are referenced by exact coordinate/hash. Either form must close the DAG. An unpublished helper cannot disappear. Consumer never reruns the declaration methods that originally produced dependency data.

### Compiler Identities and TASTy Direction

Producer library compiler is the module's declared compiler. `.pluginLibrary` supplies host API/runtime inputs, without silently changing Scala version. Validate that compiler can consume producing host API and products fit intended host; unsupported combinations fail before publication.

The executing host owns build-file compiler selection. Catalog/strategy/main and fallback use that verified host compiler identity; mixed-language fallback consumes the original tool artifacts/session described next, not a reconstructed project compiler request. Fingerprint compiler jars, boundary, and JDK/runtime. Ordinary application modules keep their declared toolchains; plugin rebuilding does not change application Scala.

### Host-Owned Compiler Bootstrap Contract (AR1)

The existing module path calls `Scala3.jars` and `CellTools.gather` through LockedTools. `CompilerRequests.compiler` requests scala3-compiler_3, scala3-sbt-bridge, their transitive closure, and the currently supported JLine roots. `BuildFile.drive` instead invokes embedded Dotty directly. Neither path establishes that a project pinned under host A contains host B's mixed Scala/Java compiler inputs.

Choose distribution packaging of the existing supported tools, with one source of selection independent of the project:

1. **Select at host production.** P10 obtains the launcher cell's exact compiler version and materialized `CompilerRequests.compiler` closure from the host producer's compilation snapshot. It includes the compiler, matching bridge, compiler interfaces/support (including currently requested JLine inputs), and Scala runtime. Record ordered roles, coordinates, byte hashes, loader-boundary format, supported JDK/release constraints, and recipe/compiler implementation format in `HostCompilerManifest`. Do not resolve from a consumer catalog, strategy, or project lock.
2. **Package before distribution.** Put the manifest and exact original split jars under `bootstrap/compiler/` in the host distribution jar. Do not treat a fat-jar code-source location as interchangeable with an original bridge/compiler jar. The distribution's existing outer checksum pins this payload. Compiler selection has one manifest entry, not a search through whatever jars happen to be installed. Embedded build-file Dotty's reported version must agree with the selected compiler version; the executed host identity and manifest digest participate in keys.
3. **Validate before project bootstrap.** Host construction opens the installed executing distribution, verifies manifest/schema/version, required unique roles, entry hashes, and the executing host JDK identity/constraints. The executing host's externally provisioned JDK, including javac, is an explicit stable host input; Histos does not install or solve a JDK. A mismatch fails before the strategy is needed.
4. **Materialize without solving.** HostCompilerInputs is constructed from InstalledHost, ContentStore, and HostJdk, with a typed HostCompilerError layer; no project service is required. `materialize: ZIO[Scope, HostCompilerError, HostCompilerSession]` reads only the checked distribution payload and verified local store. Session supplies ordered CompilerArtifacts, runtime/boundary paths, exact bridge/support inputs, checked JDK, KnownStageRegistry, and complete fingerprints. It has no Resolve or LockedTools input and no network fallback. Extract immutable jars by digest, verify reused blobs, and lease them for the host/session lifetime. Open compiler/bridge worker loaders under Scope; close them on interruption or host retirement. Do not delete shared content-store blobs when a lease ends.
5. **Compile using supplied artifacts.** P5 adds a plugin-specific preparation route accepting HostCompilerSession's compiler/runtime/bridge/support paths and fingerprints directly into the existing mixed Scala/Java Zinc machinery. It bypasses `CellTools.gather` and `Scala3.jars` project-tool requests. Project pins supply recipe/library/stage dependencies only. Catalog/strategy/main compilation uses the verified host compiler identity; no alternate compiler is selected by a policy plugin.
6. **Report typed failure.** `HostCompilerError` distinguishes absent manifest/entry, incompatible manifest or compiler version, conflicting role, bad digest, unsupported JDK, and extraction/worker IO failure. Loader nests it as a host-bootstrap failure. No incomplete-bundle repair through the project resolver, project lock, or downloaded dependency is permitted.

P1 owns manifest/session contracts; P5 owns validation/materialization and compiler consumption; P10 owns production capture, distribution assembly, installation validation, and bootstrap adoption. Packaging reuses artifacts already requested by the supported compiler toolchain. No new dependency is requested. If implementation requires an additional coordinate or compiler plugin beyond that closure, stop and ask Russ for approval before changing dependencies.

**Decisive gate:** P5/P6 use two genuinely different supported compiler bundles with a valid mixed Scala+Java recipe produced under A, a host B API/worker, and a project lock containing no B compiler cell. Clear derived plugin output, rebuild, and assert executable Scala-to-Java output, zero `resolve`/`resolveTool`/`resolvePlugin` calls, and identical lock bytes. Missing bridge fails typed with the same zero-solver assertion. P10 repeats through an assembled installed B distribution with cleared derived cache. Provider fixtures may materialize existing pinned tool artifacts before the measured phase; runtime fallback may not solve them.

Scala 3 binary-series suffix is not proof older compiler reads newer TASTy. Validate direction for selected binaries, stages, and ordinary Scala deps. Unsupported pinned ordinary dependency/compiler plugin yields `CompilerIncompatible` with path and producer/host versions. Root source compilation cannot repair an ordinary binary. Newer language syntax may still fail with typed compiler diagnostics under older host.

Exact API reuse additionally requires valid whole closure/runtime/TASTy. It cannot introduce a second runtime. Rebuild supported nodes or fail; binary-only never means unchecked loading.

## Data, Lock, Cache, and Provenance Model

### Immutable Inputs Versus Derived Outputs

Version lock syntax explicitly. Deterministic records pin plugin binary/embedded descriptor/payload, helper bundles, ordinary compile/runtime deps, stages/tools, optional source jars, classifiers, origin, digests, declaration fingerprints, substitution roles.

For SNAPSHOTs, pin immutable physical/timestamped locator plus logical revision/checksum. Missing retained artifact cannot be replaced by newer bytes at same logical coordinate. Online fetch may recover same pinned bytes; different checksum is integrity failure.

Tracked lock never contains derived host jars, machine paths, generation IDs, or compile cache keys. Host upgrade can derive a binary without changing lock. Local source bytes are declaration/compiler inputs; frozen does not freeze uncommitted local code, but validates external pins/source shape and can compile current local source.

Read legacy unversioned locks during migration, but do not pretend they contain recipes. Valid exact binaries can load. Rebuild needing missing metadata fails `MissingRebuildBundle`; explicit repair/migration pins it. Frozen cannot solve/migrate/write. Unknown/corrupt format is a typed hard error.

### Resolution Snapshot and Invalidation

One immutable `ResolutionSnapshot(intent, lockRevision, declarations, cells, provenance, selection, generation)` lives in one synchronized Ref/STM state. Selection distinguishes committed/ephemeral. Leases capture snapshot; memo keys include relevant cell revision and ordered declarations.

Commit atomically publishes state, rekeys/invalidates affected classpaths/consumers, and retires changed plugin loaders. Old solve cannot insert into new revision. Unrelated content caches remain reusable. Do not update mode/lock/memory independently as current `adopt` does.

### Sealed Artifact-Entry Identity (AR4)

Artifact identity is distinct from the existing semantic ProductDigest. `ProductDigest.ofDirectory` includes only normalized .class files and removes the TASTY attribute; ClassJar packages every regular file, including .tasty and resources. Never use that semantic digest as the complete package identity or compiler-visible helper/stage output identity.

`SealedArtifactManifest` contains an ordered entry list of normalized relative path, exact byte length, and content digest/blob reference for **every** packaged .class, .tasty, resource, and injected binding/descriptor/rebuild entry. Hash raw bytes, including .class TASTY attributes. Its versioned canonical digest plus publication/archive format is ArtifactIdentity. Duplicate/escaped entries are errors. The manifest is stored beside the content-store result, not embedded as a self-hashing entry in the artifact it describes. The lock separately pins the final jar's exact checksum.

Sealing sequence: freeze compiler outputs/resources, materialize all injected metadata, enumerate the actual final entry set, copy/hash each immutable entry, then derive package key and pack that set with deterministic archive settings. ClassJar's new manifest-based route cannot independently walk a changing classes directory after the key was computed. Revalidate/copy-to-blob failure yields InputChanged, not a successful cached artifact. Store both final archive hash and entry manifest.

Use the same complete seal for derived plugin/helper/stage outputs, including class directories used on later compiler paths. Consumers hash ArtifactIdentity, not only APIs or semantic products. Metadata regeneration is part of the seal. Binary-only packages still hash TASTy and every injected entry despite having no source payload. Ordinary semantic/API digest policy remains unchanged outside these plugin/package seams.

**Gate:** P4 first reproduces a package false hit for two valid outputs with equal ProductDigest but different TASTy, including binary-only mode. Separately assert an exact .tasty entry-byte change changes manifest/package key without trying to compile corrupted TASTy. P5 uses a valid helper whose erased class products stay equivalent while `type Label = "aa"` changes to `"bb"`; a consumer `valueOf[Helper.Label]` must recompile and return the new value. Generate both helper artifacts with a supported compiler and assert semantic equality as a fixture precondition rather than assume arbitrary inline bodies erase equally. P5 proves compiler-visible seal changes invalidate even when semantic/API-only identity would miss them.

### Full Host-Bound Compile Key

Versioned canonical key hashes:

- Recipe/embedded/referring artifact bytes and selected Scala/Java/generated entry names/bytes.
- Resources/replacement rules and ordered semantic settings/stage DAG.
- Complete ordered compile/runtime/helper closure, ordinary jar checksums, stage bodies/contracts/frozen file-env inputs, and sealed ArtifactIdentity of every derived helper/stage output. Semantic ProductDigest or macro helper API digest alone is insufficient.
- Host API, boundary/runtime jars, compiler/TASTy support identity, JDK identity/release target, compiler plugins.
- Boundary export manifest and compile/package implementation format version.

Exclude absolute checkout/temp paths, timestamps, and credentials. Hash canonical source labels and the admitted StageContract/StageInputs/worker bootstrap settings. The complete-key claim covers the declared inputs of supported stage contracts, not effects concealed by code violating its producer assertion. Unsupported/unknown contracts refuse publication or fallback as specified above.

Compile in scoped staging directory. Rehash mutable inputs before store; only complete validated outputs/provenance are successful cache entries. Failure/interruption never publishes a hit. Verified immutable blobs and supervised per-key single-flight avoid corrupt/duplicate work. Second identical load must call compiler zero times.

Package key derives from the sealed actual entry manifest plus mode/archive format, never ProductDigest.ofDirectory. Fix touched library/assembly callers, not unrelated semantic digest policy. Ordinary compile also hashes actual API substitution in `ScalacClasspath`/`CompileInputs`.

### Plugin and Task Provenance

Replace global `Loader.linksOf` classification/empty artifacts with per-source manifest and per-activation origin: source ID, defining class origin, mode, published/recipe hashes, selected helper/runtime closure, producer/running binding, derived key/output hash. Mixed local/published roots remain distinct. Helpers/services constructing plugin instances participate in identity.

Task key includes plugin/task ID, declaration, invocation args/flags, declared inputs, relevant module/output identity, complete defining plugin closure, host binding, service/configuration identity. Conservatively include catalog/bootstrap/main byte hashes and all activated service provenance in v1. Explicit activation/task inputs cover read files/env. Hash secrets, never print them. Undeclared external state makes an action open/non-hermetic.

Changed plugin bytes, macro helper body, bootstrap config, service jar, or args cannot reuse stale output. Unrelated plugin changes may conservatively invalidate initially; narrow only after measured proof.

## Lock Transaction and Invocation UX

### Modes, Flags, and Precedence

Separate Resolution opt-in from LockMode behavior. Proposed common flag: `--lock=offer|update|local|frozen`, carried in envelope. It overrides phase strategy selection for this invocation, does not enable locking on an unlocked build or weaken integrity. Reject on commands without dependency preparation; never silently ignore.

Precedence:

1. Explicit common `--lock` override selects the built-in disposition in every phase and displays its origin.
2. Explicit resolve action supplies its disposition when no common override is present: update means Update, refresh means Offer; validate/check never mutate.
3. Explicit phase strategy/configuration: early strategy for remaining plugins; main `.withLocks` for graph. Explicit CI config is allowed.
4. Core defaults local offer/CI frozen.

Reject `resolve --update` with `--lock=frozen` or `local`; `--lock=offer` changes its acceptance to a full proposal, and `--lock=update` is redundant but valid. Refresh accepts only offer/update. Plain resolve and check reject `--lock` overrides because they are validation/comparison actions, not repair. Action flags update/check/refresh are mutually exclusive. An explicit check is allowed to freshly compare in CI, but cannot write.

Seed defaults must be core-safe. Main-only CI override cannot retroactively permit missing pre-main inputs; configure catalog/bootstrap or explicit invocation. No new hidden user-global/server-env lock override.

- `resolve`: pinned validate/materialize, zero fresh solves, includes module/tool/plugin/recipe inputs.
- `resolve --update`: explicit full lock finalization transaction; retain unchanged mutable snapshot bytes unless refresh requested.
- `resolve --check`: explicit fresh comparison, no writes; not frozen CI gate.
- Proposed `resolve --refresh [selectors...]`: revalidate selected mutable snapshots/metadata and necessary closure; offer by default, `--lock=update` auto-accepts. No selector means all mutable snapshots. Reject frozen/local and combinations with check/update action flags.
- `--offline`: no artifact fetch/solve/materialization network. Complete cached metadata can satisfy non-frozen solving; frozen still uses pins only. Compiler stages additionally follow the HostKnown-only offline admission rule above; arbitrary trusted task/program effects are not sandboxed. Missing inputs or unsupported stages fail typed/actionably.

Examples: `histos compile app --lock=update`, `histos test app --lock=local`, `histos compile app --lock=frozen`. P8 proves actual parser placement/precedence.

### Complete Necessary Scope

Detect all drift in preparation, not first failed fetch. Start requested cells and changed declarations; expand shared solve domains/consumers to stable closure. Compile changes include test cells; module/version/platform/upstream changes include affected downstream/tools; catalog/bootstrap changes include helper/stage/runtime closure and generated graph. Shared immutable inputs affect referring cells; independent classpaths can retain different versions. Remove affected deleted declarations with reference validation; preserve unrelated records/order/snapshot bytes.

Diff lists every added/removed/changed direct, transitive artifact, eviction, locator, recipe/stage input, checksum within closure. Discovery may populate content cache; never tracked lock or suspended-command effects. Failed solve cannot accept a subset.

### State Machine and Atomic Commit

`Captured -> Discovering -> CandidateReady -> Offered/AutoAccepted/Local/FrozenValidated -> Committing -> Adopted -> Executing -> Completed`.

Pre-commit failure/decline/interruption discards/cleans candidate and retains old lock. Frozen miss blocks without solve. Local adoption skips Committing.

1. Proposal binds ID, raw-lock digest/absence, declaration/source digest, candidate digest, full scope/diff, client/invocation, and original command.
2. Finish discovery/validation before acceptance. Reply quotes proposal/revision; new policy/rebuild warnings are visible.
3. Take short in-process commit gate and cross-process lock-store guard; reread revisions. Stale consent fails `StaleProposal`. Never hold guard while solving/prompting.
4. Write/fsync/verify a same-directory temporary lock, then recheck revisions immediately before rename; atomic replace and directory sync where supported. If atomic replacement unavailable, typed failure, not ordinary-move fallback. All Histos writers honor the guard. Optimistic checks detect external edits observed before rename; an advisory guard cannot promise filesystem compare-and-swap against an editor that ignores it.
5. Mask interruption only around minimal replace/publication. Before rename, old lock stays. After rename, new complete lock is committed and must be adopted/recovered, not reported rolled back. Pre-rename write failure leaves old file.
6. Publish already validated candidate generation/snapshot under generation gate; retire old leases and invalidate affected bindings. Crash after rename recovers committed revision on restart; no command ran yet.
7. Atomically change invocation Prepared to Executing. Duplicate accept returns current state/result, never reruns. Disconnect before acceptance discards; disconnect after commit follows cancellation semantics, no automatic replay.

Exactly-once is within managed invocation, not guaranteed external side effects across process crash. New client submission is a new invocation. Existing old-generation leases continue against old snapshot; new commands take adopted generation. Multiple proposals can coexist but only current-base consent commits.

### Integrity and Snapshots

Declaration drift/missing lock is repairable under policy. Syntax/format corruption is hard failure. Checksum mismatch is integrity, not drift: quarantine/refetch same pinned locator if possible, then fail if disagreement persists. Ordinary update never rewrites checksum to hide replacement.

Derived cache corruption can reconstruct from pins without changing published inputs. Retain locked snapshot locator/bytes during ordinary repair; if necessary solve requires replacing retained mutable inputs, return `RefreshRequired`. Explicit refresh proposes new snapshot locator/checksum, visibly labeled mutation. Immutable release-coordinate replacement remains integrity failure.

### Client and Transport Parity

Common `InvocationEnvelope(context, call)` wraps existing Call. Structured `LockOffer`, `LockReply.Accept(proposal,revision)`/Decline, `LockCommitted`, `LockBlocked`, and binding/cache events share wire codecs. Offer routing validates caller/invocation ownership. Never reuse `RunInput.Stdin` for consent.

- CLI asks at invoking client terminal if offer-capable. Redirected input yields `ConsentRequired`, full summary, and exact rerun command with update/local. No daemon stdin read.
- MCP negotiates structured offer/reply; unsupported clients receive actionable structured diagnosis, never terminal prompt assumptions.
- BSP without negotiated consent emits diagnosis/action command. No speculative BSP extension required for v1.
- Watch retains original client context. Catalog/bootstrap/main/local sources/resources/lock revisions invalidate preparation. Accept resumes pending iteration once; new change is new iteration. Suppress repeated unattended offers for same token; obsolete proposals cancel.
- Warm generation keys exclude initiating command/client/sink. Each invocation independently validates policy against the reusable committed snapshot; ephemeral selection is separately keyed and cannot satisfy a frozen caller. Invocation data/events are passed at method execution, never cached in generation constructors.
- Native client encodes common context or falls back to JVM for unsupported flags. Fast path still sends CI/interaction. Wire version skew fails/reboots rather than misreads consent.

Expected local output uses complete structured diff; this abbreviated transcript names its shape:

Planned transcript: `examples/expected-lock-repair.txt`.

```text
Lock repair required: app compile/test and module-tools plugin closure
Policy: offer (project default, local client)
Proposal: <proposal-id>, base lock: <revision>
- app: removed direct and transitive artifacts
+ app: added direct and necessary transitive artifacts
+ module-tools: binary, recipe, helper inputs
Unrelated entries and pinned SNAPSHOT bytes retained.
Accept this proposal? [y/N]
Lock committed: <new-revision>
Plugin module-tools: rebuilt for host <api-digest> (API binding changed)
Resuming: histos compile app
```

Frozen diagnosis names input, states no solve/no lock write, suggests explicit update/refresh appropriately. Host-only mismatch with complete pins rebuilds without lock offer. Task fixture outputs `reviewed`; repeat reports existing task-cache hit while plugin-load compile count remains zero.

## Errors and Codecs

| Error family | Domain/API ownership | Adapter/wire ownership |
| --- | --- | --- |
| Catalog/bootstrap | New `api/src/histos/plugin/{Catalog,Bootstrap}.scala` | `wire/src/histos/load/LoadError.scala`, source/phase diagnostics, `WireJson.scala`. |
| Strategy/lock data | New `api/src/histos/lock/{LockModel,LockStrategy}.scala` | Engine maps resolver failures; public context never exposes Coursier/engine lock objects. |
| Recipe/publication | New API RebuildRecipe and mode in `Pom.scala` | Nest RecipeError in PluginLoadError and LocalPublishError (`wire/src/histos/publish/LocalPublishData.scala`), exhaustive codecs/renderers. |
| Boundary/compiler/loading | API `plugin/Errors.scala`, loading contracts | `wire/src/histos/load/LoadError.scala` and BuildFault in `wire/src/histos/report/CallResult.scala` preserve paths/bindings/compiler positions. |
| Activation | API BuildServices/ServiceSetupError and PluginEvents | Loader maps setup diagnosis to structured activation error; invocation/generation event closure is a typed PluginEventError. Wire owns codecs, not API. |
| Host compiler | HostCompilerManifest/Error model in wire toolchain; scoped provider/session contract in engine toolchain | Loader nests host bootstrap error; extraction/worker/JDK failures remain distinguishable. No resolver fallback or project lock repair. |
| Stage/seal/snapshot | API StageContract/ArtifactManifest, wire CompilationSnapshot | Typed unsupported stage/offline/binding/seal-change/capture-absence errors nested in recipe/compile/publication faults; canonical data codecs remain in wire. |
| Offer/commit/revision | API lock data, wire `server/Protocol.scala` or new LockProtocol | New typed transaction errors in wire resolve, events in BuildEvent, BuildFault nesting, WireJson codecs. |
| Task | Existing PluginRunError/PluginFault | TaskError is in `wire/src/histos/task/Ids.scala`; preserve typed compile/scheduler/task faults through bind and remove touched flattening to `Failed(other.message)`. |

Map Throwables at platform IO/compiler/classloader edges, without exception-message control flow. JVM classloader overrides necessarily throw under Java contracts; do not propagate this exception pattern into domain APIs. Config uses typed startup validation. Source/compiler errors remain compiler diagnostics, not generic setup strings.

## Security

Plugins are trusted executable code, including object initialization, macros/plugins, and layer setup. Checksums establish which bytes ran, not trustworthiness. No sandbox claim. Stage contracts separate reproducibility assertions from executable trust; offline guarantees Histos artifact IO and specified recipe-worker admission, not prevention of arbitrary network/IO in trusted main-build initialization, reused plugin code, tasks, or programs.

Verify one API/runtime copy and complete engine-link bans before compile/load. Maintain a reviewed export manifest from actual API artifact classes/packages, fixing handwritten whitelist drift without exporting all assembly `histos.*`. Parent-owned delegation applies only to declared boundary; conflicting other classes fail with origins.

Extraction rejects traversal/absolute paths, symlinks, duplicate/invalid entries, and excessive sizes. Validate recipe before stage execution. Fetch pinned declared origins; redact credentials in messages/keys. Derived remote-cache byte verification and existing namespace trust policy remain mandatory.

Consent binds checkout/client/invocation/declarations/base revision/candidate digest. Server env, unrelated watcher, and program stdin cannot authorize mutation.

## Observability

Structured events report compile phase, selected policy/override origin/core seed defaults, per-plugin published/derived provenance and binding, cache hit/miss/key/compiler count, proposal scope/diff/revisions/consent/commit/adoption/resume, and typed failures with path/positions.

One informing rebuild line plus ordinary compiler diagnostics is enough for expected mismatch. Frozen success states no lock modification. All adapters consume same events; never log secret env/input values.

Measure materialization, catalog/main/recipe compile, cache lookup, and total preparation in assembled fixture. No hypothetical performance figures. Second unchanged local/published load must call compiler zero times; any full-key change invalidates.

## Alternatives

| Alternative | Disposition |
| --- | --- |
| Main graph first and separate wiring | Cannot support main plugin imports/early config. |
| Policy only in main services | Too late for its own input loading; resolved B2 provides optional early strategy wiring. |
| Bootstrap from previous cached build | Fails first install/CI/host upgrade and cannot be authority. |
| Republish on every host digest | Misses approved default rebuilding; binary-only remains option. |
| Source jar plus POM | Incomplete generated/Java/resource/settings/compile-only/helper/stage inputs. |
| All Scala 3 binaries compatible | Ignores TASTy direction/runtime; exact digest initially. |
| Replay producer build | Arbitrary generators/solves defeat pinned frozen inputs. |
| Always lock or infer from file | Changes opt-in; B1 keeps explicit intent. |
| Per-cell repair writes | Partial lock and incomplete consent; one transaction instead. |
| String/object service map | Loses Tags/requirements/errors; standard typed ZIO plan. |
| Classpath auto-activation | Incidental dependencies install effects/conflicts; explicit activation instead. |
| Routine snapshot refresh | Mutates unchanged pins; separate explicit operation. |
| New enforcement compiler plugin | Requires approval; existing compiler gates suffice now. |

Bazel/Gradle provide lock-policy vocabulary; Mill/sbt describe binary baselines. References do not establish automatic published source rebuilding.

## Focused Test Kit and Proof

A small `plugin-testkit` module depends on existing API/ZIO, not production engine/compiler. Producer tests use existing ZIO Test. Helpers provide scoped invocation/generation contexts, fake events, and evaluation of supplied strategy/task against explicit data. Return real decisions/output/errors for decisive assertions, not `isSuccess`. Full publish/load/transaction belongs to engine/app integration.

Proposed pure `LockHarness.missingLock(execution: ExecutionKind): LockContext` supplies an explicit locked Graph/Discovery fixture with absent revision and stable fixture command/declarations. Scoped task helpers provide real nested scopes and record finalizers. The fixture below depends on the local kit module during implementation; a published kit is a test-only dependency matched to the producing host, never automatically added to plugin production dependencies.

Planned producer test fixture: `reviewed-locks/test/com/example/reviewed/ReviewedLocksSpec.scala`.

```scala
package com.example.reviewed

import histos.lock.*
import histos.plugin.testkit.LockHarness
import zio.ZIO
import zio.test.*

object ReviewedLocksSpec extends ZIOSpecDefault:
  private val decisions = suite("reviewed lock policy")(
    test("the interface layer keeps CI frozen"):
      val decision = ZIO.serviceWithZIO[LockStrategy]: strategy =>
        strategy.decide(LockHarness.missingLock(ExecutionKind.Ci))
      decision
        .provide(ReviewedLocks.layer(ReviewConfig(local = ReviewMode.Ask)))
        .map(found => assertTrue(found == LockDecision.Frozen))
  )

  def spec = decisions
```

| Risk | Sufficient proof and owner |
| --- | --- |
| Typing/keys/errors/ergonomics | P1/P2 exact positive files, negative missing service/late config/wrong key compiler fixtures, runtime duplicates. No lambda/result ascriptions. |
| Missing first-run strategy/remaining plugin | P6 empty-cache local repository: core seed order, then custom policy before remaining solve; frozen miss solver count zero. |
| Atomic consent/closure | P3 failure/decline/interruption pre/post rename/stale token/duplicate accept/concurrent writer/atomic-move failure; unrelated snapshot bytes preserved. |
| Complete recipe | P4 Scala+Java+generated replacement+resources+compile-only+macro stage fixture; unsupported publish makes zero uploads. P10 owns rebuilt executable output. |
| Host/cache inputs | P5/P6 mixed Scala+Java A-to-B bundle proof with no B project compiler cell, decisive output, zero solves, unchanged lock; P10 installed repeat after derived-cache clear. |
| Transitive/binary-only/boundary | P6 helper-only drift rebuild/refusal, ordinary pinned hash retained, conflicts/engine references refused. |
| Task provenance | P7 plugin byte/body/args/service-input changes and mixed origins yield correct outputs/distinct keys. |
| Resource lifetime | P2/P6 scoped counters/Promises: alive through invocation, closes candidate discard/retired final lease/task interruption. |
| Invocation parity | P2/P8 two concurrent callers/commands on one constructed generation, correct request data and disjoint invocation events; transport/CI/stdin checks still apply. |
| Stage inputs/offline | P4/P5 unchanged declared stage bytes, changed frozen file/env key/output, missing binding refusal, caller-env invariance; offline declared-stage pre-worker refusal and HostKnown success. |
| Complete artifact bytes | P4 valid equal-ProductDigest/different-TASTy package check including binary-only; P5 valid singleton-type helper change invalidates compiler-visible consumer. |
| Assembled install/import/repair/resume | P10 local producer/consumer integration, host drift, typed helpers/services/tasks, repair once, frozen/offline, counts and output. |

Use ZIO Test, deterministic clock/random, Promises/supervised fibers, scoped workspaces/acquireRelease. No sleeps, unsafe runtimes, partial accessors/casts, or exception-message branching. Real IO requiring live clock uses `withLiveClock` on a suite val. Slayer already supports scoped external services if needed; pure tests need no artificial container.

Every static correctness finding gets failing-first reproduction and a green durable assertion in its owning unit. Each unit may clean up/refactor relevant tests when implementation calls for it, explaining why and retaining or improving meaningful regression protection. Unrelated rewrites and standalone broader test-hardening remain separate scope. Do not invent missing historical suites.

## Rollout and Migration

1. Shared contracts/compiler fixtures land without new-API self-host adoption. Existing loader remains usable until replacement assembled; staging is not a permanent compatibility promise.
2. Implement recipe publication/versioned pins/compiler/cache, then new loader and activation. Route all invocation adapters through coordinator.
3. Retire graph-first plugin declaration/mandatory wiring semantics. Update scaffold/BSP source mappings/watch fingerprints/docs/fixtures together.
4. Bootstrap assembled local/approved snapshot host before repository uses new DSL. Current `build.properties` pins `0.4.2`; shim `./histos` does not execute edited engine automatically.
5. P10 uses baseline host to compile old-compatible assembled graph and existing `./histos publish local` path in isolated home/fixture, then records local ID via `./histos version`. Only then adopt new catalog/services DSL. If baseline cannot compile assembled API, pause for owner-approved source-bootstrap path, do not guess flags or call old-host tests edited-source proof.
6. CI needs assembled approved snapshot/local test host; pin/bootstrap adoption is explicit P10 work. Snapshot integration only until Russ says feature complete. No release/merge implied.

Users move `Projects.plugins` sources/local prerequisite module declarations to the core-safe catalog, opt into locking there when wanted, and let the main graph inherit without repeating `Projects.locked`. Migrate ordinary wiring to main services with correct interface keys/task activation. Mark early-strategy prerequisites in catalog and use optional `histos.strategies.scala` for early policies. Main inherits early instances unless explicitly replacing graph policy. Producers choose `.pluginLibrary` or binary-only; legacy sources are not rebuild bundles. Explicit lock migration captures recipe inputs, never silent frozen writes.

Failed candidate reports failure, not silently running new command against old graph. Old generation remains valid for already active leases. Committed lock recovers by revision.

## Open Questions

- Owner resolved B1/B2, recorded as D13-D15. No product decision blocks the reviewed architecture. Escalate any later implementation discovery affecting these final decisions.
- P1/P2 compiler-proves associated-type builder composition, union errors, environment evidence with actual Scala/ZIO. Source/docs first; compiler checks and any miniature probe belong to separately authorized implementation work.
- P4/P5 inventory effective javac/stage settings only at touched seams; unsupported semantics fail publication, not TODO omissions.
- Keep exact digest/conservative activation identity initially; optimize only with measurement/proof.
- Independent review rounds 1 and 2 are complete, with AR1-AR6 verified and closed and zero open design issues. No additional user input or dependency request is required. Compiler/runtime proof and post-creation rendered Markdown review remain pending.

## References and Required Reading

Read [README.md](../README.md), then the linked Current State seams. Apply owner-supplied local `Agents.md`/`AGENTS.md` instructions if present; those case aliases are ignored rather than tracked at this baseline, so this document deliberately has no repository link to them. All repository links are relative to this document, `docs/plugin-architecture.md`. Additional required files:

- Public [ProjectDef.scala](../api/src/histos/ProjectDef.scala), [PluginRef.scala](../api/src/histos/PluginRef.scala), [Contribution.scala](../api/src/histos/plugin/Contribution.scala), [Errors.scala](../api/src/histos/plugin/Errors.scala).
- Loader [BuildFile.scala](../engine/src/histos/load/BuildFile.scala), [PluginRuntime.scala](../engine/src/histos/load/PluginRuntime.scala).
- Compiler [Sources.scala](../engine/src/histos/zinc/Sources.scala), [GraphCompile.scala](../engine/src/histos/zinc/GraphCompile.scala), [JvmCompile.scala](../engine/src/histos/zinc/JvmCompile.scala), [CompileInputs.scala](../engine/src/histos/zinc/CompileInputs.scala), [BuildCompiler.scala](../engine/src/histos/toolchain/BuildCompiler.scala).
- Invocation [Daemon.scala](../app/src/histos/server/Daemon.scala), [Grammar.scala](../app/src/histos/cli/Grammar.scala), [ResolveError.scala](../wire/src/histos/resolve/ResolveError.scala), [WireJson.scala](../wire/src/histos/report/WireJson.scala).
- Actual conventions [BspMetalsSpec.scala](../app/test/histos/bsp/BspMetalsSpec.scala), [RunCallSpec.scala](../app/test/histos/client/RunCallSpec.scala), [NativeLibraryClasspathSpec.scala](../engine/test/histos/native/NativeLibraryClasspathSpec.scala). Reuse scoped fixture/observable assertion style. Relevant cleanup, including fixing test shortcuts, is allowed under D16 when needed; explain it and retain or improve regression protection.

Follow the public [ZLayer reference](https://zio.dev/reference/contextual/zlayer/) and [resource management reference](https://zio.dev/reference/resource/). Source mechanisms consulted were ZLayer `build/project/scoped` and ZEnvironment `add/get/getDynamic/prune/union` in `core/shared/src/main/scala/zio/{ZLayer,ZEnvironment}.scala`. An optional local checkout is `~/projects/forks/zio`; it is not a portable reference link. Read the source corresponding to the build's ZIO version when implementing. ZIO internals' casts are not Histos design patterns.

External evidence already consulted: [Bazel lockfile](https://bazel.build/external/lockfile), [Gradle dependency locking](https://docs.gradle.org/current/userguide/dependency_locking.html), [Mill plugins](https://mill-build.org/mill/extending/writing-plugins.html), [sbt 2 plugins](https://www.scala-sbt.org/2.x/docs/en/reference/plugin.html). Bazel documents update/error/refresh/off; Gradle strict/lenient/selective; Mill oldest supported host/binary line; sbt older minimum host and `_sbt2_3`. Do not claim automatic source rebuilding from these docs.

## Design Review Ledger

Original research R1-R7 is preserved; R8-R14 are focused additional observations. R9 is closed in this diff. The other rows stay open: static findings are not executable verification.

| ID/category | Finding | Owner, response, required proof | Closed |
| --- | --- | --- | --- |
| R1, task cache | `Loader.linksOf` empty artifacts/global mixed provenance. | P7 per-activation manifest/task closure; old plugin bytes reproduce stale output, then distinct key/correct output. | [ ] |
| R2, package cache | PackageKey excludes API stamp injected by LocalPublish. | P4 metadata-aware key; stamp-only byte/key change failing-first. | [ ] |
| R3, compile cache | Substituted host API has no matching input hash. | P5 actual-byte hashing; host-only false hit reproduction then green. | [ ] |
| R4, source completeness | Published sources omit generated compile replacements. | P4 freeze Sources.prefer Scala/Java/generated inputs; executable generated-code rebuild fixture. | [ ] |
| R5, compile inputs | Compile-only omitted from POM; source jar lacks resources/flags. | P4 independent complete recipe; fixture needs all, unsupported publication zero uploads. | [ ] |
| R6, transitive binding | Only root stamp checked before loading closure. | P6 full helper/runtime validation; helper-only mismatch and binary-only refusal. | [ ] |
| R7, local external jars | PluginModules omits module resolved external jars. | P6 candidate/frozen complete local DAG; external-import failure then working first load. | [ ] |
| R8, local reuse | Temporary Scala-only path lacks full settings/resources/persistent cache. | P5 primitive/P6 adoption; Java/resources/flags plus zero compiler on second load. P6 closes. | [ ] |
| R9, errors/typing | Any/Nothing macro contracts; unconstrained setup toString. | P1/P2 typed contracts/structured errors. P10 deleted the bundle macro and `WiringSource`. A lone `histos.wiring.scala` is not compiled. | [x] |
| R10, scope | readBuilt inner scope ends before retained values. | P2/P6 generation construction; service alive through task and released final lease. P6 closes. | [ ] |
| R11, boundary | Handwritten export whitelist omits theme/step API packages. | P6 reviewed actual export manifest; exported APIs work, engine/duplicates still fail. | [ ] |
| R12, clock | CompileInputs.remember uses ClockLive.instant. | P5 effect clock and deterministic test-clock assertion; no global audit. | [ ] |
| R13, task faults | PluginRuntime.bind flattens non-task faults into Failed(other.message). | P7 typed nesting; compile/scheduler failure distinguishable across adapter. | [ ] |
| R14, local host detection | PluginModules.definesHost treats source text containing `trait HistosBuild` as host identity and can skip compilation. | P6 verified HostProvided identity replaces textual heuristic; comment/string containing that text cannot suppress a real local helper compile. Failing-first fixture then green. | [ ] |
| A1, early intent | Main graph too late for pre-main policy. | B1 resolved by D13; P1/P6 prove inherited intent/conflict checks and frozen zero solve. Executable gate remains open. | [ ] |
| A2, typed bootstrap | Main services too late for remaining input loading. | B2 resolved by D14/D15; P6 proves first-run `histos.strategies.scala` without prior cache. Executable gate remains open. | [ ] |
| A3, service keys | Concrete key not capability registration contract. | P2 interface/projection/duplicates/missing service/setup/scope proofs. | [ ] |
| A4, recipe/TASTy | Closure/stages/compiler direction must be complete. | P4/P5/P6 typed supported model/refusal/closure; P6 closes assembled fixture. | [ ] |
| A5, transaction | Consent/write/adoption/resume races. | P3 store and P8 parity. A local re-run does not close this row. | [ ] |
| A6, CI/transport | Server env/stdin cannot select caller repair; frozen differs from fresh check. | P8 envelope. A local caller re-run does not close this row. | [ ] |
| A7, self-host | Shim/pin may run old code. | The pin is `0.4.2`. A local jar is not a channel publish and does not close this row. | [ ] |
| A8, host compiler tools | Existing Zinc module path obtains compiler/bridge via LockedTools; it cannot implicitly solve a new host version in frozen fallback. | P5 supplied the bundle. Local packaging of the launcher closure does not close this row. | [ ] |
| V1, review | Two rounds completed; six design findings verified and closed, none open. | Round 2 accepted AR1-AR6 with no new/reopened findings. Design review is complete; implementation proof remains pending. | [x] |

Review response format: issue ID, severity, section/path, required change, writer response, owning PR, reviewer verification. Escalate product choices to Russ; explain declined mechanisms. Resolving wording does not close executable findings.

### Independent Review Rounds 1-2: Responses

**Round summary:** Round 1 found six issues (four major, two minor); the writer addressed all. Round 2 verified and closed AR1-AR6, with zero open design issues and no new/reopened findings. D1-D15 are intact. No finding is marked won't-fix, and no dependency or owner decision is pending. Design closure does not imply compiler/runtime verification.

| Finding ID | Severity | Status | Response and owning completion gate |
| --- | --- | --- | --- |
| AR1 | Major | Closed, verified in round 2 | Added Host-Owned Compiler Bootstrap Contract: distribution-selected complete original compiler/bridge/support/runtime payload, explicit host JDK, validation/materialization without Resolve/LockedTools, scoped session and typed errors. P5 consumes supplied artifacts; P10 packages existing supported tools only. P5/P6 mixed Scala+Java A-to-B recipe with no B compiler cell asserts output/zero solves/unchanged lock; P10 repeats installed with cleared derived cache. Extra coordinates would require approval. |
| AR2 | Major | Closed, verified in round 2 | Replaced invocation-bearing construction ambient with GenerationAmbient and split PluginEvents.Generation/Invocation. Lock/loading/task requests carry per-call context and immutable invocation handle; no current-context mutation or captured initiating sink. P2/P8 concurrent distinct callers/commands share one constructed generation and assert correct data, isolated invocation events, and cleanup. |
| AR3 | Major | Closed, verified in round 2 | Added explicit HostKnown and ProducerDeclared stage contracts, frozen public file/env bindings, required-key validation, canonical cwd/source labels, supervised cleared-environment worker, deterministic/no-network assertions and trust limits. Scanning never proves arbitrary hermeticity. Offline admits known stages only and refuses declared stages before worker start. P4/P5 file/env key/output and offline/admission assertions are required. This is the supported v1 recipe boundary, not a sandbox claim. |
| AR4 | Major | Closed, verified in round 2 | Added complete sealed raw artifact-entry manifest for all class/TASTy/resource/injected entries, manifest-based packing, and compiler-visible helper/stage identity. Explicitly excluded ProductDigest.ofDirectory from complete identity; semantic policy otherwise unchanged. P4 includes binary-only TASTy false-hit reproduction, P5 uses a valid singleton-type helper/consumer fixture, not corrupted compiler input. |
| AR5 | Minor | Closed, verified in round 2 | P1 now owns named snapshot/capture/host/seal/stage/activation/provenance contracts, explicit absence states, shared result-shape adaptations and fixtures. P4/P7 develop against that landed base without sibling unpublished branches; production landing waits for P5/P6 respectively and repeats real producer-to-consumer proof. Genuine parallel waves and P10 installed gate remain. |
| AR6 | Minor | Closed, verified in round 2 | Repository links are destination-relative; ignored local instruction files are inline, not dead links. Removed local ZIO links in favor of public docs plus optional inline clone path. Standard scala/text fences have adjacent planned filenames. P9 owns destination-relative tracked-link check and GitHub Markdown rendering; run doc remains only Scala API-doc proof. Current checks are read-only text/path inspection, not rendering. |

**Reviewer verification, round 2:** Reviewer read the full revision and reused source/ZIO evidence. Verified AR1's complete compiler payload/JDK/scoped materialization independent of Resolve/LockedTools; AR2's generation-only ambient and per-invocation context/event handles; AR3's stage input/trust/offline boundary; AR4's complete raw class/TASTy/resource/metadata seal and valid regression fixture; AR5's foundation contracts and production landing gates; and AR6's portable GFM/relative links. All six design findings are closed.

Read-only checks confirmed clean e0027a3, all 39 unique repository-link targets tracked and destination-relative, 10 Scala fences plus one text fence paired, and no absolute local filesystem links. No compiler, runtime test, or probe ran for that review. Rendered Markdown review is post-creation and distinct from scaladoc. At that review, R1-R14 and A1-A8 were open. P10 closes R9 in this diff. A5-A8 stay open. These administrative closure edits change no mechanism and require no architecture re-review.

### Writer Revision Responses

These are authorship checks, not independent review or implementation proof.

| ID | Issue | Response |
| --- | --- | --- |
| W1 | Owner startup decisions arrived during drafting. | Incorporated D13-D15, marked B1/B2 resolved, used `histos.strategies.scala`, and removed duplicated main opt-in. Original product decisions and R1-R7 remain preserved. |
| W2 | Error enums do not live in same-named files. | Read-only source lookup verified LocalPublishData.scala, CallResult.scala, and task/Ids.scala; API/error map and PR owners corrected. |
| W3 | A final policy could otherwise authorize solves/consent retroactively. | Added Discovery/Acceptance stages, decision-before-solve, aggregate phase constraints, and explicit action/override conflicts. P3/P8 must prove them. |
| W4 | Covariant layer type alone does not establish interface runtime key. | Required interface Tag normalization/projection, total malformed-output diagnosis, and composite-key rejection; P2 compiler/runtime gate remains open. |
| W5 | Recipe capture/classloader reuse could drift from compiled inputs. | Required publication snapshot revalidation and one retained catalog/shared loader across strategy/main compilation. P4/P6 prove these mechanisms. |
| W6 | Example metadata and document-only constraints. | Original writer checked line metadata; AR6 replaces it with standard Scala/text fences, adjacent filenames, and destination-relative links. Current checks are read-only text/tracked-path inspection, not Markdown rendering or compiler proof. P9 owns rendered destination review. |
| W7 | Final owner comment permits justified relevant test cleanup. | Recorded D16 and aligned Non-goals, test guidance, and all owning-unit instructions. Meaningful regression protection must be retained or improved; unrelated rewrites/broader hardening stay separate. No mechanism or pending proof gate changed, so no new architecture review is required. |

## Completion Checklist

Document gate:

- [x] Owner resolved B1/B2; D13-D15, protocol, and inherited-intent examples updated.
- [x] Independent round 1 inspection completed and all AR1-AR6 writer responses recorded.
- [x] Round 2 verified and closed all six design findings; no open/reopened findings.
- [x] Read-only destination-relative tracked-link and paired-fence checks completed.
- [x] Docs-only approval received to save this handoff and perform document checks; no implementation, commit, push, PR, publishing, or release authorization.
- [x] Approved handoff saved as `docs/plugin-architecture.md`, including D16.
- [ ] Rendered GitHub-compatible Markdown review remains a later separately authorized documentation-PR gate, distinct from Scala API documentation checks. No accessible local renderer was found.

Later implementation gate:

- [ ] Exact consumer/producer/service-only/task/local/published examples compiler-proven. This unit did not compile a published consumer or producer.
- [ ] R1-R14 reproduced failing-first then green. Failing-first reproduction stays with the merged units, so this bundle line stays open.
- [ ] Complete helper/stage fallback, binary-only/unsupported/integrity/frozen/offline/refresh paths proven. This unit did not run helper, stage, binary-only, frozen, offline, or refresh fixtures.
- [ ] Consent/stale writers/interruption/adoption/once-only execution proven. This unit did not run consent or once-only fixtures.
- [ ] All client entrypoints use common caller context/coordinator; two concurrent callers share one constructed generation with isolated invocation data/events. A native client process and MCP were not run.
- [ ] A-to-B mixed rebuild uses complete distribution tools with no B project cell, zero solves, unchanged lock; declared stage inputs/offline restrictions and complete TASTy seals proven. This unit did not prove stage inputs, offline refusal, or a TASTy seal.
- [ ] P4/P7 repeat real producer-consumer proof after P5/P6 landing, not only contract fixtures. Those landing gates already merged. This unit did not publish a new producer.
- [ ] Second unchanged local/published loads zero compiler; host/full-key changes invalidate. The second load here is a local module, and a published load was not run.
- [ ] Assembled host actually executes edited sources; self-host/scaffold/BSP/watch migration complete. The pin is `0.4.2`, so this checkout's engine is not the running host.
- [ ] Format, affected full tests without quick, workflow, docs checks run. The combined `./histos test api wire engine app plugin-testkit` exited 1 on `ld: library 'crypto' not found` before engine finished.
- [ ] Hard-unit and whole-change review complete; no unauthorized merge/release. The parent reviews this unit. No release was cut.

## PR Plan

Proposed work split only, no branch/PR authorization. **10 implementation PRs mean 10 required CI check sets**, plus a documentation-only PR if separately requested. Shared APIs first; independent units run concurrently in own worktrees, at most four. Do not force independently buildable concerns into one stack. Each runs smallest sufficient checks; P10 owns assembled completion.

New suite names below are **planned**, not existing. Read files/instructions before editing; use `./histos fmt` after implementation edits. Routine Histos MCP equivalents are acceptable with executing host identity known. No new dependency is preapproved. Source/docs precede probes; compile exact example files with no ascription repair.

**Test ownership, D16:** Every unit may clean up/refactor its relevant tests when implementation calls for it. Name the need and affected tests in that unit, retain or improve meaningful regression protection, and rerun its named checks. Unrelated rewrites and standalone broader test-hardening need separate scope. This permission does not weaken any acceptance gate.

### P1. Shared API, errors, protocol and compiler surface

**Owner:** API architect/implementer, own worktree. **Depends:** separate implementation authorization; documentation approval and the resolved B1/B2 design review are complete. **Wave 1, serial foundation** because later units consume shared types/codecs.

**Owns:** API Graph/Resolution/PluginRef, lock/policy/build-service/recipe/error/publication contracts, GenerationAmbient and invocation-request/event contracts. Also owns the named ArtifactManifest, StageContract, CompilationSnapshot/CompilationCapture, HostCompilerManifest/Inputs/Session, and activation/verified-provenance contracts listed in Foundation Contracts, their shared caller/result-shape adaptations, and `engine/test/histos/contracts/ContractFixtures.scala`. Wire `zinc/CompileFeed.scala`, new snapshot/toolchain models, `load/LoadError.scala`, `publish/LocalPublishData.scala`, `report/CallResult.scala`, event/protocol/codecs. Engine contract files contain pure models/provider interfaces, not compiler/loader implementation. API never imports engine/wire.

**Consumes/provides:** Existing graph/Dep/contribution/Noted. Provides a landed buildable foundation with typed Unrecorded/Unverified states, canonical seal/contract validation, and fixture seams used by every parallel unit. No live loader behavior; P2/P5/P6 implement producers. This is the declared base, not an agreement to infer sibling branch types later.

**Risk/check:** Environment/output/error inference, Tags, dependency direction, exhaustive codec roundtrips and the AR5 fixture/result contracts. Planned `api/test/histos/plugin/PluginContractsSpec.scala`, `wire/test/histos/plugin/PluginWireSpec.scala`, and `engine/test/histos/contracts/ContractModelsSpec.scala`; separate hard type-level review.

**Gate:** `./histos compile api wire engine`; `./histos test api wire engine --only '*PluginContractsSpec*' '*PluginWireSpec*' '*ContractModelsSpec*'`; `./histos fmt --check`. Every later unit compiles/tests against this landed base. Main entrypoint fixture execution awaits P6.

### P2. Typed service activation and author kit

**Owner:** ZIO type/service implementer, own worktree. **Depends:** P1. **Wave 2**, parallel P3/P4/P5.

**Owns:** BuildServices implementation/registration helpers, new `plugin-testkit/src/histos/plugin/testkit/{PluginHarness,LockHarness}.scala` and tests, `api/test/histos/plugin/BuildServicesSpec.scala`. Root histos.scala only adds kit module using existing APIs/dependencies, no new-DSL adoption.

**Consumes/provides:** P1 contracts/HistosPlugin; supplies typed core replacements/add/task plan, duplicate checks, generation activation/kit.

**Risk/check:** R9/A3 and AR2; positive/negative compiler examples, correct projection, missing/duplicate services, scoped setup failure/interruption. Generation constructors cannot require invocation context/sink. Add two concurrent request fixtures using one constructed service and distinct contexts/event queues. R10 live-loader proof belongs P6; P8 closes actual transport routing. Separate type/scope review.

**Gate:** `./histos compile api plugin-testkit`; `./histos test api plugin-testkit --only '*BuildServicesSpec*' '*PluginHarnessSpec*'`; `./histos fmt --check`. No erased outputs/casts/unsafe.

### P3. Revisioned pins and lock transaction core

**Owner:** Resolution/concurrency implementer, own worktree. **Depends:** P1. **Wave 2**, parallel P2/P4/P5.

**Owns:** Resolve/DependencyLock/ResolutionService/LockedTools/CoursierResolve touched locator/state/repair seams; new `engine/src/histos/resolve/{LockStore,LockCoordinator,RepairScope,ResolutionSnapshot}.scala`; `engine/test/histos/resolve/{LockTransactionSpec,LockSnapshotSpec}.scala`.

**Consumes/provides:** P1 declarations/revisions/decisions, existing resolver. Versioned retained pins, candidate scope/diff, frozen validation vs fresh compare, cross-process guard/atomic commit, generation publication interface. Fake preparer/generation owner keeps proof independent P6.

**Risk/check:** A5 race/interruption/stale/decline/failure/necessary closure/atomic replacement/snapshot retention; live state atomic invalidation. Failing-first at touched seams; separate concurrency/resource review.

**Gate:** `./histos compile engine`; `./histos test engine --only '*LockTransactionSpec*' '*LockSnapshotSpec*'`; `./histos fmt --check`. Actual daemon command once-only proof remains P8/P10.

### P4. Complete rebuild publication and package identity

**Owner:** Publication/recipe implementer, own worktree. **Development base:** landed P1 contracts/fixtures. **Landing dependency:** P5 real snapshot capture. **Wave 2 development**, parallel P2/P3/P5; rebase/check P5 before landing.

**Owns:** LocalPublish, new publication/recipe capture, PackageKey/touched callers, Sources packaging helpers/class-resource jar seams; `engine/test/histos/publish/{PluginPublicationSpec,PluginPackageKeySpec}.scala`/fixtures. Not GraphCompile/ScalacClasspath/root build.

**Consumes/provides:** P1 CompilationSnapshot/StageContract/ArtifactManifest fixtures and Publisher. Pure capture-from-snapshot, complete bundle, binary-only/host metadata, compile-only closure, and manifest-derived package key. Never re-enumerate a different compile input set or treat Unrecorded as complete. P5 supplies real captured cells before this unit lands.

**Risk/check:** R2/R4/R5, AR3/AR4; failing-first stamp-only/generated omission and valid equal-semantic-product/different-TASTy package fixture including binary-only. Freeze declared file/env inputs, refuse unknown/incomplete stage contracts, unsupported zero uploads. Local/in-memory publish only.

**Gate:** On P1 base run `./histos compile engine`; `./histos test engine --only '*PluginPublicationSpec*' '*PluginPackageKeySpec*'`; `./histos fmt --check`. Before landing, rebase on P5 and repeat with at least one actual GraphCompile CompilationSnapshot through publication, not only foundation fixtures. P10 owns installed rebuilt output; no release.

### P5. Host-bound compiler and cache identity

**Owner:** Compiler/cache implementer, own worktree. **Depends:** landed P1. **Wave 2**, parallel P2/P3/P4 development. P1 fixes the capture/provenance contracts; P5 lands before P4 production integration.

**Owns:** Relevant ScalacClasspath/CompileInputs/GraphCompile/JvmCompile/CompileKey and compiler-preparation seams, BuildCompiler, HostCompilerInputs implementation/materialization, new `engine/src/histos/load/{PluginCompiler,PluginCompileKey,PluginCompileWorker}.scala`, KnownStageRegistry/admission/worker protocol; planned PluginCompileCacheSpec, HostApiInputsSpec, HostCompilerBootstrapSpec, PluginStageInputsSpec.

**Consumes/provides:** P1 snapshot/recipe/stage/seal/host-session contracts, Zinc/content store and existing scoped child-process adapter. Produces real CompilationSnapshot, complete sealed compiler-visible outputs, a mixed-language route bypassing project compiler requests, normalized stage worker inputs, directional compatibility, and safe cache/single-flight. P10 packages host artifacts; fixtures use P1 manifest/provider contracts with already pinned tool materialization.

**Risk/check:** R3/R8/R12 and AR1/AR3/AR4. Host-only false hit then green; clock/key/second-load checks. Rebuild A's mixed Scala+Java recipe with B's supplied compiler/bridge and no B cell in project lock: decisive output, zero all resolver solves, unchanged lock. Missing bridge typed, no repair solve. Valid helper TASTy-only compiler-visible change invalidates consumer. Unchanged declared stage bytes with changed file/env change key/output; missing binding refuses; caller env cannot override frozen published values. Offline admits known stages and refuses declared stage before worker start. Dedicated worker/single-flight/resource review.

**Gate:** `./histos compile engine`; `./histos test engine --only '*PluginCompileCacheSpec*' '*HostApiInputsSpec*' '*HostCompilerBootstrapSpec*' '*PluginStageInputsSpec*'`; `./histos fmt --check`. P6 repeats through preparation, P10 through installed distribution with cleared derived cache.

### P6. Exact bootstrap loader and linking closure

**Owner:** Bootstrap/loader implementer, own worktree. **Depends:** P1/P2/P3 pin interfaces/P4 format/P5 compiler. **Wave 3**, parallel P7. Separate architecture/resource review.

**Owns:** Loader/PluginShips/PluginModules/Boundary/BuildLoader/BuildFile/LoadedBuild, new declaration/bootstrap/classpath orchestration; `engine/test/histos/load/{PluginBootstrapSpec,PluginClosureSpec,PluginLifetimeSpec}.scala`/exact author fixtures. Not runtime/task inputs/app adapters.

**Consumes/provides:** Typed activation/candidate pins/bundles/compiler. First-run protocol/config, complete shared classpath/provenance and scoped prepared generation/main imports.

**Risk/check:** R6/R7/R8/R10/R11/R14/A1/A2/A4 plus AR1. Reproduce external jar/root-only stamp/lifetime/export/textual-host-detection gaps first; frozen miss zero solve; custom policy after core seed before remaining solve; helper/binary-only/TASTy/stage-admission/conflict diagnoses. Compile exact examples. PluginBootstrapSpec repeats mixed Scala+Java A-to-B rebuild through loader with no B compiler cell, decisive output, zero resolver solves, unchanged lock, and typed missing-bundle failure. Second identical load zero compiler.

**Gate:** `./histos compile engine`; `./histos test engine --only '*PluginBootstrapSpec*' '*PluginClosureSpec*' '*PluginLifetimeSpec*'`; `./histos fmt --check`. P10 owns installed command proof.

### P7. Provenance and task identity

**Owner:** Task/cache implementer, own worktree. **Development base:** landed P1/P2/P3/P4/P5, including named P1 manifest/result/fixture contracts. **Landing dependency:** P6 verified-manifest producer. **Wave 3 development**, parallel P6; no unpublished sibling branch needed.

**Owns:** PluginRuntime/PluginInputs/PluginCells/PluginSteps and relevant engine task adapter; `wire/src/histos/task/Ids.scala` TaskError and WireJson refinements exclusively this wave; `engine/test/histos/load/PluginTaskIdentitySpec.scala`.

**Consumes/provides:** P1 ActivationManifest/VerifiedPluginManifest and fixture seam plus scheduler; complete task keys/mixed origins/config/args and preserved typed faults. Refuse Unverified provenance, never substitute a fake empty closure.

**Risk/check:** R1/R13 failing-first changed plugin bytes/empty artifact stale output, complete sealed helper output/args/service inputs/mixed provenance. Keep existing output/execute checks.

**Gate:** On declared foundation base run `./histos compile engine wire`; `./histos test engine --only '*PluginTaskIdentitySpec*'`; `./histos fmt --check`. Before landing, rebase on P6 and repeat a real loader-to-task verified-manifest case. P10 remains installed assembled completion owner, not a substitute for this landing gate.

### P8. Invocations, offers, generations, client parity

**Owner:** App/client systems implementer, own worktree. **Depends:** P3/P6/P7. **Wave 4**, parallel P9.

**Owns:** app server Calls/Perform/Daemon/Watching, build Build, Grammar and actual command entrypoints, client Session/Remote, MCP HistosMcp, BSP/Connection; native client entrypoints, wire protocol/client batch refinements; planned app `server/LockInvocationSpec`, `client/LockOfferSpec`, `bsp/LockPolicySpec` and wire parser/codec tests.

**Consumes/provides:** Coordinator/prepared generation/task bindings/envelope. Common client context/flags/replies/adoption/watch once-only and safe native fallback.

**Risk/check:** A5/A6 and AR2; no server stdin/env; local ephemeral not frozen; verify/frozen zero solve; explicit refresh; stale/duplicate/disconnect/once-only; program input isolation. LockInvocationSpec runs distinct commands/callers concurrently on one retained generation and asserts one construction, correct per-call command/execution data, disjoint invocation events, and request-handle closure. Generation diagnostics remain separately labeled. Reuse related assertions and clean up/refactor relevant tests under D16 when needed, explaining the need and preserving or improving regression protection. Separate generation concurrency review.

**Gate:** `./histos compile app wire client`; `./histos test app wire --only '*LockInvocationSpec*' '*LockOfferSpec*' '*LockPolicySpec*' '*RunCallSpec*' '*BatchRunSpec*'`; `./histos fmt --check`. Native compile does not replace loopback context proof; P10 owns installed parity gate.

### P9. Author docs and compiler-backed examples

**Owner:** Documentation/API ergonomics implementer, own worktree. **Depends:** settled P1/P2/P4/P6. **Wave 4**, parallel P8.

**Owns:** Destination architecture revisions after initial approval, author guide, README plugin/lock sections, focused example fixture documentation, kit docs. Scaffold implementation P10.

**Consumes/provides:** Compiler-proven interfaces/recipes/policies; complete services-only/task/typed helper/early policy/local/published producer/binary-only/test-kit/migration docs. Reuse exact fixture source, no divergent snippets.

**Risk/check:** No timing lies/uncompiled examples; record every review issue/nit. Proposed status remains until P10 assembled gate.

**Gate:** Check every repository link relative to `docs/plugin-architecture.md` against the tracked destination paths, then inspect this file's rendered GitHub Markdown in the authorized documentation PR (standard scala/text fences, adjacent filenames, tables, anchors). No external lint dependency is required. `./histos fmt --check` and P2/P6 compiler fixture checks apply to examples; `./histos run doc` checks Scala API docs only and is not Markdown-rendering proof. No static docs server.

### P10. Assembled integration and self-host/migration closure

**Owner:** Integration/toolchain implementer and architect reviewer, own worktree. **Depends:** P2-P9. **Wave 5, serial close**, consuming assembled change/root adoption.

**Owns:** Root histos.scala/catalog/strategy source if necessary, approved local/snapshot pin/bootstrap/workflow files, host toolchain bootstrap payload/manifest packaging in existing publish/assembly seams (including supplied compiler bridge/support inputs), engine scaffold templates, old WiringSource/obsolete bundle helpers retirement, remaining source/watch fingerprints outside P8, `app/test/histos/plugin/PluginArchitectureIntegrationSpec.scala`, related BSP mapping assertions, final docs/ledger closure.

**Provides:** Actually executed assembled host/producer/consumer, complete HostCompilerManifest and split compiler/bridge/runtime/support payload selected from the launcher CompilationSnapshot, install-time outer/payload verification, self-host migration, old wiring retirement, measured first/second facts, final checks/whole-change review. Existing supported tool artifacts only; any extra coordinate requires approval.

**Completion gate:** Scoped isolated repo/home publishes fixture via `./histos publish local` or approved snapshot, installs assembled B host, records version/jar/HostCompilerManifest identity, and clears derived output. Rebuild A's valid mixed Scala+Java plugin with no B compiler cell in project lock; assert executable output, zero resolver solves, unchanged lock bytes, and missing/corrupt bridge typed failure without network fallback. Also prove binary/helper paths, typed config/services/tasks, full-seal TASTy identity, declared stage file/env key/output change and offline refusal/known-stage success, two callers sharing one generation with isolated events, necessary repair/resume once, snapshot retention/refresh, frozen/offline diagnostics and lease cleanup, second load zero compiler. This closes dependent-unit installed integration promises.

After verified bootstrap adoption run:

- `./histos fmt`
- `./histos compile api wire engine app plugin-testkit client`
- `./histos test app --only '*PluginArchitectureIntegrationSpec*'` (planned)
- `./histos test api wire engine app plugin-testkit` without `--quick`
- `./histos fmt --check`
- `./histos workflow --check`
- `./histos run doc`

Record host identity, command, actual suites/results, decisive fixture assertions. Old pinned host checks are not proof of edited loader. Review hard units individually and whole change. Close all in-scope rows/checks or name blocked owner decision. Green foundation is not completion; no release/merge authorization.
