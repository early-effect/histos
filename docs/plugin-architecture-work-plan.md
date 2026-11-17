# Plugin architecture work plan

WIP board for [plugin-architecture.md](plugin-architecture.md). This file is the source of truth for order, what is in flight, and what is done. The architecture doc stays the design. Its completion checklist and the Closed column on R1-R14 and A1-A8 are not a second board. P10 closes those rows in one pass from the proofs recorded here.

D1-D16, B1/B2, and AR1-AR6 are settled. Do not reopen them. Histos is pre-release. The wiring-file load path, the `ZLayer[Nothing, Any, Plugins]` bundle macro, and exact API-jar stamps go away when their owning unit lands. No compatibility shim, no ABI oracle, no release.

Every later PR edits this file in the same branch as the code. A box is checked only with the gate command, the head SHA that gate ran on, and the PR number. An open PR is a row in the units table, not a checked box. A bug found during a unit is a new ledger row (next free id after R14), fixed in that unit, not a GitHub issue. If this file and a PR body disagree, fix this file in that PR before it merges.

## How the work lands

Ten implementation PRs, in five waves. Separate PRs against `main`. P2, P3, and P5 are independent after P1. P4 lands only after P5. P7 lands only after P6. P8 and P9 can proceed together.

Each implementation PR pays a full `ci` run. Do not add stack-position gates, and do not edit `ci.yml` before P10.

At most four worktrees at once. Short paths under `~/.grok/worktrees/early-effect-histos/` so `.histos/server.sock` stays under the macOS 104-byte limit. Names: `plug-p1` through `plug-p10`. Branches: `histos/plugin-p1` and so on. Unset upstream after `git worktree add` so the branch cannot track `origin/main`. One `./histos` server per checkout. The main agent commits, pushes, and opens the PR. Implementers do not push.

Merge: squash-merge each implementation PR when its named gate is green on that head, the log has no "not advancing the test clock" warning, and review findings on that PR are addressed. P2, P3, and P5 may merge in any order. P5 merges before P4. P6 waits for P2, P3, P4, and P5. P7 waits for P6. P8 and P9 wait for their dependencies, then either order. P10 is last. A red gate or an extra PR stops the series. No release, tag, or `sonaRelease`. Docs-only updates to this board go straight to `main`.

`./histos` runs `~/.histos` toolchains from the pin or `.histos/toolchain`. Edited engine code runs when that toolchain compiles and tests this tree. P10 is the unit that publishes a host and moves the pin. Until then, proof is the named `./histos test` gates.

Baseline for the spec's current-state table is `e0027a3`. The spec landed in `c60ee8d`. `Ship.next` stays `0.4.4`. After review, `histos.version` is `0.4.2`. `0.4.4-0d9c83706be4-SNAPSHOT` names parent commit `0d9c837` and is not on the snapshot channel. Root `histos.scala` declares no plugins.

## Standing rules

- The spec's types, error channels, and example files are the API. Examples compile unchanged apart from test coordinates. A call site that needs `(x: Foo) =>` or a result ascription means the builder is wrong.
- Failing-first for every ledger row the unit owns: red test, then the fix, then the green gate. Record the red and green SHAs in the ledger.
- D16: a unit may rewrite a test it breaks. Name the test and why. Keep the regression. Leave unrelated suites alone.
- Typed errors. No `Task` in a new contract. No `Either[String, _]`. No `toString` on a domain error. No `throw`, `return`, `null`, or `var` in a public contract. No `asInstanceOf`. No partial accessors. Clock comes from the effect environment.
- No new library and no compiler plugin. If a unit needs a coordinate outside the compiler closure the spec already names, stop and ask.
- `./histos fmt` on touched Scala before commit. sbt `test` is not a substitute for the gate.
- Hard units get a review on that diff before the PR is called done: P1, P2, P3, P5, P6, P8, and the whole change at P10.
- A discovery that would change what a user sees (intent, consent, frozen, offline, what gets rebuilt) stops for Russ. A discovery that only changes an internal type stays in the unit.
- Later units may be coded from the P1 commit once its gate is green locally. Their PRs rebase onto `main` after P1 merges. They do not import types that exist only on a sibling branch.

## Out of scope

Stable ABI shims. Scala.js or Native plugin execution. Producer-build replay. A plugin sandbox. A new dependency or compiler plugin. Unrelated test rewrites. A release. Editing the specular repo. Splitting P1 so later waves share unpublished types.

## Now

P0 is on `main`. The spec is `c60ee8d`. This board is `ce6fd7f`. P1 is on `main` as `8f74a2e` (#216). P2 is on `main` as `0073b37` (#217). P3 is on `main` as `f60ae81` (#218). P5 is on `main` as `86e0bc1` (#219). P4 is on `main` as `250c089` (#220). P6 is on `main` as `23037d9` (#221). P7 is on `main` as `82303e8` (#222). P9 is on `main` as `db3a44c` (#223). P8 is on `main` as `b2c9632` (#224). P10 is on `main` as `8dbccf1` (#225). The pin moves in #226 to `0.4.4-8c61390d3c7a-SNAPSHOT`, packed from `8c61390`. The 0.5.0 pin moves from `0.5.0-a67fddcd62fb-SNAPSHOT`, packed from `a67fddc`, to `0.5.0-7c073bd2f203-SNAPSHOT`, packed from `7c073bd2`. The launcher timestamp is `0.5.0-7c073bd2f203-20261117.233941-1`. The pointer `0.5.0-SNAPSHOT` moved to `0.5.0-20261117.234019-6` and is not the pin. M1, M4, M5, and M6 are merged. Architecture rows A5 through A8 stay open. Later units must not invent the behaviors named in the P1 notes.

## Units

| Unit | Depends | Branch | Worktree | PR | Status | Gate | Result |
| --- | --- | --- | --- | --- | --- | --- | --- |
| P0 | none | direct to main | this checkout | none | merged | docs only | spec `c60ee8d`; board `ce6fd7f` |
| P1 | P0 | `histos/plugin-p1` | `plug-p1` | #216 | merged | `./histos compile api wire engine`; `./histos test api wire engine --only '*PluginContractsSpec*' '*PluginWireSpec*' '*ContractModelsSpec*'`; `./histos fmt --check` | merged as `8f74a2e` (#216). CI run 43931858280 on `7395b60` passed 7 failed 0, no test-clock warning. Local gate exit 0 on `80e20c8`. |
| P2 | P1 on main | `histos/plugin-p2` | `plug-p2` | #217 | merged | `./histos compile api plugin-testkit`; `./histos test api plugin-testkit --only '*BuildServicesSpec*' '*PluginHarnessSpec*'`; `./histos fmt --check` | merged as `0073b37` (#217). CI on `e7c4979` passed 7 failed 0, no test-clock warning. Local gate exit 0 on `c7ea9e6`. |
| P3 | P1 on main | `histos/plugin-p3` | `plug-p3` | #218 | merged | `./histos compile engine`; `./histos test engine --only '*LockTransactionSpec*' '*LockSnapshotSpec*'`; `./histos fmt --check` | merged as `f60ae81` (#218). CI run 43951028491 on `b5bf8f8` passed 7 failed 0, no test-clock warning. Concurrent writer passed. |
| P5 | P1 on main | `histos/plugin-p5` | `plug-p5` | #219 | merged | `./histos compile engine`; `./histos test engine --only '*PluginCompileCacheSpec*' '*HostApiInputsSpec*' '*HostCompilerBootstrapSpec*' '*PluginStageInputsSpec*'`; `./histos fmt --check` | merged as `86e0bc1` (#219). CI run 43970363195 on `81232e3` passed 7 failed 0, no test-clock warning. Local gate exit 0 on `637c3e7` (13 passed). |
| P4 | P5 on main | `histos/plugin-p4` | `plug-p4` | #220 | merged | `./histos compile engine`; `./histos test engine --only '*PluginPublicationSpec*' '*PluginPackageKeySpec*'`; `./histos fmt --check` | merged as `250c089` (#220). CI run 43984700796 on `07441f0` passed 7 failed 0, no test-clock warning. Local gate exit 0 on `e0186e8` (14 passed). The cell snapshot published. |
| P6 | P2, P3, P4, P5 on main | `histos/plugin-p6` | `plug-p6` | #221 | merged | `./histos compile engine`; `./histos test engine --only '*PluginBootstrapSpec*' '*PluginClosureSpec*' '*PluginLifetimeSpec*'`; `./histos fmt --check` | merged as `23037d9` (#221). CI run 44006796848 on `82ffd50` passed 7 failed 0, no test-clock warning. Local gate exit 0 on `610c21e` (23 passed). |
| P7 | P6 on main | `histos/plugin-p7` | `plug-p7` | #222 | merged | `./histos compile engine wire`; `./histos test engine --only '*PluginTaskIdentitySpec*'`; `./histos fmt --check`; real loader manifest before merge | merged as `82303e8` (#222). CI run 44012856100 on `79f6bc1` passed 7 failed 0, no test-clock warning. Local gate exit 0 on `62b151c` (4 passed). The loader seal reached the task. |
| P8 | P3, P6, P7 on main | `histos/plugin-p8` | `plug-p8` | #224 | merged | `./histos compile app wire client`; `./histos test app wire --only '*LockInvocationSpec*' '*LockOfferSpec*' '*LockPolicySpec*' '*RunCallSpec*' '*BatchRunSpec*'`; `./histos fmt --check` | merged as `b2c9632` (#224). CI run 44027154861 on `2d3023c` passed 7 failed 0, no test-clock warning. Local JVM gate: app 20 passed, wire 6 passed, `./histos fmt --check` unformatted 0. `./histos run doc` exited 0 on `2d3023c`. |
| P9 | P2, P4, P6 on main | `histos/plugin-p9` | `plug-p9` | #223 | merged | tracked link check; `./histos fmt --check`; `./histos run doc` | merged as `db3a44c` (#223). CI run 44015702128 on `e5c5b3b` passed 7 failed 0. The test job was `./histos test --affected '[]'`. No test-clock warning. Local link scan 84 links, 0 missing. `./histos run doc` exited 0. |
| P10 | P2 through P9 | `histos/plugin-p10` | `plug-p10` | #225 | merged | see the P10 brief | merged as `8dbccf1` (#225). CI run 44041175250 on `29b5de7` passed 7 failed 0, no test-clock warning. Pin stayed `0.4.2`. A5 through A8 stayed open. Local gate: `PluginArchitectureIntegrationSpec` 5 passed, `./histos fmt`, `./histos workflow --check`, `./histos run doc` exited 0. |

- [x] P0. Spec and this board on `main` (`c60ee8d`, `ce6fd7f`)
- [x] P1. Contracts, codecs, fixtures. Merged as #216 on `8f74a2e`. CI run 43931858280 on `7395b60`. Local gate: `./histos compile api wire engine`; `./histos test api wire engine --only '*PluginContractsSpec*' '*PluginWireSpec*' '*ContractModelsSpec*'`; `./histos fmt --check` on `80e20c8`
- [x] P2. `BuildServices` and `plugin-testkit`. Merged as #217 on `0073b37`. CI on `e7c4979`. Local gate on `c7ea9e6`.
- [x] P3. Lock transaction with a fake preparer. Merged as #218 on `f60ae81`. CI run 43951028491 on `b5bf8f8`.
- [x] P5. Host compiler, snapshot capture, cache, stage worker. Merged as #219 on `86e0bc1`. CI run 43970363195 on `81232e3`. Local gate on `637c3e7`: `./histos compile engine`; `./histos test engine --only '*PluginCompileCacheSpec*' '*HostApiInputsSpec*' '*HostCompilerBootstrapSpec*' '*PluginStageInputsSpec*'`; `./histos fmt --check`
- [x] P4. Publication and package identity, rebased onto P5, real snapshot in the suite. Merged as #220 on `250c089`. CI run 43984700796 on `07441f0`. Local gate on `e0186e8`: `./histos compile engine`; `./histos test engine --only '*PluginPublicationSpec*' '*PluginPackageKeySpec*'`; `./histos fmt --check`
- [x] P6. Bootstrap loader and closure. Merged as #221 on `23037d9`. CI run 44006796848 on `82ffd50`. Local gate on `610c21e`: `./histos compile engine`; `./histos test engine --only '*PluginBootstrapSpec*' '*PluginClosureSpec*' '*PluginLifetimeSpec*'`; `./histos fmt --check`
- [x] P7. Task identity, rebased onto P6, real manifest in the suite. Merged as #222 on `82303e8`. CI run 44012856100 on `79f6bc1`. Local gate on `62b151c`: `./histos compile engine wire`; `./histos test engine --only '*PluginTaskIdentitySpec*'`; `./histos fmt --check`
- [x] P8. Client envelope, offers, generation leases. Merged as #224 on `b2c9632`. CI run 44027154861 on `2d3023c`. Local gate: `./histos compile app wire client`; `./histos test app wire --only '*LockInvocationSpec*' '*LockOfferSpec*' '*LockPolicySpec*' '*RunCallSpec*' '*BatchRunSpec*'`; `./histos fmt --check`. `./histos run doc` exited 0 on `2d3023c`.
- [x] P9. Author docs and rendered spec review. Merged as #223 on `db3a44c`. CI run 44015702128 on `e5c5b3b`. Local gate: tracked link check, `./histos fmt --check`, `./histos run doc`
- [x] P10. Installed host and wiring retirement. Merged as #225 on `8dbccf1`. CI run 44041175250 on `29b5de7`. Pin stayed `0.4.2`. A5 through A8 stayed open. Local gate: `PluginArchitectureIntegrationSpec` 5 passed, `./histos fmt`, `./histos workflow --check`, `./histos run doc` exited 0.

## Ledger

| ID | Owner | Red | Green | Closed |
| --- | --- | --- | --- | --- |
| R1 task cache, empty artifacts and mixed provenance | P7 | | | [ ] |
| R2 package key ignores injected stamp bytes | P4 | | | [ ] |
| R3 substituted host API has no input hash | P5 | | | [ ] |
| R4 published sources omit generated replacements | P4 | | | [ ] |
| R5 compile-only inputs absent from the recipe | P4 | | | [ ] |
| R6 only the root stamp is checked | P6 | | | [ ] |
| R7 local plugin compile omits external jars | P6 | | | [ ] |
| R8 temporary compile lacks settings, resources, and a persistent cache | P5 primitive, P6 adopts it | | | [ ] |
| R9 `Any`/`Nothing` macro contracts and setup `toString` | P1 contracts, P2 activation, P10 retires the macro | | dirty `0d9c837`: macro files deleted; wiring file is not compiled | [ ] |
| R10 `readBuilt` scope ends before retained values | P2 builder, P6 live loader | | | [ ] |
| R11 export whitelist omits theme and step | P6 | | | [ ] |
| R12 `CompileInputs.remember` uses `ClockLive` | P5 | | | [ ] |
| R13 `PluginRuntime.bind` flattens non-task faults | P7 | | | [ ] |
| R14 `definesHost` treats source text as host identity | P6 | | | [ ] |
| A1 catalog intent inherited by main | P1 data, P6 proof | | | [ ] |
| A2 early strategy file before remaining loads | P1 data, P6 proof | | | [ ] |
| A3 registration key is the capability interface | P2 | | | [ ] |
| A4 recipe, TASTy direction, and closure | P4, P5, P6 | | | [ ] |
| A5 consent, write, adoption, resume | P3 store, P8 and P10 once-only | | dirty `0d9c837`: local lock re-run, not an architecture close | [ ] |
| A6 caller context, not server env or stdin | P8 | | dirty `0d9c837`: local caller re-run, not an architecture close | [ ] |
| A7 shim and pin may run old code | P10 | | dirty `0d9c837`: local jars are not a channel publish and not A7 | [ ] |
| A8 host compiler tools are not solved from the project lock | P5, P10 packages them | | dirty `0d9c837`: local manifest fingerprint, not an architecture close | [ ] |

## Briefs

An implementer reads `Agents.md`, [plugin-architecture.md](plugin-architecture.md), and the one brief below. Hand them that brief by name.

### P1. Shared contracts

Wave 1. One worktree. Merge to `main` before any later PR opens.

Owns API graph, resolution, catalog, lock, policy, build-service, recipe, publication, and error contracts. `GenerationAmbient`, invocation request types, and both event channels as data. Explicit `Unrecorded` and `Unverified` absence. Files:

- `api/src/histos/plugin/ArtifactManifest.scala`
- `api/src/histos/plugin/StageContract.scala`
- `wire/src/histos/zinc/CompilationSnapshot.scala` and the `CompileFeed` capture state
- host compiler manifest and session data in the wire toolchain model
- provider trait in `engine/src/histos/toolchain/HostCompilerInputs.scala`
- activation and verified-manifest data, plus the engine provenance result shape
- `engine/test/histos/contracts/ContractFixtures.scala`

Wire codecs and nested faults for those families (`LoadError`, publish data, `CallResult`, protocol additions the contracts need). Engine files in this PR are pure models and provider traits.

Must not touch loader, compiler, or publisher behavior. API does not import engine or wire. `Plan[S, E]` is types here. P2 implements registration. If the associated-type builder does not compile as the examples require, a `/tmp` scala-cli probe settles the Scala 3 shape before the signatures change. The public examples stay the contract. Nest new fault families the way `PluginGraphFault` is nested.

Ledger: R9 contracts, A1 and A2 data. Loading proof is P6. No red SHA: there was no prior failing test, and this unit did not manufacture one. The three specs landed with the types.

Review: dependency direction, codec round-trips, `Unrecorded` is absence rather than a successful capture.

P1 notes. These are boundaries, not new product behavior.

- `ProjectGraph.resolution` still defaults to `Resolution.Solve`. `intent` defaults to `ResolutionIntent.Inherit`. `Projects.locked` and `Organized.locked` set both `resolution = Resolution.Lock` and `intent = ResolutionIntent.Select(Resolution.Lock)`. The loader still reads `resolution`. Normalizing `Inherit` is P6.
- `PluginRuntime` fills `RunRequest` with a stand-in caller: `ExecutionKind.Local`, `Interaction.Unattended`, `OfflineSelection.Online`, `ClientId("plugin-runtime")`, `CommandIdentity.Task`, and `PluginEvents.closedInvocation`. P8 replaces that caller. There is no third `ExecutionKind` and no unscoped origin.
- `Plan.activate` and `StrategyPlan.activate` refuse a duplicate slot before `materialize`, so activation is not an empty success. P2 owns registration, generation-scope lifetime, concurrent callers, and the harness. There is no `ServiceActivationError.Unavailable`.
- Service slots and task slots are duplicate-checked separately. `.add[P].tasks[P]` constructs two instances, and `.add` accepts an intersection key. P2 rejects an intersection key and constructs one instance for `.add[P].tasks[P]`.
- `InvocationEnvelope`, lock offer, reply, committed, blocked, and `LockEvent` are data and codecs. The live build-event channel is unchanged. The resolve CLI does not gain refresh.
- A stage network restriction is prohibited in v1. There is no network-enabled stage mode.
- `HostCompilerInputs` is a provider trait. There is no live layer.
- The API digest is a pure SHA-256 so an API native test does not link `libcrypto`. `PluginContractsSpec` checks the published vectors.
- After one native compile, Scala 3 leaves the nscplugin zip file system closed in that histos JVM. A later native compile then warns `No plugin in path` and emits class files with no NIR. Reboot so the compile that must emit NIR is the first native compile in that JVM. Once analysis is stored, a later command skips and does not open the plugin. This unit does not change the compiler.
- `BuildServices.default.add[A]` yields `Plan[NoExtraServices & A, E]`. The marker stays in the intersection. Examples do not ascribe `S`.
- Built-in loading policies are pure functions of `BindingMatch`. They do not special-case offline.

### P2. Typed activation and the test kit

Wave 2. Open with P3 and P5 after P1 is on `main`.

Owns `BuildServices` and `StrategyServices`: `.withLocks`, `.add[A]`, `.tasks[P]`, interface `Tag` projection, duplicate detection, `Plan.activate` in the generation `Scope`. `plugin-testkit` depends on API and ZIO Test only. Root `histos.scala` gains that module and does not adopt the new catalog DSL.

Must not edit `Loader`.

Proof. Positive examples from the spec, including `CheckedReviewLabel` inferring `LabelSetupError` with no ascription. Negative compiler fixtures: missing `ReviewLabel`, a late main loading override, a generation layer that requires `InvocationContext` or `PluginEvents.Invocation`, and an intersection registration key. Runtime: duplicate `.add` fails before construction, an empty label keeps the previous generation and surfaces `Noted`, two concurrent requests on one constructed service see their own command and their own event queue. `.add[P].tasks[P]` constructs one instance. Scoped setup interruption runs finalizers. P1 left the intersection key and the two-instance construction open.

Ledger: R9 activation, A3, R10 at the builder, AR2 at the builder. Live loader proof of R10 stays P6.

### P3. Lock transaction

Wave 2. Open with P2 and P5.

Owns versioned pins, `ResolutionSnapshot`, the necessary-closure diff, `LockStore`, `LockCoordinator`, and the repair state machine, under `engine/src/histos/resolve/`. Fake preparer and fake generation owner.

Must not edit invocation envelopes or client replies. Those are P8. Consume the wire types P1 already published.

Proof. Decline, solve failure, and interruption before rename leave the previous lock bytes, including unrelated snapshot locators. Interruption after rename recovers the new revision and does not report a rollback. Stale proposal fails. Duplicate accept does not run the suspended command twice. Concurrent writer loses. Atomic replace failure is typed, with no ordinary-move fallback. Frozen drift performs zero solves. `LocalSolve` publishes an ephemeral snapshot and does not write the tracked lock. Aggregation: one Reject or Frozen drift blocks, one LocalSolve makes the invocation ephemeral, OfferRepair is one consent for the whole diff, Update requires every repaired phase to allow it.

Ledger: A5 at the store. Daemon once-only is P8 and P10.

Review: concurrency and interruption, separately from the feature diff.

### P5. Host compiler and cache identity

Wave 2. Open with P2 and P3. Lands before P4.

Owns real `CompilationSnapshot` capture. `HostCompilerInputs.materialize` from a verified distribution payload, with no `Resolve` and no `LockedTools`. A plugin compile route that takes the session's compiler, bridge, and support jars and does not call `CellTools.gather` or `Scala3.jars`. The complete compile key, including actual substituted API bytes. Sealed compiler-visible outputs. `KnownStageRegistry`. A scope-owned worker JVM: cleared environment, private recipe root, supervised interruption. Single-flight cache. `CompileInputs` uses the effect clock.

Must not edit `PackageKey` or `LocalPublish`. P10 packages the real distribution payload. Fixtures here materialize pinned tool artifacts before the measured phase.

Proof.

- A host-only API byte change misses the cache (R3), red first.
- A second identical compile calls the compiler zero times (R8 primitive).
- The test clock covers the remember path (R12). No `ClockLive` left on this seam.
- A mixed Scala and Java recipe produced under compiler A rebuilds with supplied B compiler and bridge. The project lock has no B compiler cell. Decisive Scala-to-Java output, zero `resolve`, `resolveTool`, and `resolvePlugin`, lock bytes unchanged. A missing bridge is a typed `HostCompilerError` and still zero solves.
- A helper whose erased class products match, `type Label = "aa"` versus `"bb"`, forces the consumer `valueOf` to recompile and return the new literal. Semantic equality of the class products is a fixture precondition.
- One producer-declared stage reads a declared file and a public env binding. Values A and B change the key and the output. A missing required binding is a typed refusal. Caller env does not change a frozen published input. Offline refuses that stage before the worker starts, and the fixture's network probe is not called. A HostKnown-only recipe succeeds offline with zero downloads. Source-label output is stable across scratch roots.

Ledger: R3, R8 primitive, R12, A4 compiler half, A8 materialization.

### P4. Rebuild publication and package identity

Wave 2 development against P1 fixtures. Open the PR only after rebase onto P5, with one real `GraphCompile` snapshot pushed through publication.

Owns `plugin.json` plus `META-INF/histos/rebuild/...` in the ordinary binary jar. Binary-only mode keeps binding metadata and makes no rebuild claim. Recipe v1 validation (`Unsupported` and `Incomplete` before any upload). Manifest-derived `PackageKey` for plugin and helper outputs. Semantic `ProductDigest` stays the policy everywhere this unit does not own.

Must not edit `GraphCompile`.

Proof. Red first: two outputs with equal `ProductDigest` and different TASTy bytes must not share a package key, including binary-only. A TASTy entry-byte change changes the key without compiling a corrupt TASTy. Stamp-only metadata changes the key (R2). Generated replacements from `Sources.prefer` are in the bundle (R4). Compile-only inputs are in the recipe and absent from the consumer POM (R5). An unknown or incomplete stage contract, and an unclassified or secret stage input, refuse publication with zero uploads. `Unrecorded` capture cannot publish a rebuildable recipe. Changed sources after the snapshot fail publication.

Develop gate, on the P1 base: `./histos compile engine`; `./histos test engine --only '*PluginPublicationSpec*' '*PluginPackageKeySpec*'`; `./histos fmt --check`.

Ledger: R2, R4, R5, A4 publication half.

### P6. Bootstrap loader

Wave 3. Starts when P2, P3, P4, and P5 are on `main`.

Owns the first-load protocol in `Loader`, `PluginShips`, `PluginModules`, `Boundary`, `BuildLoader`, `BuildFile`, and `LoadedBuild`. One retained catalog object and one shared loader from catalog through strategies through main. Core-default seed, then optional `histos.strategies.scala`, then custom policy for the remaining closure, then main. No required `histos.wiring.scala` on this path. A repository with no catalog still compiles `histos.scala` and inherits Solve. An explicit main `Resolution` that conflicts with the catalog is `BootstrapError.ResolutionConflict`. Lock file presence does not select intent.

The export manifest comes from the API artifact's packages, including theme and step. Engine packages stay refused. Host-provided API and runtime use verified identity. A comment that contains `trait HistosBuild` does not suppress compilation.

Must not edit BSP, watch fingerprints, or scaffold templates. They still name `histos.wiring.scala` until P8 and P10.

Proof. Red first for R6, R7, R10, R11, and R14. Then: empty-cache first run, core seed before custom policy, custom policy before the remaining plugin's first solve, frozen miss with zero solves. A local module plus an external jar loads. Helper-only API drift rebuilds or, under binary-only, refuses with a path. Ordinary binary bytes stay pinned. A classpath conflict names both origins. The spec's author fixtures compile. Mixed Scala and Java A-to-B rebuild goes through the loader with the same zero-solve bar as P5, plus a typed missing-bundle failure. A second identical load calls the compiler zero times. A service acquired in the generation scope is alive for a task and closed when the last lease releases. A discarded candidate closes. A late main `.withPluginLoading` is a typed rejection.

Ledger: R6, R7, R8 adoption, R10, R11, R14, A1, A2, A4 closure.

Review: resource lifetime and classloader parent delegation, separately.

### P7. Task provenance

Wave 3. Develop against P1 through P5 fixtures. `Unverified` is refused and is not rewritten into an empty artifact list. Open the PR only after a rebase onto P6 and one real loader-to-task verified manifest.

Owns `PluginRuntime`, plugin inputs, cells, and steps, and `TaskError` in `wire/src/histos/task/Ids.scala`. This wave is the only one that edits that file.

Proof. Red first: old plugin bytes or an empty artifact list reuse a stale task output (R1). Then the key includes defining closure, service inputs, args, and host binding, and the output matches the new bytes. Compile and scheduler failures stay distinguishable through bind (R13). No `Failed(other.message)` flattening on this path.

Ledger: R1, R13.

### P8. Invocation parity

Wave 4. Depends on P3, P6, and P7. May be open beside P9.

Owns the common `InvocationEnvelope` through CLI, daemon, MCP, BSP, watch, and the native client. `--lock=offer|update|local|frozen` in the grammar. Structured offer and reply. Consent is never `RunInput.Stdin` and never the server environment. The native client sends CI and interaction on the fast path, and falls back to the JVM for a flag it cannot encode.

Proof. Parser precedence from the spec, including `resolve --update` combined with frozen or local (rejected), offer (full proposal), and update (valid). `resolve` and `resolve --check` reject `--lock`. Refresh rejects frozen, local, and combination with update or check. A CLI with a tty can accept. A redirected CLI returns `ConsentRequired`, the summary, and the rerun command, and the daemon does not read stdin. MCP and BSP without negotiated consent return a diagnosis, not a prompt. Watch resumes one accepted iteration and cancels an obsolete proposal. Two commands from two callers on one retained generation: one construction, each sees its own command and execution kind, each receives only its own invocation events, handles close on disconnect. Generation diagnostics are a separate channel. An ephemeral local snapshot does not satisfy a frozen caller. Program stdin still feeds the program.

Reuse `RunCallSpec` and `BatchRunSpec`. D16 applies if those need to learn the envelope.

Ledger: A5 once-only, A6.

### P9. Author docs

Wave 4. Depends on P2, P4, and P6. May be open beside P8.

Owns the README plugin and lock sections, an author guide, and fixture-backed examples. Snippets are the compiler fixtures, not a second copy. The architecture doc's status stays proposed until P10. Rendered GitHub Markdown review of `docs/plugin-architecture.md` happens on this PR. `./histos run doc` is the Scala API-doc check only.

Must not edit scaffold sources. Those are P10.

Ledger: none. The rendered-spec box in the architecture doc waits for P10 along with the rest of that checklist.

Rendered review, recorded here and not as a checked completion box. `gh api --method POST /markdown` with `mode=gfm` and `context=early-effect/histos` rendered `docs/plugin-architecture.md`. 39 destination-relative links resolve. Headings are unique. Eleven fences are balanced and render as Scala or text. Tables have a separator row and consistent columns. A `<` inside a code span, and `\|` inside a table cell, render as `<` and `|`. Nine author fences equal the `Author` strings in `PluginBootstrapSpec`. Left for P10 with that checklist: the opening still says no compiler verification has been performed; the service table still writes `def add[A: Tag]` where the landed method is `inline def add[A](using Tag[A])`; `ReviewedLocksSpec` and `examples/expected-lock-repair.txt` are not tracked files. The Markdown API response has no heading `id` attributes. Blob anchors stay unique because the headings are unique.

### P10. Assembled host

Wave 5. Depends on P2 through P9.

Owns distribution packaging of the host compiler manifest and the original split jars under `bootstrap/compiler/`, taken from the launcher cell's compilation snapshot. Install-time verification. Scaffold, BSP mappings, and watch fingerprints that still mention wiring. Retirement of `WiringSource` and the bundle macro. Root build adoption only if the assembled host needs a catalog. This repo's `histos.scala` has no plugins today. Do not invent one. `PluginArchitectureIntegrationSpec`.

Sequence.

1. With the baseline `0.4.2` host, compile the assembled tree and `./histos publish local` into an isolated home. Record `./histos version`, the jar digest, and `HostCompilerManifest`.
2. If that host cannot compile the assembled API, stop and ask for a source-bootstrap path. Do not invent compiler flags, and do not claim the old host tested the new loader.
3. Install that local host as B. Clear derived plugin output. Rebuild A's mixed Scala and Java plugin. The project lock has no B compiler cell. Assert executable output, zero resolver solves, unchanged lock bytes, and a typed failure for a missing or corrupt bridge with no network repair.
4. The same installed host proves binary and helper paths, typed services and tasks, TASTy seal identity, declared stage file and env key change, offline refusal and HostKnown success, two callers with isolated events, one repair then one resume, snapshot retention and explicit refresh, frozen and offline diagnoses, lease cleanup, and a second load with zero compiler calls.
5. Only then move the pin and run `workflow --update` for the files that pin owns. Snapshot channel only. `Ship.next` stays `0.4.4` unless a separate release instruction exists. No release workflow.

Gate, after the local pin proof.

- `./histos fmt`
- `./histos compile api wire engine app plugin-testkit client`
- `./histos test app --only '*PluginArchitectureIntegrationSpec*'`
- `./histos test api wire engine app plugin-testkit` without `--quick`
- `./histos fmt --check`
- `./histos workflow --check`
- `./histos run doc`

Record host identity, commands, and the decisive fixture outputs in this file. Then close the matching rows in the architecture doc's ledger and completion checklist from those proofs, or name the owner decision that blocked a row.

Ledger: R9 closes in the architecture doc because the bundle macro is retired. A5-A8 stay open there. The work-plan Closed boxes stay open until a PR number exists. A7 is not checked.

### P10 record

Local evidence was taken on dirty `0d9c83706be477162c18bcf8a1a76246e7eab557`. The PR is #225. `Ship("histos").next` stayed `0.4.4`. Root `histos.scala` gained no plugin. `ReviewedLocksSpec` and `examples/expected-lock-repair.txt` were not created.

After review, `histos.version` is `0.4.2` again. `0.4.4-0d9c83706be4-SNAPSHOT` names parent commit `0d9c837` and is not on the snapshot channel. Maven Central and Sonatype both 404 that coordinate. CI cannot install it. The setup action SHA stays `4a49a640bc8345737c22160b4e67b028d61cd908`, the one already on main. This diff does not publish, tag, or release. `histos update` was not run.

Local jars are evidence of a local pack. They are not a channel publish and not A7.

- J1, packed by the 0.4.2 process: `dist/histos.jar` sha256 `8d886289348fb00f23e325d5265bd790bdda05761be96aaaec77f69e22d2ac7c`. `bootstrap/compiler/` was absent, so `HostCompilerManifest` was absent. The ivy library jar `~/.ivy2/local/rocks.earlyeffect/histos/0.4.4-0d9c83706be4-SNAPSHOT/jars/histos.jar` is a different file, sha256 `23dbaeb64fce626d590f4f385c3204f455282a237e2899a8703530589384de52`, and also has no compiler payload.
- J2, packed by J1: `dist/histos.jar` sha256 `da4a6f8399e1b8fd47430ec5163fd20f709830367f089d05a7c91275d06c5b5e`. `bootstrap/compiler/manifest.json` is schema 1, Scala `3.10.1`, JDK `25.0.5.0.3`, 21 tools, roles Compiler, Bridge, Interfaces, Runtime, and Support. Fingerprint `0ec0c17b8f397a30cf33ffd7c48acd90c2cc1eefb728ac6691708a82d3d2ee66`. Bridge digest `5b4e5f158d08206b357ccb3d4640af7b5969d73f7b8d034cc69e88f8fa5806dc`.

`PluginArchitectureIntegrationSpec` names, as local evidence and not A7: the packaged closure materializes and the bridge digest matches the jar; a missing bridge is `EntryAbsent` and does not solve; a corrupt bridge is `BadDigest` and does not solve; the packaged host rebuilds a mixed module, then a second load adds no compiler starts; a lone `histos.wiring.scala` is not compiled. The checkout `dependencies.lock` contains `tool compiler 3.10.1 jvm`. The integration fixture lock is the bytes `fixture\n` and contains no compiler cell.

The combined `./histos test api wire engine app plugin-testkit` exited 1 on `ld: library 'crypto' not found` before engine finished. `./histos test engine` then exited 0, 111 passed. JDK `Unsafe` warnings were printed by the 0.4.2 runner. They are not claimed gone.

Rows left open, and why:

- P10 checkbox and the work-plan Closed boxes stay open until merge. The PR is #225. A7 is not checked.
- Architecture A5-A8 stay open. The local jars are not those proofs.
- Architecture R9 is checked. The bundle macro and `WiringSource` are deleted, and a lone `histos.wiring.scala` is not compiled.
- Completion native client and MCP, the P4/P7 line, and the review line stay open.
- `.bsp/histos.json` was restored. Not part of this diff.

Snapshot `0.4.4-8c61390d3c7a-SNAPSHOT` was published from clean `8c61390`. The launcher timestamp is `0.4.4-8c61390d3c7a-20261117.120003-1`. The pointer `0.4.4-SNAPSHOT` moved to `0.4.4-20261117.120023-2` and is not the pin. `Ship.next` stays `0.4.4`. No release was cut.

## Remaining milestones

These are the next plans. None of them is authorized by the pin PR. A milestone that changes what a command writes stops for Russ before that behavior is chosen.

- M1. Daemon consent (A5). Merged as `b120b2df` (#232). CI run 44198375987 on `ff643ed3` ran `./histos test --affected '["app"]'`: 47 passed, 0 failed, no test-clock warning. Ordinary commands do not call `prepare`. A dependency lock is `Corrupt` and is not rewritten. The architecture checklist row stays open.
- M2. Refresh. `resolve --refresh` currently does not revalidate and says so. It revalidates retained snapshot locators through the session coordinator once M1 is on main. The parser still rejects frozen, local, and a combination with update or check. A dependency lock stays `Corrupt` and is not rewritten.
- M3. Published pin grammar. Do not invent the lock lines. After Russ names them, add `ReviewedLocksSpec` and `examples/expected-lock-repair.txt`, and compiler-prove the published examples.
- M4. Lock strategy context. Merged as `8caeeb74` (#229). CI run 44198380924 on `24d93882` ran `./histos test --affected '["engine","app"]'`: app 37 passed, engine 129 passed, 0 failed, no test-clock warning.
- M5. Compiler and runtime identity. Merged as `460ca294` (#230). CI run 44199493705 on `6e5377df` ran `./histos test --affected '["engine","app"]'`: app 47 passed, engine 130 passed, 0 failed, no test-clock warning.
- M6. Installed-host proof. Merged as `7c073bd2` (#231). CI run 44199500059 on `9614742a` ran `./histos test --affected '["app"]'`: 66 passed, 0 failed, no test-clock warning. The native client process was not spawned, so the architecture rows stay open.

Not scheduled: the `cache-seed` 404 on pushes to `main`, and the four JDK `Unsafe` lines from a runner that does not pass the allow flag.
