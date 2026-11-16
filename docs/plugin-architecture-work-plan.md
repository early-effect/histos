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

Baseline for the spec's current-state table is `e0027a3`. The spec landed in `c60ee8d`. Pin in `build.properties` is `0.4.2`. `Ship.next` is `0.4.4`. Root `histos.scala` declares no plugins.

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

P0 is on `main`. The spec is `c60ee8d`. This board is the following commit. No implementation PR is open. Next unit is P1. Nothing is blocked on Russ.

## Units

| Unit | Depends | Branch | Worktree | PR | Status | Gate | Result |
| --- | --- | --- | --- | --- | --- | --- | --- |
| P0 | none | direct to main | this checkout | none | merged | docs only | spec `c60ee8d`; board is the commit that added this file |
| P1 | P0 | `histos/plugin-p1` | `plug-p1` | | pending | `./histos compile api wire engine`; `./histos test api wire engine --only '*PluginContractsSpec*' '*PluginWireSpec*' '*ContractModelsSpec*'`; `./histos fmt --check` | |
| P2 | P1 on main | `histos/plugin-p2` | `plug-p2` | | pending | `./histos compile api plugin-testkit`; `./histos test api plugin-testkit --only '*BuildServicesSpec*' '*PluginHarnessSpec*'`; `./histos fmt --check` | |
| P3 | P1 on main | `histos/plugin-p3` | `plug-p3` | | pending | `./histos compile engine`; `./histos test engine --only '*LockTransactionSpec*' '*LockSnapshotSpec*'`; `./histos fmt --check` | |
| P5 | P1 on main | `histos/plugin-p5` | `plug-p5` | | pending | `./histos compile engine`; `./histos test engine --only '*PluginCompileCacheSpec*' '*HostApiInputsSpec*' '*HostCompilerBootstrapSpec*' '*PluginStageInputsSpec*'`; `./histos fmt --check` | |
| P4 | P5 on main | `histos/plugin-p4` | `plug-p4` | | pending | same commands as the develop gate, after rebase onto P5, including one real `GraphCompile` snapshot | |
| P6 | P2, P3, P4, P5 on main | `histos/plugin-p6` | `plug-p6` | | pending | `./histos compile engine`; `./histos test engine --only '*PluginBootstrapSpec*' '*PluginClosureSpec*' '*PluginLifetimeSpec*'`; `./histos fmt --check` | |
| P7 | P6 on main | `histos/plugin-p7` | `plug-p7` | | pending | `./histos compile engine wire`; `./histos test engine --only '*PluginTaskIdentitySpec*'`; `./histos fmt --check`; real loader manifest before merge | |
| P8 | P3, P6, P7 on main | `histos/plugin-p8` | `plug-p8` | | pending | `./histos compile app wire client`; `./histos test app wire --only '*LockInvocationSpec*' '*LockOfferSpec*' '*LockPolicySpec*' '*RunCallSpec*' '*BatchRunSpec*'`; `./histos fmt --check` | |
| P9 | P2, P4, P6 on main | `histos/plugin-p9` | `plug-p9` | | pending | tracked link check; `./histos fmt --check`; `./histos run doc` | |
| P10 | P2 through P9 | `histos/plugin-p10` | `plug-p10` | | pending | see the P10 brief | |

- [x] P0. Spec and this board on `main` (`c60ee8d` plus the board commit)
- [ ] P1. Contracts, codecs, fixtures. Merged before wave 2 PRs open
- [ ] P2. `BuildServices` and `plugin-testkit`
- [ ] P3. Lock transaction with a fake preparer
- [ ] P5. Host compiler, snapshot capture, cache, stage worker
- [ ] P4. Publication and package identity, rebased onto P5, real snapshot in the suite
- [ ] P6. Bootstrap loader and closure
- [ ] P7. Task identity, rebased onto P6, real manifest in the suite
- [ ] P8. Client envelope, offers, generation leases
- [ ] P9. Author docs and rendered spec review
- [ ] P10. Installed host, migration, pin, full gates, architecture-doc ledger closed from this board

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
| R9 `Any`/`Nothing` macro contracts and setup `toString` | P1 contracts, P2 activation, P10 retires the macro | | | [ ] |
| R10 `readBuilt` scope ends before retained values | P2 builder, P6 live loader | | | [ ] |
| R11 export whitelist omits theme and step | P6 | | | [ ] |
| R12 `CompileInputs.remember` uses `ClockLive` | P5 | | | [ ] |
| R13 `PluginRuntime.bind` flattens non-task faults | P7 | | | [ ] |
| R14 `definesHost` treats source text as host identity | P6 | | | [ ] |
| A1 catalog intent inherited by main | P1 data, P6 proof | | | [ ] |
| A2 early strategy file before remaining loads | P1 data, P6 proof | | | [ ] |
| A3 registration key is the capability interface | P2 | | | [ ] |
| A4 recipe, TASTy direction, and closure | P4, P5, P6 | | | [ ] |
| A5 consent, write, adoption, resume | P3 store, P8 and P10 once-only | | | [ ] |
| A6 caller context, not server env or stdin | P8 | | | [ ] |
| A7 shim and pin may run old code | P10 | | | [ ] |
| A8 host compiler tools are not solved from the project lock | P5, P10 packages them | | | [ ] |

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

Ledger: R9 contracts, A1 and A2 data. Loading proof is P6.

Review: dependency direction, codec round-trips, `Unrecorded` is absence rather than a successful capture.

### P2. Typed activation and the test kit

Wave 2. Open with P3 and P5 after P1 is on `main`.

Owns `BuildServices` and `StrategyServices`: `.withLocks`, `.add[A]`, `.tasks[P]`, interface `Tag` projection, duplicate detection, `Plan.activate` in the generation `Scope`. `plugin-testkit` depends on API and ZIO Test only. Root `histos.scala` gains that module and does not adopt the new catalog DSL.

Must not edit `Loader`.

Proof. Positive examples from the spec, including `CheckedReviewLabel` inferring `LabelSetupError` with no ascription. Negative compiler fixtures: missing `ReviewLabel`, a late main loading override, a generation layer that requires `InvocationContext` or `PluginEvents.Invocation`. Runtime: duplicate `.add` fails before construction, an empty label keeps the previous generation and surfaces `Noted`, two concurrent requests on one constructed service see their own command and their own event queue. Construction count is one. Scoped setup interruption runs finalizers.

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

Ledger: A7, the installed repeat of A5, A8, and the A-to-B and repair gates. R9 closes when the bundle macro is retired.
