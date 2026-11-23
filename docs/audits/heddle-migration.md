# Heddle migration audit

Base: Histos `65a1f2fe`. H0 owns public contracts and graph validation. Runtime proofs remain with the units named below.

## H0 contract ledger

| Done | Category / finding | Owner / proof |
| --- | --- | --- |
| [x] | Main/Test cell stages, module test-to-test edges, scoped compile declarations | H0, `MigrationContractsSpec`, `LibrarySurfaceSpec` |
| [x] | File/Directory declarations, named staging lookup, typed missing/wrong-kind errors | H0, `MigrationContractsSpec` |
| [x] | Scoped emitted File/Directory sources/resources and indirect generation/link cycles | H0, validated graph regressions |
| [x] | Exact scoped platform roots and JVM/Native shared-root selection | H0, root persistence/refusal proofs |
| [x] | JS tool/module/task optimization cascade | H0, effective-mode and platform-evidence proofs |
| [x] | Nonempty platform selection, run defaults, typed escaped text-list parsing | H0, runtime and compile-refusal proofs |
| [x] | Native aggregate overlap declaration validation | H0, validated graph regression |
| [x] | JVM options and properties separated from application arguments | H0, typed contract and module persistence proofs |
| [x] | Typed verify test selection and JDK 25 runner floor | H0, graph refusal and compile-refusal proofs |
| [x] | Preserve legitimate test-complete Compile and TASTy-aware cross-version dependency policy | H0, positive docs graph and cross-version cycle/refusal proofs |

## H0 review ledger

| Done | Finding | Resolution / proof |
| --- | --- | --- |
| [x] | R1, observed scope and effective link mode default silently | Full CellClasspath and LinkedScript constructors require facts; named mainOnly/testComplete classpath factories have explicit semantics; compile-refusal regression reproduced red before the fix |
| [x] | R2, new nonconfiguration default arguments | Link, PluginOutput and RunRequest use companion omission overloads; pre-existing compiled/linked defaults and unrelated configuration defaults remain unchanged |
| [x] | R3, docs diamond does not cover source File ownership | JVM Main produces one source File consumed by JVM Test and JS Main; an independent task owns a sibling File; exact collisions and parent Directory ownership are rejected |
| [x] | R4, generated File consumption and resource naming are ambiguous | EmittedContent distinguishes SourceFile/SourceTree/ResourceFile/ResourceTree; existing ArtifactPath and ResourceName name exact entries; graph rejects declared-kind mismatches with a typed fault |
| [x] | R5, platform spelling duplicated | VerifyRun.Tests renders through Platform.token; all three canonical tokens are asserted |

## Runtime and integration ledger

These checkboxes do not claim completion from API declarations alone.

| Done | Category / finding | Owner / completion gate |
| --- | --- | --- |
| [ ] | Main/Test scheduling, emitted sources/resources, transitive generation and classpaths | H1/H2, cold compile and fixture visibility |
| [ ] | Shared roots in source jars, packaging, BSP, affected selection and watch | H1 |
| [ ] | File/Directory ownership hashing, restore, stale cleanup and transaction safety | H1, execution-boundary proofs |
| [ ] | JS effective-mode product identity, dependency content hashing and asynchronous ownership | H1 |
| [ ] | Native aggregate member-only dependencies/resources and once-only suite execution | H2/H3 |
| [ ] | Provider properties before child bootstrap main; VM options and argument forwarding | H2, child property assertion |
| [ ] | CLI/wire platform/list semantics and typed verify workflow tool planning | H3 |
| [ ] | Explicit wire JSON codecs for the new nested contract ADTs | H3/integrator, dependent compatibility suites |
| [ ] | Compiler flags applied and keyed on bootstrap/direct paths | H4 |
| [ ] | Per-artifact compatibility, typed fetch errors, incomplete verification | H4 |
| [ ] | Relay byte/hash/source integrity and generated browser assets | B1 |
| [ ] | Site directory publication, restore, failure/interruption safety | H1/S1 |
| [ ] | Docs scoped generation, marker/root hidden IO, preview/client parity | B2 |
| [ ] | Example property/classpath partial access and Native canary | B3 |
| [ ] | Complete artifact inventory and operational sbt callers | B0/B4/integrator |

## API audit

The existing `histos.plugin.SourceSet` already models Main/Test. It is reused rather than introducing a competing scope enum.

`TaskDep.Compile` legitimately supplies main and test classpaths to Specular. It remains test-complete. New scoped compilation is explicit, not a compatibility fallback.

`RepoPath` already rejects absolute paths, traversal, backslashes, and empty segments. Graph ownership validation rejects exact and ancestor overlaps across plugins. Filesystem symbolic-link checks and transactional output publication belong to H1.

The engine's existing `UpstreamCells` policy permits TASTy-compatible cross-version Scala 3 module dependencies. The declaration DAG considers exact upstream cells when present and otherwise potential same-series stages; actual TASTy acceptance stays at the engine boundary. H0 does not impose an incorrect exact-version restriction.

The new code contains no partial accessors, non-FFI casts, throwing/returning domain control flow, mutable contracts, unsafe runtime calls, internal Task aliases, or string error channels. `BuildNode.Task` is a dependency-node enum case, not `zio.Task`.

Adjacent broad HeddleApp `Any` signatures and Specular raw configuration APIs remain outside this migration. H0 does not edit H4's doctor service.

## Proof log

- Initial `./histos test api` failed before production changes. The executable graph regressions showed that generation cycles and duplicate output ownership were accepted.
- `./histos fmt` passed. No unrelated files were formatted.
- `./histos compile api` passed on JVM and Native after `./histos clean api` removed stale Native incremental definitions. The installed Native pin remained `0.5.13-20261122-fa4437d-SNAPSHOT`.
- `./histos test api` passed without quick mode: 59 JVM tests and 57 Native tests, zero failures or ignored tests. This includes 19 migration contract tests on each platform and the existing complete API suites.
- The focused `./histos test api --only '*MigrationContractsSpec*' --test overlaps` reproduced the same-scope overlapping-platform dependency defect before its corrective production change. The complete API run repeated that assertion after the fix.
- The dependent command `./histos test engine --only '*ContractModelsSpec*' --only '*ApiPinSpec*' --only '*PluginTaskIdentitySpec*' --only '*VerifyJobSpec*'` is blocked at `wire/src/histos/report/WireJson.scala:517`, deriving `JsonCodec[BuildError]` exceeds the default successive-inline depth. H3/integrator must register explicit codecs for the new nested ADTs, not increase the compiler limit or hide cases. The suites did not execute. H1 must also implement the new TaskDep cases and three-field Link patterns in its owned scheduling/runtime files before the assembled gate can close.

The provisional `9cb941ec` verification above completed successfully. No publication, branch change, push, PR, merge, tag, or dependency addition is part of H0.

## R1-R5 red/green evidence

- `./histos test api --only '*MigrationContractsSpec*'`, run `501088f6-0bb9-494d-8dc9-71869bad9d5f`, was red on JVM and Native before the R1 constructor fix. Both omitted-scope CellClasspath and omitted-mode LinkedScript applications still typechecked. The real generated-source File docs diamond and independently owned sibling passed in that same run.
- After introducing the typed emission ADT but before adding declared-kind validation, `./histos fmt && ./histos compile api && ./histos test api --only '*MigrationContractsSpec*' --test mismatches` compiled both API platforms and failed the focused test on both. Run `253aa15d-cd9d-4ca6-9d79-db98aa15779c` demonstrated all four wrong File/Directory bindings were accepted.
- Final `./histos fmt && ./histos compile api && ./histos test api && ./histos fmt --check && git diff --check` returned exit 0. Full API run `ab542909-9b41-4337-adb9-e43ce7df4e1b` executed 63 JVM tests and 61 Native tests, zero failures or ignored tests, including 23 migration contract tests per platform. No quick mode was used. Scala 3.10.1 and the installed Native pin were unchanged.
- R2 companion overloads and explicit result factories are covered by runtime value assertions and positive typing checks. R4 refusal checks reject absent/string resource names, escaping ResourceName literals, and the former RootKind-only scoped emission signature. Browser resource declarations preserve `apps-browser/counter-view.js` and `apps-browser/reference-host.js` as exact classpath entries.

File sources contribute only the selected file and their explicit package-relative ArtifactPath source-jar entry. File resources contribute only the selected file at their explicit ResourceName. Neither contributes, scans, or owns the parent directory. Directory source/resource trees retain entries relative to their declared root. H1 owns execution of these rules, not a new package parser. Its adapters must now supply observed result scope/mode explicitly and populate destinations for output-producing requests. The H3 wire codec and assembled runtime gates remain open.
