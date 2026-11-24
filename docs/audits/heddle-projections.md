# Heddle projection audit

The projection source of truth is `StageRoots` plus the validated `BuildDag`. Projection consumers must not reconstruct source layouts or module dependency closure.

## Consumer seams

- `LocalPublish.sourceJar` reads Main `StageFiles` and packages each `PackagedSource.entry`. The entry is independent of its physical path. `LocalPublish.runUsing(PublishCompiler[R, E](ensure))` asks the existing executor for selected Main stages, requires their real products and retains `ExecutionProducts.generatedRoots` through packaging in the same scope. Its error channel preserves adapter E, ExecutionError, LocalPublishError and StageRootError.
- `Affected.project` projects each module's Main/Test closure. Every consuming owner of a shared source/resource root or transitive producer input is selected. Invalid declarations remain typed; the compatibility `decide` adapter conservatively selects everything.
- `SourceWatch.project` carries File/Directory extents for roots and exclusions. It includes conventional/exact roots, dependency/producer inputs and Main closures for producer Module material inputs. Every declared generated output and `.histos` is excluded. Legacy unmanaged source roots are excluded too. Asset File inputs remain watchable unless owned by a generated output. Snapshot polling hashes bytes, uses the ambient clock and refuses input links. Integrators must retain the projection instead of converting it to the old path-list API.
- `Plan.project` returns the dependency-first ExecutionPlan and each compilation stage's inventory plus actual StageFiles, including resources, assets and provenance. Preview does not run producers. The caller supplies the leased generation view. Legacy preview methods also use the shared stage projection and preserve root/DAG errors.
- `Products.project` carries exact declared extents and reachable generator task IDs. Full graph clean includes all task outputs. `Products.namedUsing(GeneratedCleaner[R, E](clean))` retires generated products before ordinary module products. The capability must use the persistent PluginProducts instance and its ownership/lease validation. The projection never deletes declared generated paths directly.

## Audit ledger

| Closed | Category / finding | Owner and proof |
| --- | --- | --- |
| [x] | One-base source-jar scanner omitted extra/shared inputs. | H1d. Actual legacy ZIP inventory failed; actual shared Main projection ZIP inventory passes. |
| [x] | Scoped generated File path was omitted by the legacy packaging recipe. | H1d. Actual File source jar retains the explicit artifact entry from a provided physical generation and excludes adjacent files. |
| [x] | Longest-owner ranking selected only one owner of a shared root. | H1d. Actual red selected one; typed projection selects both owners and a Test-dependent consumer. |
| [x] | POM dependencies must stay Main-only. | H1d. Explicit Main edge query and real local-publisher POM read assert Main libraries/edges and absence of Test fixtures/libraries. |
| [x] | Watch omitted transitive producer inputs and could observe generated Directory descendants. | H1d. Filesystem snapshots plus TestClock polling assert source invalidation, File sibling exclusion and nested generated output exclusion. Module material input Main dependency closure has its own snapshot regression. |
| [x] | Preview scanner and module closure diverged from scoped graph semantics. | H1d. Actual shared generation/resource reads and producer nodes are asserted; missing generated view retains StageRootError. |
| [x] | Generated clean must respect exact File versus Directory extent and active leases. | H1d projection/capability seam. Actual PluginProducts generation/read lease rejects clean before ordinary products are removed; retirement removes the File and Directory while retaining a User.scala sibling. |
| [ ] | Unrecognized user content at a declared output can be deleted by this seed's PluginProducts.clean. | H1b/output owner and integrator. The capability must refuse unmanaged content and a File replaced by a Directory. Projection code cannot safely recreate product ownership metadata. |
| [x] | Totality, typed errors and constructor capability pinning. | H1d. Strict owned source/test compilation uses -Wunused:all -Werror. No new casts, Task, Unsafe, partial accessors, mutable public contracts, string errors or dependencies. Existing failures.head is a NonEmptyChunk. Other get calls are total graph/Map lookups. |
| [ ] | Expanded root/DAG errors through publish/release/preview/report facades. | Integrator. Normal engine compile reaches PublishRun.scala and ReleaseRun.scala error-channel adapters, which must adopt typed root errors. |
| [ ] | Actual generated source local publish and binary/Main classpath proof. | Integrator. Route through BuildExecution, inspect binary/source jars and POM, and prove Test fixture classes/resources never enter Main classpaths. The focused POM test uses a real source ZIP payload, not a binary compilation product. |
| [ ] | Actual BSP/Calls/watch entrypoints and preview server/process retirement. | Integrator. Adopt SourceWatch.Projection and leased StageProjection; retire servers/processes and release leases in their generation scopes. |

The moving pins and build/plugin metadata are unchanged. No dependency, release, remote or branch operation is required for this unit.
