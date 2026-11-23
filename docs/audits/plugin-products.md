# Owned plugin products

## Publication contract

`PluginProducts` owns staging, sealing, publication, replay and cleanup. `ProductRequest` groups the repository, task declaration and invocation identity. Only `StagedOutputs.destinations` uses the public `OutputDestinations` contract. Those paths are invocation staging locations, never final output locations.

A File owns exactly its declared path. Its staging and fixed-generation locations retain the declared filename for suffix-sensitive tools. Named private namespaces use output-name digests rather than raw names, so case-insensitive filesystems cannot merge distinct destinations. Final aliases are revalidated after preparation and before the pointer switch. A Directory owns the complete tree beneath its declared path. Reports must match declaration names, repository-relative paths and kinds exactly, with no duplicates or omissions. Every directory, including an empty directory, is recorded. File bytes are hashed and persisted in `ContentStore`; a hash-only manifest is not a restorable product.

The version 2 manifest records typed declarations and sorted relative entries. Entries use `ProductEntryPath`, with the generic ResourceName filename domain (including spaces, `@` and `+`), not the narrower archive-specific ArtifactPath grammar. Absolute paths, empty segments, traversal, backslashes, drive prefixes and NUL are refused. ArtifactPath is unchanged. `OutputManifest.contentDigests` supplies the CAS keep set. Missing content is a cache miss. Corrupt content, unsafe paths, embedded links and malformed manifests remain typed failures.

Staging and cleaning reserve the first `.histos`, `.git` and `.bsp` segments case-insensitively. These are engine, repository-owner and BSP connection state, not plugin products. File clean unlinks only the exact regular file or alias and refuses a physical directory with a typed kind error. Directory clean can remove its complete owned tree. File clean never recursively traverses a wrong-kind tree.

### Transaction strategy

Each declaration set has one managed home under `.histos/products`. Public output paths are engine-managed symbolic links through that home's `current` pointer. Each generation contains all File and Directory products. Publication atomically replaces only the pointer, not a nonempty directory. There is no non-atomic rename fallback.

The task writes to a scoped staging tree. Individual helper filesystem operations finish before scoped cleanup can remove their paths; cancellation remains available between operations and while waiting at the pre-switch hook. Sealing copies a verified complete snapshot into a private prepared tree and persists its bytes. Promotion prepares an unpublished generation, then switches the single pointer in a bounded uninterruptible region. Failure or interruption before that switch removes the unpublished generation and newly installed aliases. Previously published content and the reload stamp are untouched. Cancellation after the switch observes a complete committed generation, never a partial tree.

Old successful generations remain until explicit clean. This is deliberate: a preview request that already canonicalized a path can finish without losing its generation. `CommittedOutputs.read` additionally gives scoped readers a fixed `ProductLocations` snapshot. Its lease prevents clean while active. Clean and lease acquisition share a gate, so clean cannot race lease registration. One persistent helper instance under one exclusive repository owner must cover production, replay, clean, generated readers, graph reload and preview retirement. It must outlive all readers. Semaphores and lease counts are in-process coordination, not cross-process protection. Another instance or process must not mutate or clean this repository while these readers are active. Supporting independent concurrent repository owners would require a separate interprocess ownership/lease protocol.

Engine-managed output-root links are distinct from embedded links. The pointer target must be a real generation in the managed home, and declared output aliases must have the exact engine-created target. Products within a generation cannot contain links, and output ancestors cannot be links.

### Existing unmanaged directories

An existing unmanaged output is refused with `ProductError.Unmanaged`. The helper does not silently perform backup-then-rename adoption. That would expose a missing directory to concurrent preview readers. Existing legacy outputs require explicit clean before their first managed publication. Whether automatic live adoption is required remains an integration/product decision, not a guarantee supplied by this helper.

### Consumer inspection

Specular's `DocsServe.resolveRoot` keeps the normalized configured path. Ascent's preview server keeps that configured root and canonicalizes it during each request, rather than once at server startup. Its stamp poll also uses the configured path. Heddle's `Files.fromDirectory` retains the normalized configured root. This prevents permanently pinning the first generation, but does not prove HTTP request consistency.

Ascent independently canonicalizes the candidate and jail root. A pointer switch between those operations can reject an intact old-generation file with a transient 404. Heddle can stat a logical path and acquire its stream after the pointer moves, giving metadata and bytes from different generations. Retaining generations alone does not fix either boundary. S1 must resolve one physical root per request, jail its candidate against that same root, and use one fixed physical file for stat and stream acquisition. The actual HTTP gate must prove old-or-new complete responses, subsequent visibility of the new generation, embedded-link escape refusal and process retirement. No Ascent/Heddle change or passing HTTP proof is claimed in this unit. Multi-file consumers needing one generation must use a read lease.

## Runtime handoff

The runtime integration is intentionally separate from this unit.

1. Provide one `PluginProducts` instance for the execution/preview lifetime, alongside `ContentStore`. Its layer constructs the helper; methods perform filesystem work.
2. Call `Scheduler.cached[E](action, digest, request, local)` with `local: IO[E, CommittedOutputs]` for owned-output actions. It requires `ContentStore & PluginProducts` and returns `OwnedResult(record: String, outputs: CommittedOutputs)`. Both fresh production and cache replay retain the actual committed generation and read capability. Product failures remain `ScheduleError.Products`. Legacy text-only scheduling is a separate overload.
3. On a miss, run `stage`, plugin invocation, `seal`, and `commit` within one local `ZIO.scoped` effect. Pass `staged.destinations` into the frozen `RunRequest` constructor. Do not stage before cache lookup, and do not inspect final paths with the legacy `PluginAspects.declaredOutputs` aspect.
4. Return `CommittedOutputs` from local production. The scheduler serializes its manifest and records the action only after local success. A hit retains the `CommittedOutputs` returned by its single restore. Use `result.outputs` directly, not another parse/restore or parallel generation lookup. `result.record` is the stored representation; `PluginManifest.output(result.outputs.manifest)` reconstructs the public plugin report. Replay removes stale owned entries without rerunning the producer.
5. Keep `manifest.contentDigests` in the project's CAS keep set. Retain the committed result for generated-input consumers. `committed.locations` names the public roots; `committed.read` names fixed generation roots for scoped multi-file reads.
6. Configure a long-lived preview server with `committed.locations.directory`, the stable public path, not the fixed generation path returned by `committed.read`. Hold a lease in the child-process scope to exclude clean without pinning the server's configured root. Release preview processes and read scopes before calling `clean`. Clean owns exact declared paths and the managed home, not adjacent unowned siblings.
7. Add wire codecs for `ProductError` and its nested data before deriving the extended `ScheduleError[PluginFault]` codec. No codec or runtime wiring file is owned by this unit.

Remote action manifests can be replayed when their content blobs exist in the local `ContentStore`. A remote manifest without its local bytes is a miss, never a fabricated hit. Transporting product blobs between machines is not added here.

## Audit ledger

| Done | Category / finding | Owner / proof |
| --- | --- | --- |
| [x] | Existing Directory validation treats directories as regular files | H1b, executable legacy red and `PluginProductsSpec` |
| [x] | Existing cache hit validation cannot restore deleted bytes | H1b, executable legacy red and `PluginProductReplaySpec` |
| [x] | Exact File ownership and complete Directory manifest, including empty directories | H1b, `PluginProductsSpec` |
| [x] | Durable bytes, missing/corrupt content, stale removal and sibling preservation | H1b, product and scheduler boundary suites |
| [x] | Staged failure/interruption and prepared-promotion rollback | H1b, Promise-gated `PluginPublicationTransactionSpec` |
| [x] | Concurrent reads, fixed-generation leases and clean exclusion | H1b, publication and product boundary suites |
| [x] | Report validation, invalid/escaped entry names and link refusal | H1b, product boundary suite |
| [x] | Staging filename loss and case-insensitive namespace/final-path aliasing | H1b, additional executable reds and product boundary regressions |
| [x] | B1: mixed-case private/engine namespace reservation before stage and clean | H1b, destructive-clean red and sentinel/unrelated-generation/active-lease regression |
| [x] | B2: File cleanup extent cannot recursively own a physical directory | H1b, child-deletion red and exact unlink/kind-refusal regression |
| [x] | S1: generic product entries must not use archive filename restrictions | H1b, generic-name seal red and serialization/CAS replay regression |
| [x] | S2: owned cache result retains its committed-generation capability | H1b, type-capability red and single-restore fixed-generation regression |
| [ ] | Runtime request destinations, owned-result adoption, cache/keep wiring, generated-root handling and error codecs | Integrator, assembled engine and plugin suites |
| [ ] | Installed Specular site/preview and process retirement | S1/integrator, approved assembled snapshot gate |
| [ ] | Automatic live adoption of unmanaged nonempty directories, if required | Product/integrator decision |

New product code has no `Task` contracts, partial collection accessors, casts, throwing control flow, unsafe runtime calls, mutable public contracts or string error channels. The legacy version 1 `PluginManifest.accept` still treats read failures as misses and cannot replay bytes; owned-output integration must use the new scheduler overload instead.

## Proof log

- The actual pre-change `PluginManifest` was compiled with the current API and tested in isolation: Directory acceptance and deleted-file replay both failed, with no compiler failure substituted for red proof.
- The review regressions executed against the unchanged implementation: mixed-case root/descendant clean removed the engine sentinel and unrelated leased tree; wrong-kind File clean removed child bytes; safe generic resource names failed sealing. The S2 type-capability assertion also executed and failed because the cache returned String. These are actual red proofs, not an unrelated compilation failure.
- All owned production and test sources pass `-Wunused:all -Werror` with Scala 3.10.1, ZIO 2.1.27 and zio-json 1.1.1, without suppressions. Unowned dependencies are first compiled from their actual source into a support artifact, then the actual owned sources are strictly recompiled against it. This avoids editing or suppressing the existing unused imports in `schedule/Action.scala`.
- Full isolated ZIO Test execution passes 32 cases: `PluginProductsSpec`, 13; `PluginPublicationTransactionSpec`, 7; `PluginProductReplaySpec`, 4; `PluginProductSafetySpec`, 8. The harness uses the actual disk ContentStore and scheduler, not a replacement implementation. The cache capability test counts CAS/index reads to prove one hit restore and verifies fresh/hit leased bytes after later publication with unchanged producer count on the hit.
- Earlier repository attempts failed before tests on `ClassTooLargeException: histos/report/WireBuild$`. The parent now has the core codec fixes (`d37e58ef`, `53496b88`). They are not applied in this isolated checkout, and only the orchestrator owns that branch operation. Assembled repository proof remains pending, not falsely reported green or reproduced as red.
- The exact runnable source-harness command and runtime handoff are recorded in the unit summary outside the checkout. The integrator must repeat the repository command after assembling the codec and runtime changes, then run the existing `PluginTaskIdentitySpec` and actual Specular/site gate.
