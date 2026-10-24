# Histos plugin contract

**Checkpoint:** 2026-10-24. The loader, isolation boundary and Step-based engine orchestration are implemented. M4 lock/provenance integration passed the final local suite; Windows bootstrap remains a separate platform gate. The public typed plugin-step API and remaining fixtures are M7 work, not current guarantees.

Plugins compile against [`histos.api`](../api/src/histos/plugin). Loader/runtime code lives in [`histos.load`](../engine/src/histos/load); commands live in [`histos.cli`](../app/src/histos/cli). [architecture.md](architecture.md) describes resources and surfaces; [audit.md](audit.md) owns known limits. Examples are linked tests, not duplicated declarations here.

## Enablement and API boundary

A plugin is a Scala 3 library with one public entry object implementing [`HistosPlugin`](../api/src/histos/plugin/HistosPlugin.scala). It contributes pure descriptions and returns `ZIO[Scope, PluginRunError, PluginOutput]` from run. It is not an AutoPlugin, a test framework or a Scala compiler plugin.

Two explicit choices enable it:

- The `plugin(...)` header puts a published jar or repository path on the build classpath.
- `use(...)`, first in Projects/Organized arguments, registers its tasks, commands and jobs. A jar present in the header contributes nothing unless registered, but it is still verified/loaded and is trusted code.

The loader never scans the dependency classpath for an entry or enables a plugin transitively. Module settings remain ordinary immutable `ModuleDef => ModuleDef` functions applied with pipe. Notes are the data those functions leave behind, not another settings language.

The plugin compiles against the API and its allowed libraries, never engine/scheduler/CLI/central classes. The launcher supplies the boundary Scala/ZIO/izumi classes at runtime. A project compilation never receives plugin URLs and refuses Histos/plugin classpath markers.

Before 1.0, there is no binary compatibility promise or shim. A binary plugin must match the actual API jar's ApiDigest or use a locked source fallback. The product snapshot label is not that digest. LinkStrategy.Api is a typed refusal, not bytecode rewriting or MiMa.

`central` is an in-tree publisher module, not a HistosPlugin. A future Central plugin needs a package outside the reserved Histos namespace. Tests/providers and Native nscplugin use different seams. Cross-plugin task dependencies, arbitrary typed plugin Steps, a filesystem/network sandbox and remote execution are not current capabilities.

## Descriptions, names, and validation

[`Contribution`](../api/src/histos/plugin/Contribution.scala) holds tasks, commands and CI jobs. The graph stores RegisteredPlugin descriptions; instances and classloaders remain with the loaded runtime. `contribute` runs during graph evaluation and must be total/no-IO. Returning data is the contract, not compiler enforcement of purity. A thrown exception becomes BuildError.PluginDefect with its class name.

| Contract | Meaning |
| --- | --- |
| DeclaredTask | Task name, Seal, Needs, declared inputs/outputs and dependencies |
| TaskDep.Compile | Main/test classpaths of selected module cells |
| TaskDep.Same | Another task of the same plugin, with empty arguments |
| CommandSpec | Root command name, positional/flag schema, task and help detail |
| CiJob | Job name, task, trigger and typed permissions |
| ModuleNote | Plugin/key-owned one-line data on a module |

[`PluginId`](../api/src/histos/plugin/PluginId.scala), [names](../api/src/histos/plugin/Names.scala), [`RepoPath`](../api/src/histos/plugin/RepoPath.scala), [texts](../api/src/histos/plugin/Texts.scala), [`EntryName`](../api/src/histos/plugin/EntryName.scala), [`ApiDigest`](../api/src/histos/plugin/ApiDigest.scala) and [`Cron`](../api/src/histos/plugin/Cron.scala) have validated literals and typed runtime parsers. PluginId uses dot-separated letter-led segments; task/command/job names use letter-led letters/digits/hyphens. RepoPath is relative, rejects traversal/absolute paths, and permits filenames that ProjectPath does not. Cron validates GitHub's five-field grammar. [`PluginTaskId`](../api/src/histos/plugin/PluginTaskId.scala) pairs plugin and task and renders exactly one slash.

Projects validates duplicate plugins/tasks/commands/jobs, missing tasks, reserved roots/jobs, duplicate/missing notes and missing module/cell references. TaskDep.Compile and DeclaredInput.Module can select all versions/platforms of a module or one valid cell; aggregates are not cells. See [PluginSpec](../api/test/PluginSpec.scala) and [Graph](../api/src/histos/Graph.scala).

Only a Hermetic task with no declared Need is sendable under [`Sendable`](../engine/src/histos/schedule/Sendable.scala). That is a declaration-based cache policy, not proof of isolation. Core Step kinds exist, but the public plugin SPI still exposes DeclaredTask plus run. ClassHome does not currently include `histos.step` among boundary packages; M7 must deliberately connect the public typed-step contract and loader boundary (G11/G16).

## Header and compilation unit

[`HeaderScan`](../engine/src/histos/load/HeaderScan.scala) reads a constrained prefix before compiling the build. It skips empty lines and whole-line comments. Published requests accept an exact dependency literal and optional ExactBinary, Source or BinaryOrSource strategy; Api is refused. Path requests accept only Source. No strategy on a published request means BinaryOrSource.

Ranges/latest revisions, malformed coordinates/paths, trailing syntax, unknown strategies and misplaced header lines fail before resolution. An import ends the prefix. Header values cannot be assembled from ordinary Scala expressions; after the prefix, a recognizable plugin header is Misplaced. The complete accepted/rejected grammar lives in [LoadSpec](../engine/test/LoadSpec.scala).

ScannedBuild contains requests and source text with header lines blanked, preserving diagnostic lines. Loader compiles the temporary copy, rewrites diagnostic paths back to histos.scala, and removes temporary files with the load Scope. Fingerprint hashes the original build file, header included.

Loader's live implementation takes Resolve and ContentStore. `load(repo, apiJar)` requires Scope; `loadLocked` requires the supplied DependencyLock generation to be used. It is a mandatory operation, not a default that silently calls ordinary load. The same preparation supplies BSP build-file diagnostics.

## Plugin lock and explicit update

A published plugin cell records the direct dependency and every resolved transitive jar, each with repository/checksum and optional classifier. Resolution uses the validated running [`BuildCompiler`](../engine/src/histos/toolchain/BuildCompiler.scala) Scala revision on JVM, not a hardcoded build compiler pin. Direct plugin lookup uses the shared ArtifactName rule.

[`DependencyLock`](../engine/src/histos/resolve/DependencyLock.scala) renders plugin cells beside module/test/tool cells. A source row pins the direct sources artifact for Source/BinaryOrSource; ExactBinary omits it. Locks containing only plugin or tool cells are valid. Empty locks are not. Path plugins have no published cell and compile against boundary jars only; third-party path-plugin libraries belong on a published coordinate.

The lock's strategy wins. Editing a header link strategy does not retarget execution until an explicit update.

### CLI transaction

[`Cli`](../app/src/histos/cli/Cli.scala) and [`Calls`](../app/src/histos/server/Calls.scala) implement staged update:

1. Scan and plan header plugin cells without writing the committed lock.
2. Load the build with that staged plugin generation through loadLocked.
3. Solve its module/tool requests, preserving the captured Native origin and graph resolver policy.
4. Commit plugin/module/tool sections together only after success.

A compile or later solve failure leaves the prior committed lock intact. Populating artifact/plugin caches during an unsuccessful attempt is not an intermediate lock commit. Header planning currently uses Central before graph resolvers exist. Lower-level Resolve.updatePlugins still supports independent fixture/library writes; it is not this CLI transaction. See [ToolCellsSpec](../engine/test/ToolCellsSpec.scala).

- Ordinary resolve/load/compile/test fetch only recorded plugin artifacts, verify checksums and fail Unlocked on a missing cell. Sources are never fetched without a pinned source row; missing required sources are NoSource.
- Resolve check requires existing cells and re-solves plugin/module drift without writing. Planned tool cells are materialized, not re-solved.
- Cold exact materialization may need network. No silent tool solve or missing-cell repair is authorized by loading.

One command's plugin compile dependencies and classpath preparation must share one captured lock/runtime view. Current PluginRuntime memoizes one captured lock/runtime and supplies it to compile/classpath preparation. Real plugin/application lock-replacement barriers and request-runtime freshness regressions passed under L11/L13.

## Entry resources and linking

The plugin author writes two one-line resources in the direct binary jar:

- `META-INF/histos/plugin`: JVM binary EntryName of its public entry object, including `$`.
- `META-INF/histos/version`: API jar SHA-256 used to compile that binary.

The loader reads only the direct artifact's resources, never markers in transitive libraries. Missing/invalid entry, multiple entry lines or missing/invalid digest produce typed NoEntry/AmbiguousEntry/Unversioned errors. A path plugin keeps the entry resource at its source-directory root; copied resources accompany its compiled classes.

| Lock strategy | API digests match | API digests differ |
| --- | --- | --- |
| ExactBinary | FromBinary | VersionMismatch |
| BinaryOrSource | FromBinary | FromSource, or NoSource |
| Source | FromSource, or NoSource | FromSource, or NoSource |

Binary links use the verified direct/transitive jars after excluding boundary-supplied copies. Source links compile the locked sources against the running API/boundary plus third-party jars; they exclude the published direct binary and duplicate boundary jars. Compiled classes replace the direct binary on both compiler and runtime classpaths. Path plugins are source-linked by construction. [LoadSpec](../engine/test/LoadSpec.scala) covers strategy, path and refusal behavior.

The entry is initialized inside the load failure boundary. The registered object must come from its header artifact/compiled directory, match the entry resource and plugin id, and be declared. Initialization defects name the exception class and relevant coordinate, not an unrelated plugin.

### Source compilation cache provenance

[`PluginCompile`](../engine/src/histos/load/PluginCompile.scala) stores completed class directories under `<cache>/plugins/<digest>`. Its key contains API digest, source content, dependency checksums and CompilerIdentity. CompilerIdentity combines validated running revision, actual embedded/support byte provenance and the JVM runtime executing in-process Dotty. Module/bridge tool-cell hashes cannot substitute for those actual bytes.

Current wiring hashes Dotty/TASTy/compiler-interface sources plus the supplied boundary/classpath, with the actual executing JVM identity. Changed-support-byte and non-assembly scenarios passed final integration review tests under L14. The value formerly called launcher in this path is the API digest; it is not evidence for the complete launcher. Runtime/content or support-byte changes must invalidate the source cache even when version/API bytes match.

A directory is reusable after its complete marker. Load replaces its project's keep set; another project's keep set still protects its plugin cache. Cache clean ages out unused completed directories after retention, without deleting shared runtimes/launchers. See [ContentStore](../engine/src/histos/cache/ContentStore.scala).

## Classloader and execution trust

[`Boundary`](../engine/src/histos/load/Boundary.scala) supplies API root/plugin/version, Scala, ZIO/stacktracer and izumi-reflect classes from the host. Build/source compilation uses those boundary jars plus allowed plugin libraries. Duplicate boundary jars in a plugin cell are excluded from the flat compiler classpath.

[`BuildLoader`](../engine/src/histos/load/BuildLoader.scala) is parent-first outside reserved Histos names, but refuses non-boundary Histos packages before searching plugin URLs. Entry and other classes in reserved Histos/ZIO/Scala packages are rejected. A refusal records its requesting class and clears after observation, preventing a later unrelated run from inheriting it.

[`PluginThreads`](../engine/src/histos/load/PluginThreads.scala) owns ordinary/blocking executors whose context loader is BuildLoader. The load scope closes threads and loader. Plugins on that loader can see each other's ordinary libraries, which the build imports. Project compiles never see those URLs and reject API/plugin markers through [JvmCompile](../engine/src/histos/zinc/JvmCompile.scala).

This is a linking boundary, not a hostile-code sandbox. Under java -jar the system loader still sees the assembly. Enabled/header code can use ordinary filesystem/network/process APIs with the user's permissions. A plugin that lies about declared inputs can poison a cache. Exact checksums and API matching do not remove that trust decision.

## Running and caching tasks

[`LoadedBuild`](../engine/src/histos/load/LoadedBuild.scala) pairs graph and runtime. [`PluginRuntime`](../engine/src/histos/load/PluginRuntime.scala) builds only tasks/cells reachable from the requested id using [`PluginSteps`](../engine/src/histos/load/PluginSteps.scala), then runs them through Histos. TaskDep.Compile performs real main/test preparation before dependent work. Dependencies precede dependents; only the requested task receives supplied arguments.

The engine's error channel is [`PluginFault`](../engine/src/histos/load/PluginFault.scala), nesting Task, Compiled, Resolved, Tested and Scheduled errors. The plugin runner itself still uses PluginRunError. Neither a compile failure nor a lock failure is flattened to the plugin's arbitrary detail text at the engine boundary.

### Action inputs

The engine hashes declared inputs plus inputs a plugin cannot omit:

- Verified plugin runtime jars or compiled directories, named by content rather than cache paths, with exact duplicates removed.
- Running API digest and plugin task identity.
- Class directories selected by Compile/Module dependencies and upstream task values.
- Seal and Needs through Policy.Plugin.

File hashes its bytes; Directory hashes sorted relative filenames/bytes; Module hashes selected class directories; Note/Text hash their values; Env hashes the captured value. Module inputs need Compile dependencies when fresh classes are required. Module notes visible to run belong only to that plugin. Graph output never prints environment values.

[`PluginInputs`](../engine/src/histos/load/PluginInputs.scala) and [`Digest`](../engine/src/histos/schedule/Action.scala) own this key. Reads a plugin failed to declare cannot change it. Required declared inputs are checked before run. Scope or a declaration is not a read sandbox.

### Cache policy and output manifest

Histos orders Steps; Scheduler.cached and ScheduleAspects handle cache policy for an action. Current lookup is remote when enabled, then local. Production CLI/server use local cache with remote off. A declared-output hit requires every regular file in the repository to match its recorded hash; absent/changed files are a miss. A text-only hit requires no output file.

On a miss, run executes in a Scope on PluginThreads. Every WrittenFile must be declared and every declared output must exist as a regular file. Failure stores nothing. The manifest stores text and declared path/hash metadata, not output bytes. Both hit paths validate it. Another machine with only a manifest and no matching output files reruns locally.

Policy.Plugin stores only sendable successes; Open/needed tasks return their results without storing. Core cache policy differs and does not gain a plugin policy line. See [Scheduler](../engine/src/histos/schedule/Scheduler.scala), [ScheduleAspects](../engine/src/histos/schedule/ScheduleAspects.scala), [PluginManifest](../engine/src/histos/schedule/PluginManifest.scala), [SchedulerSpec](../engine/test/SchedulerSpec.scala), and [PluginRunSpec](../engine/test/PluginRunSpec.scala).

### Runner request and cancellation

[`RunRequest`](../api/src/histos/plugin/Request.scala) supplies task id, repo root, module views, named arguments/flags, materialized inputs and compiled classpaths. Env materialization contains the raw declared value for the runner, while manifests/graph/workflows do not. Compile/test classpaths keep the engine out and preserve dependency scope.

Interruption closes the inner Scope and stays interruption, CLI exit 130, rather than PluginRunError. Plugin resources must be scoped too. No shutdown hook repairs a plugin's fire-and-forget process. Partial work-tree outputs may remain; there is no transactional output directory, and partial files do not satisfy manifests. Defects/linkage/missing outputs/failures store nothing.

Load failures nest in LoadError and exit 3. A deliberate task failure or mapped run defect exits 1. Exception messages are not failure control flow. PluginIo retains its operation/path; Defect/Linkage carry class names. See [plugin errors](../api/src/histos/plugin/Errors.scala).

## CLI, server, MCP, and jobs

The grammar is closed before loading. Only an unknown root outside [`CommandName.reserved`](../api/src/histos/plugin/Names.scala) falls through to plugin commands. Built-ins, including launcher, win; acp remains reserved although deleted. Cache subcommand words remain available at the root. A malformed command under a reserved root does not load plugins.

[`PluginCommands`](../app/src/histos/cli/PluginCommands.scala) interprets required texts, at most one optional text, and typed flag names. Unknown flags, too many/few positionals and invalid specs fail with usage. Watch uses the same dispatch. A root typo can require loading before usage and thus fail with LoadError if the build is broken.

`run <plugin>/<task>` uses empty arguments and refuses a platform token. Contributed commands supply their parsed arguments in process. Call.Task currently carries only the id, so server/MCP runs receive empty arguments (A25). Cancellation through those surfaces still has task-id/serial-request limitations (A8/A23). No plugin-specific MCP tool or BSP task target exists.

Graph text shows plugin binary/source provenance, task seal/needs/declared paths, commands and jobs. Server fingerprints the build, dependency lock and path-plugin files and reloads changed generations. Resource leasing, typed surface reports and current request runtime/lock proof are separate A20/L11/L13 concerns, not plugin-cache guarantees.

[`Workflow`](../engine/src/histos/workflow/Workflow.scala) renders always jobs beside test/versions/workflow and scheduled jobs as separate `histos-<job>.yml` files. Job permissions are typed ContentsRead/ContentsWrite. The step runs the task id, not its contributed CLI command. Check detects changed/missing/orphan scheduled files; update writes rendered files and deletes only those orphans. See [WorkflowSpec](../engine/test/WorkflowSpec.scala) and the [plugin job golden](../engine/test/fixtures/ci/plugin-always.yml).

The dedicated Windows launcher fixture is hand-written M4 verification, not a contributed plugin job. Graph-derived tool installation/caching and eventual ownership of that workflow remain M8.

## Author tests and surviving limits

A plugin author can call run with ZIO Test and [`PluginFixture`](../api/src/histos/plugin/PluginFixture.scala) against API only. The fixture constructs a request; it does not load jars, hash input or run the executor. Compiled fixture values remain unavailable (G17). [PluginSpec](../api/test/PluginSpec.scala) exercises registration/parser contracts. [LoadSpec](../engine/test/LoadSpec.scala) tests linking and isolation. [PluginRunSpec](../engine/test/PluginRunSpec.scala) tests real engine orchestration, inputs, argument routing, output checks, path-independent jar keys and interruption.

[SpecularFixtureSpec](../engine/test/SpecularFixtureSpec.scala) is a complete fixture with a plugin plus transitive helper library, a build using its setting/note/command/job and local publication. It is not the real Specular plugin. Its low-level update setup is fixture code, not the public staged CLI transaction. T14 still owns runner-through-PluginFixture, a complete lock cell example and a combined dependency/path/strategy header test.

| Limit | Owner |
| --- | --- |
| Declaration-based sendability and undeclared reads, no sandbox | Future isolation policy |
| Engine system loader reachable to enabled code | Explicit local-user trust boundary |
| Operation-captured generation/runtime/provenance | Verified under M4 L11/L13/L14 |
| Compile/Same-only dependencies, separate declared kind, no public typed phase Steps | M7 G16 |
| String notes/settings and registration/version error flattening | M7 G6/G10/G14/G15 |
| Empty compiled fixture values and missing complete examples | M7 G17/T14 |
| Lost remote command arguments, serial cancel/progress and string reports | M6 A1/A2/A8/A23/A25 |
| No live remote cache configuration, output bytes never transferred | D10 and future remote cache |
| No binary compatibility promise before 1.0; Api linking refused | Deliberate prerelease contract |

Explicit enablement replaces automatic triggers; an ordinary library API replaces a second settings language; full lock cells replace single-jar checksums; source fallback replaces premature binary shims. Those decisions remain. M1/M2 isolation/orchestration work is delivered, while M4 acceptance and M7's public phase API remain open.

## History

The original plugin series added types (`c6bafde`), registration/settings (`4a6bfd7`), header/lock/loader (`d440ce0`), commands/cache policy (`449ef75`), jobs (`8809552`), source/path fallback (`844dcc4`), the Specular-shaped fixture (`f50fa66`) and compiled inputs (`7eeacb3`). `6dd46d3` made MCP attach to the server. M1 fixed loader/thread/refusal/resource behavior; M2 moved reachable task orchestration to Histos and preserved nested faults; M3 separated construction/resources. M4 now extends artifact identities, staged generations and source compiler/runtime provenance. This repository's build enables no plugin.
