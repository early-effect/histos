# Histos architecture

This is the tree as it is built today. [roadmap.md](roadmap.md) says where it is going, and [audit.md](audit.md) lists every known gap with the step that closes it. Gaps appear here as ledger ids, such as (S8). [plugins.md](plugins.md) is the plugin contract.

A PR that changes behavior described here updates this file in the same PR.

## What Histos is

Histos is a Scala 3 build tool written in ZIO. A repository describes its build in `histos.scala`. Evaluating that file produces one immutable, validated `ProjectGraph`. Compile, test, run, publish, and the CI render are ZIO programs over that graph. The same graph serves a terminal, Metals over BSP, and an agent over MCP.

Histos builds itself. `./histos test` compiles and tests the four modules of this repository, and `.github/workflows/ci.yml` is a render of `histos.scala`.

## Modules

| Module | Role | Depends on |
| --- | --- | --- |
| `api` | The graph, the builder, the literals, the version scheme, and the plugin contract (`histos.plugin`). The jar plugins compile against. | ZIO (because `HistosPlugin.run` returns a ZIO) |
| `central` | Publishers: a local Maven directory, and Sonatype snapshot upload through heddle's `Client`. | `api`, ZIO, heddle |
| `engine` | Load, selection, resolution, compile, test, Scala.js, Scala Native, toolchains, publish, assembly, versions, CI render, cache, scheduler, plugin runtime. | `central`, `api`, ZIO, zio-streams, scala3-compiler, Zinc, Coursier, ASM. The Scala.js and Scala Native linkers are fetched for a child JVM and are not dependencies. |
| `app` | The CLI, the server, BSP, and MCP. One `ZIOAppDefault`, `histos.cli.Main`. | `engine`, ZIO, zio-cli, heddle-mcp |

The pinned coordinates are in `histos.scala`. Histos itself is built with Scala 3.10.0. That compiler loads `histos.scala`. A module compile fetches the compiler the module names.

## The build file and the graph

This repository's own [`histos.scala`](../histos.scala) is a real build: four JVM modules, an assembly, and a version catalog. [`GraphSpec`](../api/test/GraphSpec.scala) builds graphs with Js and Native platforms, several Scala versions, and aggregates, and [`SelectionSpec`](../engine/test/SelectionSpec.scala) selects through an aggregate.

- `module(name, platform, platforms*)(version, versions*)` requires at least one platform and one full Scala version. A series such as `3` is not a pin.
- `aggregate(name, module, modules*)` takes `ModuleDef`s only, so a nested aggregate does not compile. An aggregate selects modules and has no classpath and no cell.
- `scala"..."`, `dep"..."`, `ship"..."`, `manual"..."`, and the other literals are macros. A malformed literal fails the build-file compile.
- Settings are methods that return a new `ModuleDef`: `libraryDependencies`, `testDependencies`, `dependsOn`, `in`, `main`, `test`, `testProvider`, `scalac`, `jsEnv`, `jsModule`, `nativeVersion`, `clang`, `artifact`, `note`, and `pipe`.
- `Projects(...)` returns `Either[BuildError, ProjectGraph]`. Validation reports the first error (G12).
- A cell is one module, one Scala version, and one platform. Compile and test work per cell.

Module flags are typed: `.scalac(ScalacFlag.EncodingUtf8)` adds `-encoding UTF-8`. `ScalacFlag` has one case today (G13).

### Source layout

A module's directory is its name, or the path given to `.in(...)`. Sources are `<directory>/src`. A single-module graph may use `src` at the repository root. Tests are `src/test` under the source root plus a sibling `<directory>/test`. Platform and version directories (`jvm/`, `js/`, `native/`, `scala-2.13/`, `scala-3/`) select files for a cell. Main and test are separate compilation units, and test outputs never become main classes.

## Load

`Loader.load(repo, apiJar)` is a `ZIO[Scope & Resolve & ContentStore, LoadError, LoadedBuild]`. The `Scope` closes the build class loader and its plugin threads.

1. `Loader.scan` reads the plugin header (`plugin(dep"...")`, `plugin(path"...")`) at the top of `histos.scala`.
2. The loader resolves header plugins through the lock, compiles source and path plugins first, and writes the build file to a temporary copy with the header lines blanked, so line numbers match.
3. The Scala 3 compiler compiles that copy in process against the api classpath and the plugin classpaths.
4. The build object loads in a `BuildLoader` whose parent is `ApiBoundary`. `ApiBoundary` is parented on the platform class loader and loads the boundary names (`histos` with no subpackage, `histos.plugin`, `histos.version`, `zio.*`, `izumi.reflect.*`, `scala.*`) from the engine host, so `ProjectGraph` is one class. Other `histos.*` names are refused. Reading a refusal clears it, so the failure belongs to the load or run that made it.
5. `graph` and every plugin `run` execute on [`PluginThreads`](../engine/src/histos/load/PluginThreads.scala): a fixed pool and a blocking pool whose daemon threads have `BuildLoader` as their context class loader. A fiber that moves to another thread, including a blocking step, still sees it.

Loading is the same in the batch command and the server. A load error is a `LoadError` and exits 3.

## Selection

`compile`, `test`, `testOnly`, `testQuick`, and `publish` share `Selection.select`. No names is every module. A module name is that module, and an aggregate name is its direct members. A repeated module is refused. Selection errors reuse `CiError` today (A24).

## Resolution and the lock

`.histos/dependencies.lock` is the authority for module dependencies. `histos resolve --update` resolves with Coursier and writes exact versions, repositories, and SHA-256 checksums. Plain `histos resolve` verifies, and `--check` fails on drift. Compile and test read the lock and do not re-resolve. A range is refused before fetch. A checksum that disagrees with the bytes is an error. Test dependencies are a separate cell and never reach the compile classpath or the published pom. Plugin cells live in the same file.

The lock does not yet cover the toolchain: Scala compiler jars, semanticdb-scalac, the Scala.js linker and environments, Chekhov, Scala Native tools, and the bridge jars are fetched from Maven Central with no lock entry or checksum (L1). Several tool versions are literals in engine code (L2).

## Compile

The engine compiles JVM, Scala.js, and Scala Native cells with Zinc 2.0.4 and the compiler the module names. Scala 3 and Scala 2.13 are accepted. Scala 2.12 and older are `CompileError.UnsupportedScala`.

- Outputs go to `.histos/classes/<module>/<scala>/<platform>`. Test outputs go to `.histos/test-classes`.
- Upstream cells compile first. A cell starts when its own upstreams finish, and independent cells run together.
- A failed cell does not cancel an independent sibling. The command fails after the last cell.
- SemanticDB is on for every cell: `-Xsemanticdb` for Scala 3, and the `semanticdb-scalac` plugin for 2.13.
- The compile classpath is the locked compile cell plus upstream classes. Jars that contain the Scala compiler or a Histos build marker are refused, so the engine never reaches user code.
- Each compile starts a fresh Zinc compiler with an empty previous result, so every compile is full and a deleted source leaves its class files behind (S8).

## Test

A test cell runs in its own subprocess: one per module, Scala version, and platform. Tests never load inside the engine or the server.

- The default framework is ZIO Test (`TestProvider.Zio`) when a module names neither a provider nor a main. `.testProvider(...)` names another sbt test-interface framework, and `.test(TestMain(...))` runs one main instead.
- The child classpath is the module's main and test classes, upstream main classes, and the locked compile and test jars. The engine is never on it.
- The child environment is `HISTOS_REPO`, plus `PATH`, `HOME`, and `XDG_CACHE_HOME` from the parent. A JVM child calls `setpgid` before user code. Stopping or cancelling a child kills every process below it, and for a JVM child its whole process group, which also reaches what a test left running after its main returned. Node and native children have no group of their own, so a descendant that outlives the node or native process is not reached.
- Output streams to the console as it arrives, one suite tree per cell in module order. One fiber reads each pipe, and the cell's status does not keep a copy of the output.
- `testOnly <pattern>` filters suites by fully qualified name (`*` crosses dots, `?` is one character). A pattern that matches nothing fails.
- `testQuick` skips a suite whose stamp and last success match. The stamp covers the suite's referenced classes and misses upstream jars, the JDK, and flags (P6).
- `~ <command>` reruns a command when a watched file changes.
- A failure in one cell's preparation stops later cells (P4).

## Scala.js and Scala Native

Scala.js and Scala Native cells compile through Zinc with the platform's compiler support (`-scalajs` for Scala 3, the Scala Native compiler plugin). Linking runs in a child JVM, so the engine never loads a linker.

- Scala.js test and run go through bridge programs that the engine writes as Scala source, compiles, and links per run (P1, P2). The JS environment is `JsEnv.Node` (default), `Jsdom`, or `Firefox` through Chekhov. Js test and run launch the Node that `histos toolchain` reports. It is chosen at a command's first Js launch and kept for the rest of the command, and Chekhov's Playwright driver gets it as `PLAYWRIGHT_NODEJS_PATH`. When no Node can be chosen, the Js cell fails with `NoNode` before it links.
- A Native module names its tools with `.nativeVersion(...)`, default 0.5.12 from Maven Central. `HISTOS_NATIVE` overrides the fetch origin for a local build. A command reads it once from the machine and hands it to compile, the test compile, link, and runner, and the run link. A malformed value fails only Native cells. Native links need clang on `PATH`, and `.clang(...)` names the expected major.
- Library suffixes follow the platform: `_sjs1_3`, `_native0.5_3`.

## Run

`histos run <module> [jvm|js|native]` starts the module's `.main(...)` class on its platform. A module that builds more than one platform needs the platform argument. `histos run inspect` and `histos run versions` are built-in tasks, and `histos run <plugin>/<task>` runs a plugin task.

## Toolchains

`.histos/jdk.lock` names the JDK major and per-platform Temurin downloads. The engine picks a JDK in this order: `.histos/toolchain.local`, `JAVA_HOME`, `PATH`, Coursier, sdkman. When a manager is installed but holds no match, Histos prints an install hint. Otherwise it uses the Histos cache, then provisions the locked Temurin. When the running JVM is not the chosen one, the engine starts a child on the chosen JDK that shares the terminal, and exits with the child's status. Interrupting the engine sends SIGTERM to that child and every process below it, then force-kills whatever is still running 2 seconds later. Coursier counts as installed when `cs` is on `PATH` or its JVM cache holds a JDK. Engine staging unpacks a zip (the Windows archive) itself, and a tar.gz or tar.xz with the system `tar`, which keeps file modes and symlinks. The archive is copied to a scratch file and unpacked into a scratch directory beside its destination, then moved into place atomically. A failure or an interrupt removes the scratch, and a complete unpack that another stager placed first is kept.

`.histos/node.lock` pins Node, and `.histos/node.local` overrides it. `histos toolchain` reports which Node it would use, and Js test and run launch that one. When no installed Node matches and the lock names a `file:` zip, tar.gz, or tar.xz, that archive is staged into the cache the same way. The probe only reads: a `node` without its execute bit is not a match, and its `node --version` child is killed with everything it started if the probe is interrupted.

`histos toolchain` prints the JDK decision, then Node when the graph has Js, then clang when it has Native.

## Publish and versions

The version scheme is the zipx scheme, implemented in `histos.version`: everyday builds are `<version>-ci`, a snapshot is `<version>-<sha>-SNAPSHOT`, the default branch also updates a `<version>-SNAPSHOT` pointer, and a ship number is the next release. A catalog can state a manual version, which is what this repository does. `histos run versions` checks the catalog and the registry.

`histos publish <directory> [project ...]` writes jars and poms into a local Maven directory. Each platform cell is its own coordinate (`core_3`, `core_sjs1_3`, `core_native0.5_3`), with a pom that names that platform's dependencies. `histos publish snapshots` uploads to Sonatype's snapshot repository. The repository is `https`, or `http` on loopback. `SONATYPE_USERNAME` and `SONATYPE_PASSWORD` are read from the environment the command was given, and the password is a `Config.Secret`. The upload uses heddle's `Client` and the fiber's clock.

## Assembly and the launcher

`histos assembly [path]` writes one runnable jar of `app` and its dependencies, rewrites lazy-val bytecode that would call `sun.misc.Unsafe`, and records its SHA-256 in `.histos/assembly.sha256`. The assembly spec is hardcoded for this repository (P8).

`./histos` and `histos.bat` are frozen scripts. With `dist/histos.jar` present, the script verifies it against `.histos/assembly.sha256` and execs it. Otherwise it downloads the launcher for `.histos-version`. A release launcher is checked against its pin. The snapshot path does not check one (L7). The script fetches the locked Temurin only when no `java` is on `PATH`.

## CI render

`.github/workflows/ci.yml` is a render of the graph. `histos workflow` and `--check` compare it, `--update` rewrites it, and drift exits 6. A pull request starts that workflow once. A push starts it only on `main`, so updating a pull request branch does not run it again.

- Jobs are `test` (`./histos test`), `versions` (`./histos run versions`, when the catalog is non-empty), each plugin job with `JobWhen.Always`, and `workflow` (`./histos workflow --check`). A plugin job with a schedule renders as its own `histos-<job>.yml`.
- When `.github/ci-tools` exists, the test job also installs Node 22, LLVM 21.1.8, and the Playwright Firefox build Chekhov reads. Those installs are not cached, so every `test` job repeats `npm install`, `install-deps firefox`, and the browser download (C1). M8 restores an install the graph still needs from a keyed cache.
- The renderer can scope a pull request to the changed modules and their dependents. The committed `ci.yml` is the full render, so pull requests run every module.

## Cache

The content store lives in `$XDG_CACHE_HOME/histos` or `~/.cache/histos`. Entries are content-addressed under `cas/`. `histos cache path`, `cache list`, and `cache clean [--dry-run]` manage it. `clean` removes entries under `cas/` and `plugins/` that no current project has used for 30 days, and temp files in `tmp/` older than that. It does not touch JDKs or launchers. Each project has its own keep file, replaced by the plugins that load used.

The remote cache is off. The only implementation is in memory, used by tests, and there is no configuration path (D10).

## Tasks and the scheduler

`Tasks` is the run registry: it starts a task, returns a `RunId`, and cancels by interrupting the fiber. Built-in tasks are `inspect` and `versions`.

`Histos` in `histos.schedule` runs a step graph: one promise per step id, a permit per resource class, and a failure recorded on the outcome instead of cancelling siblings. A plugin task is a step. A `TaskDep.Compile` is a compile step, and only the tasks reachable from the one being run are built. `pluginLoader` and `declaredOutputs` are step aspects. Test, publish, and run are still separate pipelines (K1).

## Plugins

A build opts into plugins in `histos.scala`: the header names each jar or path, and `use(...)` names each plugin object as the first argument to `Projects`. Nothing is discovered by scanning a classpath. A plugin contributes data (tasks, commands, CI jobs, module notes), and the graph stores that data. `HistosPlugin.run` returns the effect the engine runs later inside the plugin's `Scope`. [plugins.md](plugins.md) is the contract.

## Process model

The CLI is one-shot by default: each command loads the build, runs, and exits.

`./histos server` is the opt-in warm process. It writes `.histos/server.json` with its version, a fingerprint of `histos.scala`, `.histos/dependencies.lock`, and path-plugin files, and a socket path. A command that loads the graph reads that record:

| Record | Decision |
| --- | --- |
| Missing, corrupt, or unreadable | Run in process (`Decision.Absent`) |
| Different `Cli.VersionText` | Refuse and ask for `histos reboot` (`Decision.DifferentVersion`) |
| Matching version | Send a `Call` over the socket (`Decision.Open`). The server compares the fingerprint on each call and reloads a changed build. A failed reload keeps the last good graph and fails the call. |

The server covers compile, test, publish, graph, run, a task, cancel, toolchain, resolve, workflow, assembly, reload, reboot, and a BSP splice. `cache` and `version` never load a graph. `histos reload` reloads the build in the same JVM and keeps the last good graph on failure. `histos reboot` replaces the JVM.

A command that attaches never loads the build in the client. `histos server` exits when its server stops: after a reboot has started the replacement, or when binding or writing the record fails, which also removes the socket. Stopping closes every open connection, including an idle one.

## BSP

`.bsp/histos.json` names `./histos bsp`. Metals spawns it and speaks BSP 2.2 over stdio. When a server is running, the BSP process splices its stdio onto the server socket.

| Method | Behavior |
| --- | --- |
| `build/initialize`, `build/initialized`, `build/shutdown`, `build/exit` | Lifecycle. Requests before `initialize` are refused. |
| `workspace/buildTargets` | One JVM main target per module and Scala version, a test target when the module has tests, and a target for `histos.scala`. Js and Native targets appear only when `.histos/bsp-platforms` names them. |
| `buildTarget/sources`, `buildTarget/dependencySources` | Source roots and locked source jars. |
| `buildTarget/scalacOptions` | The module flags, then SemanticDB options, and the classpath. |
| `buildTarget/compile` | Compiles the cell without linking. Diagnostics go out as `build/publishDiagnostics`. The `histos.scala` target goes through `Loader.reportBuild`, which prepares the file as a load does (header blanked, plugins resolved and compiled) and compiles it against the api and plugin classpaths. |
| `buildTarget/scalaMainClasses`, `buildTarget/scalaTestClasses` | Discovered from class files. |
| `buildTarget/jvmRunEnvironment`, `buildTarget/jvmTestEnvironment` | The classpath and environment Metals launches with. |
| `buildTarget/didChange` | Sent after a reload changes the target list. |
| `build/logMessage` | Progress and errors. |

Each request checks the build fingerprint and reloads a changed build, and every spliced session receives `buildTarget/didChange`. The session handles one request at a time and ignores `$/cancelRequest` (A10).

## MCP

`histos mcp` serves the Model Context Protocol over stdio through heddle-mcp. The tools are `graph`, `compile`, `test`, `publish`, `run`, `cancel`, and `toolchain`. Each call re-reads `.histos/server.json` and sends a `Call` when the server matches. Otherwise the session keeps one loaded graph.

Tool results are the CLI text as a string, success and failure alike. The roadmap replaces this with typed reports and typed faults (A2).

## Errors and exit codes

Every module reports failures as error enums with a `message` rendering, for example `BuildError`, `LoadError`, `ResolveError`, `GraphCompileError`, `ModuleTestError`, `CiError`, `PluginRunError`, and `ServerError`. Many IO cases drop the path and the reason (S2), and the server turns errors into strings at the wire (A1).

| Exit | Meaning |
| --- | --- |
| 0 | Success |
| 1 | A task failed |
| 2 | Usage |
| 3 | Load failed |
| 4 | Toolchain failed |
| 5 | Resolve failed, including a missing lock for test and run |
| 6 | Workflow drift or a missing workflow file |
| 7 | A test failed, or an unknown project for test |
| 8 | Compile failed, including a missing lock for compile |
| 9 | Publish failed |
| 130 | Interrupted |
| other | The exit status of the program `histos run` started |

A missing lock exits 8 from `compile` and 5 from `test` and `run` (A17).

## Security

The trust boundary is the local user. `histos.scala`, header plugins, and test code run with the user's permissions.

- The launcher verifies `dist/histos.jar` and release launchers by SHA-256. The snapshot path does not (L7).
- The lock checksums module and plugin dependencies. Toolchain jars are not checksummed (L1).
- `ApiBoundary` keeps engine classes away from build and plugin code. It is a class-loading boundary, and plugins still run with the user's permissions.
- The server socket lives under `.histos` and serves the local user.
- A test child gets a reduced environment and its own process. It is not a filesystem or network sandbox.
- `histos publish snapshots` sends Basic auth to Sonatype's snapshot repository. An `http` repository is accepted only on loopback.
