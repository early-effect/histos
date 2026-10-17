# Histos Design Document

**Status:** Draft
**Date:** 2026-10-16
**Org:** early-effect
**Repo:** `early-effect/histos`

## 1. Vision

Histos is an effect-oriented, agent-native build tool for the Scala ecosystem. Evaluating a build produces an immutable project graph. Tasks run as ZIO effects. Compile, test, publish, and inspect share one task API for the CLI, BSP, MCP, and ACP.

A clone of a Histos project builds with `./histos`. The repo carries frozen bootstrap scripts and a version pin. The scripts download the pinned JVM assembly, verify it, and exec it. They do not need a JDK already installed: when no `java` is present they fetch the locked JDK, and the engine checks that lock again.

Histos replaces sbt-zipx for a repo that has moved. The build describes CI. Artifact names stay the ones downstream catalogs already pin.

The tool and every plugin are Scala 3.9+. Projects being built may cross-build Scala 2.13 and Scala 3, on JVM, Scala.js, and Scala Native.

The name comes from Ancient Greek ἱστός (*histós*): loom, mast, warp, web. It is the machine that weaves sources, dependencies, and effects into artifacts.

## 2. Goals

- **Effect-native.** Task execution is a ZIO effect with typed errors, resource safety, interruption, and structured concurrency. The graph stores task descriptions, not the effects. There are no stringly `TaskKey`s.
- **Agent-drivable.** `./histos mcp` is the agent surface. Harnesses call it over stdio. They do not scrape CLI output and they do not need Metals installed.
- **Human-usable.** The CLI is a one-shot command. Metals talks BSP to the same task API.
- **Zero-install for a clone.** `./histos test` downloads the pinned JVM assembly and a suitable JDK when the machine does not already have one.
- **Cross-platform from the start.** JVM, Scala.js, and Scala Native are equal citizens in the project model.
- **Exact cross-Scala.** A project names full Scala versions, such as `3.9.0` and `2.13.16`. A series such as `3` is not a pin.
- **Provider-agnostic publishing.** Maven Central, GitHub Packages, and custom repositories are plugins behind one publisher interface. Publish runs on the local machine.
- **A cache that can match sbt 2.** A local content-addressed store first. An opt-in remote cache next. Remote execution uses the same action later, and stays off until configured.
- **Plugins as ordinary libraries.** A plugin is a Scala 3.9+ library on `histos.api`. The build depends on it like any other library.
- **Histos replaces zipx.** A moved repo does not apply sbt-zipx. CI, affected tests, and version identity live in the engine. Central, docs, docker, deploy, coverage, and the dependency-update bot are Histos plugins.

## 3. Non-Goals

- Supporting arbitrary non-Scala JVM languages as first-class (Java interop is fine; full Kotlin/Groovy is out of scope for v1).
- Backward compatibility with sbt 1.x plugin APIs.
- Replacing Zinc or the Scala compiler. Histos orchestrates them.
- A pure YAML or data-only build format. Build definitions are Scala 3 programs that produce a typed graph.
- A binary-compatibility promise for `histos.api` before 1.0.
- A remote execution farm in the first dogfood.

## 4. Core Principles

1. **Typed graph over stringly configuration.** The build definition constructs projects, platforms, exact Scala versions, dependencies, and task nodes.
2. **One task API.** CLI, BSP, MCP, and ACP call the same operations: inspect the graph, run a task, watch it, cancel it.
3. **Resource-safe execution.** File I/O, processes, and network calls are ZIO. Interruption cleans up. Partial outputs are not cached.
4. **Declared inputs.** Cache keys are content hashes of the inputs a task declares, plus the resolved toolchain. A task that does not declare an input does not get to upload an output.
5. **The build describes CI.** The workflow file is a render of the graph. Drift between the file and the graph fails the build.
6. **One-shot by default.** `./histos test` runs in-process and exits. A long-lived server exists for BSP and MCP.

## 5. Architecture

### 5.1 Project graph

Loading a build is an effect. Its result is data:

```scala
type Load = ZIO[LoadEnv, BuildError, ProjectGraph]
```

`LoadEnv` can see the repo, the JDK lock, and the declared environment. `ProjectGraph` contains:

- Projects (named modules)
- Platforms (`Jvm`, `Js`, `Native`)
- Full Scala versions for the projects being built
- Library and inter-project dependencies
- Task nodes
- Settings (scalac options, and similar)

The project list is fixed when load finishes. A generator cannot add a project.

Load does not compile, test, or publish. Agents, CI generation, and the workflow drift check all read this value. Load also does not resolve libraries. A `Dep` in the graph is a coordinate. The jars are the resolution in the next section.

### 5.2 Dependency resolution

Resolution runs after load and before any task that needs a classpath. Compile, test, and later link consume its result. Inter-project `dependsOn` is not a library coordinate: those class directories are prepended by the task, ahead of the resolved jars.

Coursier is the resolution engine, linked as a library. The `cs` program is not invoked. Coursier types do not leave the adapter. The Coursier coordinate is chosen when the layer is implemented, from the published artifacts, and it is not guessed in this document.

```scala
trait Resolve:
  def resolve(cell: ResolveCell): IO[ResolveError, Classpath]

final case class ResolveCell(
  module: ProjectName,
  dependencies: List[Dep],
  scalaVersion: ScalaVersion,
  platform: Platform,
  repositories: Repositories,
)
```

`Resolve` is the seam. Tests substitute a fixture. The live implementation is the Coursier adapter.

A direct revision is exact. A revision that contains `[`, `]`, `(`, `)`, `+`, or `,`, or that starts with `latest`, is `ResolveError.Range`. A hyphen inside `1.0.0-M1` or `0.5.13-SNAPSHOT` is an exact revision.

The artifact name is a pure function of the dependency, the Scala version, and the platform suffix:

| Kind | JVM | Scala.js | Native |
|------|-----|----------|--------|
| `Java` (`:`) | `name` | `name` | `name` |
| `Binary` (`::`) | `name_3` or `name_2.13` | `name_sjs1_3` | `name_native0.5_3` |
| `Full` (`:::`) | `name_3.9.0` | `name_sjs1_3.9.0` | `name_native0.5_3.9.0` |

`_3` is the Scala 3 binary suffix. `_2.13` is the Scala 2.13 binary suffix. `_sjs1` and `_native0.5` are v1 platform suffixes, named values, not string literals scattered through the fetch. This layer fetches JVM artifacts for whatever exact Scala version the cell names. Fetching a Js or Native cell is `ResolveError.UnsupportedPlatform` until those layers. The name function is still tested for those suffixes, so the later layers do not invent a second scheme.

Transitive revisions come from the published metadata. When several versions of one module are requested, Coursier evicts down to one. Each eviction is a value on the result (`requested` revision, `selected` revision, cross-versioned artifact name). An eviction is not a failure. A module the metadata does not contain is `ResolveError.NotFound`. A checksum that does not match is `ResolveError.Checksum`.

```scala
final case class ResolvedArtifact(
  organization: String,
  name: String,
  revision: String,
  path: Path,
  sha256: Sha256,
)

final case class Eviction(
  organization: String,
  name: String,
  requested: String,
  selected: String,
)

final case class Classpath(artifacts: List[ResolvedArtifact], evictions: List[Eviction])
```

`name` on an artifact is the name after the suffix above. Classpath order is the resolved order with duplicates removed, first occurrence kept. That order is part of the lock.

```scala
enum ResolveError:
  case Range(dependency: Dep)
  case NotFound(organization: String, name: String, revision: String)
  case Checksum(path: Path, expected: Sha256, actual: Sha256)
  case LockDrift(module: ProjectName, scalaVersion: ScalaVersion, platform: Platform)
  case NoLock(path: Path)
  case UnsupportedPlatform(platform: Platform)
  case Lock(error: LockError)
  case Repository(op: RepositoryIo)

enum LockError:
  case Empty
  case BadLine(line: String)
  case DuplicateCell(module: ProjectName, scalaVersion: ScalaVersion, platform: Platform)
  case Missing(field: LockField)
  case BadSha256(raw: String)
  case BadRepository(raw: String)
```

`RepositoryIo` names the operation (`Read`, `Fetch`, `Metadata`). The exception text is not the error. `LockField` is the lock's own fields (`Cell`, `Direct`, `Artifact`, `Evict`). A download that cannot satisfy the locked sha256 stays `Checksum`. The build does not try another repository.

Repositories, in the order they are asked:

```scala
enum Repository:
  case Central
  case IvyLocal
  case Maven(url: RepoUrl)

final case class Repositories(first: Repository, rest: List[Repository]):
  def all: List[Repository] = first :: rest
```

`RepoUrl` is a `file:` or `https:` URL, the same split as a JDK archive. An empty repository list is not representable. The default value is `Repositories(Central, Nil)`. `IvyLocal` is not on that list. The Scala Native layer adds it for its own resolve. Extra Maven repositories are declared by the build. Declared repositories come first. `Central` is last when it is present. Tests pass a `Maven` `file:` repository and do not include `Central`, so the suite does no network.

`.histos/dependencies.lock` is committed. It is the authority for compile and test. One cell per module, Scala version, and platform the graph declares. An aggregate has no cell. Blank lines and `#` comments are ignored. A bad line, a repeated cell, or a missing field is a lock error enum, not a string.

```text
cell core 3.9.0 jvm
direct dev.zio zio 2.1.26 binary
artifact dev.zio zio_3 2.1.26 <sha256> central
artifact org.scala-lang scala-library 2.13.16 <sha256> central
evict dev.zio zio_3 2.1.20 2.1.26
```

`direct` lines are the cell's `Dep`s, in graph order, with kind `java`, `binary`, or `full`. `artifact` lines are the classpath, in order. The repository token is `central`, `ivy-local`, or `maven:<url>`.

Compile and test compare the graph's direct dependencies to the `direct` lines. A difference is `ResolveError.LockDrift`. They do not re-solve versions. A locked jar that is absent locally is downloaded at that revision from the recorded repository, and the bytes must match the locked sha256. A mismatch is `ResolveError.Checksum`. A missing lock is `ResolveError.NoLock`.

`histos resolve` checks the lock against the graph and the local bytes. `histos resolve --update` re-resolves and rewrites the lock. `histos resolve --check` re-resolves and fails if the selected revisions or checksums differ from the lock. Update and check are the network paths. `./histos test` is not.

The compile cache key includes each artifact sha256 in classpath order. The coordinate text is not the key. A jar that keeps its version and changes its bytes is a different compilation.

### 5.3 Tasks and actions

A task node is data: an id, a kind, declared inputs, declared outputs, and dependencies. The scheduler turns a requested node into a ZIO effect.

Core runners (compile, test, publish) live in the daemon. A custom task in the build file binds a local closure to a node. The closure stays in the daemon process. Agents see the node and its declared inputs. They do not see the closure.

A generator (protobuf, OpenAPI, or anything else that writes sources) is a task node. Compile depends on its outputs. It does not run during load.

Execution is one action type. The same action is the unit for local execution, the local content-addressed cache, a later remote cache, and a later remote executor.

```text
digest the action
  -> look up the action cache, if a remote cache is configured
  -> on a hit, fetch outputs from the CAS
  -> on a miss, run locally
  -> if the cache is read-write and the action is sendable, upload
```

Sendable, for the first remote cache, means a core compile: a known runner, declared inputs, no build-file closure. Publish never leaves the machine. Custom closures run locally and are not uploaded. A plugin runner is sendable only when the plugin says it is hermetic.

A test task runs as one subprocess for that project, Scala version, and platform. It does not run inside the server, and it does not fork once per test case. The process gets a fresh work directory, a scrubbed environment, and the declared classpath and inputs. Interruption kills the process group, so `System.exit` in a test cannot take Metals down.

`Network` and `Docker` are declared needs. A task that declares either stays local and is not cacheable. The filesystem jail and the network jail are the same test action, added before any test is sendable. Until that jail exists, tests run locally in the subprocess and are not uploaded.

A cache miss runs locally. Naming a remote cache does not turn on remote execution. Remote execution is a later client on this same action, off unless an executor is configured. There is no second scheduler.

The local cache is a content-addressed store under `~/.cache/histos`, or the XDG cache home. Bytes land under a temporary name and move into place after verification. Concurrent batch and server writers are safe because outputs are addressed by digest.

`histos cache list` shows id, path, size, and last used. `histos cache clean` deletes JDKs, launchers, and compiled plugin sources unused for 30 days, prints what it removed, and keeps the current project's entries. `--dry-run` previews. `histos cache path` prints the root. Nothing is deleted during a build.

Cache keys include source contents, the sha256 of each resolved artifact in classpath order (section 5.2), compiler flags, the full Scala version, the platform, the resolved JDK, and declared environment variables. A Js task that needs Node includes the locked Node build. A Native task includes the full clang version that satisfied the declared major.

### 5.4 Process model

`./histos test` is batch. It does not start a server, and it does not leave a process behind. That is the CI command.

If a server is already running for this repo, and the build fingerprint and Histos version match, the batch command attaches so Zinc is shared with Metals. A missing or stale server falls back to batch.

`./histos mcp` is the process the client owns over stdio. It is not a daemon. BSP and `./histos server` are the long-lived server. A session that wants a daemon starts one. `./histos server` is the explicit opt-in for a human who wants that process left up.

The entry point is the frozen script, not a native image. It downloads the pinned JVM assembly, verifies the checksum, and execs it. When no `java` is on `PATH`, the script fetches the locked JDK first. The engine then applies the toolchain policy and re-execs onto the chosen JDK. Scala Native is not the launcher. It is the toolchain for a project that targets Native.

### 5.5 Protocol surfaces

| Surface | Role | Transport |
|---------|------|-----------|
| **CLI** | One-shot commands. Attaches to a matching server, otherwise runs in-process. | Local process |
| **BSP** | Metals. Starts a server. | Build Server Protocol |
| **MCP** | Agents. Tools are the graph, run, cancel, and toolchain. | MCP over stdio |
| **ACP** | Handshake, then the same capabilities. | Agent Client Protocol |

Histos does not emulate Metals MCP tool names. Metals is a BSP client. An agent config points at `./histos mcp` with the repo as the working directory.

The first commands (`version`, `graph`, `run`, `mcp`, `acp`) are a direct match into a `Command` value. That match stays until a command grows a real option. `cache clean --dry-run` and `toolchain use --lock` against `--local` are the point where the parser should become zio-cli, and zio-cli should parse into that same `Command` enum. zio-cli is still 0.8, and taking it for five subcommands would pull in `zio-streams` and `zio-process` for no grammar those libraries serve.

ACP in the skeleton answers `initialize`. Production ACP follows the MCP tools. Neither protocol contains build logic. The handlers call the task API.

### 5.6 Bootstrap and JDK

A repo commits:

- `./histos` and `histos.bat`. Frozen scripts. They read the version pin, download that JVM assembly, verify it, and exec it.
- `.histos-version`. The launcher version.
- `.histos/jdk.lock`. The exact Temurin build, URL, and sha256.

The lock is one field per line: `major`, `version`, `url`, and `sha256`. Blank lines and lines starting with `#` are ignored. `url` is a `file:` or `https:` URL. The major has to be the major of `version`. The script reads the lock only when `java` is not on `PATH`. It fetches that url, checks the sha256, unpacks into the Histos JDK cache, and execs that `java`. The engine does not fetch `https`. When the policy says to provision and the url is a `file:` zip, the engine stages it into the same cache and re-execs onto it. An `https` lock on a machine that already has some other `java` stops with the provision report instead of downloading.

`histos bootstrap` writes the scripts and `.histos-version`. It is idempotent. It does not write `histos.scala` or CI. A maintainer has a Histos binary once, runs the command, and commits the files. Everyone else uses `./histos`.

The build names a JDK major. `./histos test` uses the lock and verifies the checksum. `histos jdk update` rewrites the lock. One default toolchain per build.

Detection of an existing JDK is read-only. The order is:

1. An explicit override.
2. `JAVA_HOME`, when it is a JDK of the pinned major (`javac` is present).
3. `java` on `PATH`, under the same rule.
4. A JDK already unpacked in the Coursier cache.
5. An sdkman candidate already installed.
6. A JDK already in the Histos cache.

The probe does not call `cs java`, `cs java-home`, or `sdk install`. Those commands install, and `sdk install` prompts to change the default JDK.

When Coursier or sdkman is present and already has a match, Histos uses it. When a manager is present and the pin is missing, Histos prints that manager's install command and stops with a typed error. It does not install into the manager, and it does not hide a private JDK beside it. If both managers exist and neither has a match, the command is `cs java-home --jvm temurin:<major>`, because that name stays stable. An sdkman-only machine is told to run `sdk list java` and install the matching Temurin id.

A bare machine, with no suitable JDK and neither manager, gets a private Temurin in the Histos cache. It is not placed on `PATH` and it is not registered with a manager.

Node follows the same rules, and only when a Js task needs Node. The build names a Node major. `.histos/node.lock` stores the exact build, URL, and sha256. nvm, fnm, and volta count as managers. A manager that lacks the pin gets an install command. A bare machine that actually runs Js gets a private Node in the Histos cache. A JVM-only repo does not probe Node.

A Native project declares a clang major. Histos checks `clang` on `PATH` and does not provision one. The cache key includes the full version of the binary that matched. A miss is a typed error that lists what was found and how to install it.

`histos toolchain` prints every candidate considered, why it was rejected or chosen, the version, the path, and whether the decision came from the lock or a local override. `histos toolchain use --lock` rewrites the committed lock. `histos toolchain use --local` writes `.histos/toolchain.local`, which is gitignored. CI does not read that file. When a local override is active, the build prints one line. An unchanged lock hit stays quiet. The first time a private JDK or Node is provisioned, one line names the path and `histos cache clean`.

### 5.7 Plugins

Plugins compile only against `histos.api`. histos and ZIO are provided by the launcher. The engine and the scheduler are not on the plugin compile classpath.

There is no binary-compatibility promise before 1.0. `LinkStrategy.Api` is a typed error until then. At 1.0, MiMa applies to `histos.api` only.

An author publishes one of:

- **ExactBinary.** A jar built for one Histos version.
- **Source.** A source jar the launcher compiles.
- **BinaryOrSource.** The jar when the launcher matches, otherwise the sources.

An in-repo `plugin(path(...))` is source.

The loader uses the binary on an exact Histos match. Otherwise it compiles the source jar and reports `PluginLinked.FromSource` with both versions. The adopter may force `.link(ExactBinary)` or `.link(Source)`. Forcing a binary that does not match is a typed mismatch that names the plugin and both versions. Compiled sources are cached under the Histos cache and follow the 30-day clean.

A plugin contributes task nodes and runners. It may mark a task as a CI job (when it runs, which task id). The workflow renderer emits that job without knowing what the task does.

### 5.8 Platforms and Scala versions

**Platforms.** `Jvm`, `Js`, and `Native`. A project targets one or more. Shared sources live in a common directory. Platform-specific sources live in `jvm/`, `js/`, and `native/`.

**Scala versions.** The graph names full versions. Moving to a new compiler is a committed edit. Version-specific sources use `scala-2.13/`, `scala-3/`, or a more specific directory.

**Tool and plugins.** Histos and every plugin are compiled with Scala 3.9+. They never appear on the classpath of the projects being built.

### 5.9 Publishing and versions

Publishing is a task implemented by a plugin.

```scala
trait Publisher:
  def publish(artifact: Artifact, credentials: Credentials): ZIO[Scope, PublishError, Published]
```

The Central plugin is the one early-effect libraries need. GitHub Packages, other Maven repositories, and a local `.m2` are further plugins. Credentials and signing are layers. Provider choice is configuration.

Publish runs locally, including in CI. It is not cached and it is not sent to a remote executor.

Version identity is the zipx scheme, implemented as Histos tasks, so existing pins still resolve:

- Everyday builds compile `<version>-ci`.
- A snapshot publish stores `<version>-<sha>-SNAPSHOT`, which is the coordinate a downstream catalog pins.
- The default branch also updates a `<version>-SNAPSHOT` pointer. That pointer is not a dependency.
- A Ship number is the next release.
- Once that number is on the release registry, further snapshots of it publish nothing.
- The drift gate fails the build when a row has moved on.
- The release job opens a pull request that bumps the catalog. CI does not commit the catalog on the tagged SHA.

Changing this scheme would be an organization-wide catalog migration. Histos does not invent a second one.

### 5.10 Build definition

The file is ordinary Scala 3. Projects are values, so one project can depend on another by reference. A loop may build the module list. `scala"3.9.0"` and `dep"dev.zio::zio:2.1.26"` are inline literals. A malformed literal fails when the build file compiles. Builder calls capture source positions.

v1 has one front end. `module` starts a compiling project. Platforms and Scala versions are required arguments, so forgetting either is a compile error. Further settings, including plugin extensions, are immutable methods that return a new value. `aggregate` is the constructor for a root that does not compile.

```scala
import histos.*

object build extends HistosBuild:
  def graph =
    val core =
      module("core", Jvm, Js, Native)(scala"3.9.0", scala"2.13.16")
        .libraryDependencies(dep"dev.zio::zio:2.1.26")

    val tests =
      module("tests", Jvm)(scala"3.9.0", scala"2.13.16")
        .dependsOn(core)

    Projects(aggregate("root", core, tests), core, tests)
```

Evaluating `graph` does not compile or publish. A client runs a task by id.

The graph is the model. A later front end, including a context-function block, is allowed only if it produces the same `ProjectGraph`. v1 does not ship that second DSL. A structural macro is not a v1 surface. `inline` is limited to literals and positions. Plugins stay ordinary methods and libraries.

### 5.11 CI

Histos replaces sbt-zipx. The graph renders `.github/workflows/ci.yml`. A drift check fails the build when the committed file disagrees.

The default workflow is Aggregate: `./histos test` in one job, plus format and the workflow check, plus a publish job on tags. On a pull request, the test job runs the changed projects and their dependents. Elsewhere it runs the full graph.

Fan-out is one split model, not a second renderer. The graph holds a set of axes: `Platform`, `ScalaVersion`, and `Module`. The empty set is Aggregate, and it is the default. A non-empty set is a matrix. On a pull request the matrix is filtered by the affected set, so an unchanged cell is not a job. There is no wave scheduler and no separate Layer or Graph mode. Ordering is `dependsOn` plus the content-addressed cache.

The runner needs a shell and curl. The launcher provisions the JDK. The workflow does not need `setup-java`.

`./histos test` in CI is the batch command. No server is started. Publish is a separate local job with CI credentials.

Central, docs, docker, deploy, coverage, and the scheduled dependency-update bot are plugins. Each adds tasks, and any of those tasks can be marked as a CI job. A repo drops sbt-zipx when every capability it uses has a Histos plugin. A library can drop it once CI, test, and the Central plugin exist.

sbt-splice's job (bundling Scala.js) is a Histos plugin, not part of the engine.

## 6. Adoption

A cloner of an existing Histos repo runs `./histos test`. Nothing else is installed first.

A maintainer moving a repo:

1. Runs `histos bootstrap` and commits `./histos`, `histos.bat`, and `.histos-version`.
2. Writes `histos.scala` by hand: the modules, platforms, and exact Scala versions. Pins the JDK major and commits `.histos/jdk.lock`. Runs `histos resolve --update` and commits `.histos/dependencies.lock`. A repo with Js also commits `.histos/node.lock`. A repo with Native declares a clang major. `histos toolchain` shows which binary was chosen.
3. Lets Histos render the workflow and commits it. The drift check keeps it aligned.

The first repo is hand-written. An sbt importer waits until a second repo repeats the same shape. The builder is allowed to change during that first migration.

No change to source layout is required for `src/main/scala`, `src/main/scala-2.13`, and the platform directories. Cross-platform projects map onto the platform model.

Remote cache stays off until the build names one. The default mode is read-only. Write is explicit. Laptops without a credential do not fail the build for lack of a cache.

## 7. Phased Roadmap

**Phase 0. Skeleton.**
Bootstrap scripts, JVM assembly entry, ZIO app, typed graph, batch CLI, MCP tools (`graph`, `run`, `cancel`), ACP `initialize`. The v1 builder (`module`, `aggregate`, then an immutable chain) is the front end the skeleton loads. `Projects` returns `Either[BuildError, ProjectGraph]`. The sample omits that type.

**Phase 1. Local engine and CI.**
Scheduler, local content-addressed cache, JDK lock and provisioning, `histos toolchain`, Zinc, dependency resolution and `.histos/dependencies.lock`, and JVM compile and test for one exact Scala 3 version. Tests run as one subprocess per test task. Workflow render, drift check, and affected tests land here, for the JVM tasks that exist. The split axes are already in the model. The default is the empty set (Aggregate). A repo that sets `Module` gets one cell per module from this same renderer.

**Phase 2. Cross-build and versions.**
Scala.js, Scala Native, exact Scala 2.13 versions. Node lock for Js tasks. Clang major check for Native tasks. The same renderer emits `Platform` and `ScalaVersion` cells, filtered by the affected set on pull requests. Version catalog and the zipx snapshot/release names. Central plugin, enough for a library to publish without sbt-zipx.

**Phase 3. Protocols and remote cache.**
BSP for Metals. Production MCP, including `toolchain`. Remote cache for sendable compile actions, opt-in, miss falls back to local.

**Phase 4. Dogfood.**
One hand-written early-effect library. Generated CI. Drop sbt-zipx for the capabilities that library uses. Measure against sbt 2 and Mill. Filesystem and network jail on the test action, before any test is sendable.

**Phase 5. The rest of the zipx surface, then execution.**
Docs, docker, deploy, coverage, and the dependency-update bot, as plugins. sbt importer once a second repo repeats the shape. Remote execution client, still off by default. `LinkStrategy.Api` and MiMa on `histos.api` at 1.0.

## 8. Review close

The builder surface, the test subprocess, Node and clang resolution, and CI fan-out were decided in the same review as sections 5 through 7. This draft does not leave them open. A context-function front end is a later producer of the same `ProjectGraph`, and it is not part of v1.

The native-image launcher from that review is withdrawn. The script is the launcher. The engine is a JVM process. Scala Native stays a target platform: the linker is the Scala Native `tools` library and the native libraries, not an sbt plugin, and not a bootstrap binary.

## 9. Building Histos

Histos is built with scala-cli until it can build itself. This repository does not run CI until dogfood has a generated workflow. No sbt, and no sbt-zipx, on the Histos build.

---

*This document is a living draft. Sections 5 through 8 record the design review of 2026-10-16. Section 9 and the launcher sentences record the implementation amendments of the same day.*
