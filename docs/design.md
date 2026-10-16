# Histos Design Document

**Status:** Draft
**Date:** 2026-10-16
**Org:** early-effect
**Repo:** `early-effect/histos`

## 1. Vision

Histos is an effect-oriented, agent-native build tool for the Scala ecosystem. Evaluating a build produces an immutable project graph. Tasks run as ZIO effects. Compile, test, publish, and inspect share one task API for the CLI, BSP, MCP, and ACP.

A clone of a Histos project builds with `./histos`. The repo carries a small bootstrap and a version pin. The launcher is a native image. It does not need a JDK already installed.

Histos replaces sbt-zipx for a repo that has moved. The build describes CI. Artifact names stay the ones downstream catalogs already pin.

The tool and every plugin are Scala 3.9+. Projects being built may cross-build Scala 2.13 and Scala 3, on JVM, Scala.js, and Scala Native.

The name comes from Ancient Greek ἱστός (*histós*): loom, mast, warp, web. It is the machine that weaves sources, dependencies, and effects into artifacts.

## 2. Goals

- **Effect-native.** Task execution is a ZIO effect with typed errors, resource safety, interruption, and structured concurrency. The graph stores task descriptions, not the effects. There are no stringly `TaskKey`s.
- **Agent-drivable.** `./histos mcp` is the agent surface. Harnesses call it over stdio. They do not scrape CLI output and they do not need Metals installed.
- **Human-usable.** The CLI is a one-shot command. Metals talks BSP to the same task API.
- **Zero-install for a clone.** `./histos test` downloads the pinned launcher and a suitable JDK when the machine does not already have one.
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

Load does not compile, test, or publish. Agents, CI generation, and the workflow drift check all read this value.

### 5.2 Tasks and actions

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

Sendable, for the first remote cache, means a core compile: a known runner, declared inputs, no build-file closure. Tests stay local until a sandbox exists. Publish never leaves the machine. Custom closures run locally and are not uploaded. A plugin runner is sendable only when the plugin says it is hermetic.

A cache miss runs locally. Naming a remote cache does not turn on remote execution. Remote execution is a later client on this same action, off unless an executor is configured. There is no second scheduler.

The local cache is a content-addressed store under `~/.cache/histos`, or the XDG cache home. Bytes land under a temporary name and move into place after verification. Concurrent batch and server writers are safe because outputs are addressed by digest.

`histos cache list` shows id, path, size, and last used. `histos cache clean` deletes JDKs, launchers, and compiled plugin sources unused for 30 days, prints what it removed, and keeps the current project's entries. `--dry-run` previews. `histos cache path` prints the root. Nothing is deleted during a build.

Cache keys include source contents, resolved dependency artifacts, compiler flags, the full Scala version, the platform, the locked JDK build, and declared environment variables.

### 5.3 Process model

`./histos test` is batch. It does not start a server, and it does not leave a process behind. That is the CI command.

If a server is already running for this repo, and the build fingerprint and Histos version match, the batch command attaches so Zinc is shared with Metals. A missing or stale server falls back to batch.

BSP and MCP start a server. A session needs one. `./histos server` is the explicit opt-in for a human who wants the daemon left up.

The launcher is a native image, on the Mill pattern, so the process that decides all of this does not need a JDK to start.

### 5.4 Protocol surfaces

| Surface | Role | Transport |
|---------|------|-----------|
| **CLI** | One-shot commands. Attaches to a matching server, otherwise runs in-process. | Local process |
| **BSP** | Metals. Starts a server. | Build Server Protocol |
| **MCP** | Agents. Tools are the graph, run, and cancel. | MCP over stdio |
| **ACP** | Handshake, then the same capabilities. | Agent Client Protocol |

Histos does not emulate Metals MCP tool names. Metals is a BSP client. An agent config points at `./histos mcp` with the repo as the working directory.

ACP in the skeleton answers `initialize`. Production ACP follows the MCP tools. Neither protocol contains build logic. The handlers call the task API.

### 5.5 Bootstrap and JDK

A repo commits:

- `./histos` and `histos.bat`. Frozen scripts. They read the version pin, download that native launcher, verify it, and exec it.
- `.histos-version`. The launcher version.
- `.histos/jdk.lock`. The exact Temurin build, URL, and sha256.

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

### 5.6 Plugins

Plugins compile only against `histos.api`. histos and ZIO are provided by the launcher. The engine and the scheduler are not on the plugin compile classpath.

There is no binary-compatibility promise before 1.0. `LinkStrategy.Api` is a typed error until then. At 1.0, MiMa applies to `histos.api` only.

An author publishes one of:

- **ExactBinary.** A jar built for one Histos version.
- **Source.** A source jar the launcher compiles.
- **BinaryOrSource.** The jar when the launcher matches, otherwise the sources.

An in-repo `plugin(path(...))` is source.

The loader uses the binary on an exact Histos match. Otherwise it compiles the source jar and reports `PluginLinked.FromSource` with both versions. The adopter may force `.link(ExactBinary)` or `.link(Source)`. Forcing a binary that does not match is a typed mismatch that names the plugin and both versions. Compiled sources are cached under the Histos cache and follow the 30-day clean.

A plugin contributes task nodes and runners. It may mark a task as a CI job (when it runs, which task id). The workflow renderer emits that job without knowing what the task does.

### 5.7 Platforms and Scala versions

**Platforms.** `Jvm`, `Js`, and `Native`. A project targets one or more. Shared sources live in a common directory. Platform-specific sources live in `jvm/`, `js/`, and `native/`.

**Scala versions.** The graph names full versions. Moving to a new compiler is a committed edit. Version-specific sources use `scala-2.13/`, `scala-3/`, or a more specific directory.

**Tool and plugins.** Histos and every plugin are compiled with Scala 3.9+. They never appear on the classpath of the projects being built.

### 5.8 Publishing and versions

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

### 5.9 Build definition

The file is ordinary Scala 3. Projects are values, so one project can depend on another by reference. A loop may build the module list. `scala"3.9.0"` and `dep"dev.zio::zio:2.1.26"` are inline literals. A malformed literal fails when the build file compiles. Builder calls capture source positions. A missing required field, such as Scala versions, is a typed load error with that position.

The concrete builder shape (a statement block, or a chained builder) is not settled. This sketch shows the information the graph holds, not the final method names:

```scala
import histos.*

object build extends HistosBuild:
  def graph =
    val core = project("core"):
      platforms(Jvm, Js, Native)
      scalaVersions(scala"3.9.0", scala"2.13.16")
      libraryDependencies(dep"dev.zio::zio:2.1.26")

    val tests = project("tests"):
      dependsOn(core)
      platforms(Jvm)
      scalaVersions(scala"3.9.0", scala"2.13.16")

    Projects(core, tests)
```

Evaluating `graph` does not compile or publish. A client runs a task by id.

A structural macro is not the v1 authoring surface. `inline` is limited to literals and positions. Plugins stay ordinary methods and libraries.

### 5.10 CI

Histos replaces sbt-zipx. The graph renders `.github/workflows/ci.yml`. A drift check fails the build when the committed file disagrees.

The default workflow is aggregate, one job per verify task, in the zipx sense: `./histos test` in one job, plus format and the workflow check, plus a publish job on tags. On a pull request, test runs the changed projects and their dependents. Elsewhere it runs the full graph. Splitting jobs by Scala version or platform is a setting on the graph, off by default. Layer and Graph fan-out from zipx are that setting, later. Aggregate is the default a repo gets now.

The runner needs a shell and curl. The launcher provisions the JDK. The workflow does not need `setup-java`.

`./histos test` in CI is the batch command. No server is started. Publish is a separate local job with CI credentials.

Central, docs, docker, deploy, coverage, and the scheduled dependency-update bot are plugins. Each adds tasks, and any of those tasks can be marked as a CI job. A repo drops sbt-zipx when every capability it uses has a Histos plugin. A library can drop it once CI, test, and the Central plugin exist.

sbt-splice's job (bundling Scala.js) is a Histos plugin, not part of the engine.

## 6. Adoption

A cloner of an existing Histos repo runs `./histos test`. Nothing else is installed first.

A maintainer moving a repo:

1. Runs `histos bootstrap` and commits `./histos`, `histos.bat`, and `.histos-version`.
2. Writes `histos.scala` by hand: the modules, platforms, and exact Scala versions. Pins the JDK major and commits `.histos/jdk.lock` from `histos jdk update`.
3. Lets Histos render the workflow and commits it. The drift check keeps it aligned.

The first repo is hand-written. An sbt importer waits until a second repo repeats the same shape. The builder is allowed to change during that first migration.

No change to source layout is required for `src/main/scala`, `src/main/scala-2.13`, and the platform directories. Cross-platform projects map onto the platform model.

Remote cache stays off until the build names one. The default mode is read-only. Write is explicit. Laptops without a credential do not fail the build for lack of a cache.

## 7. Phased Roadmap

**Phase 0. Skeleton.**
Bootstrap script, native launcher, ZIO app, typed graph, batch CLI, MCP tools (`graph`, `run`, `cancel`), ACP `initialize`.

**Phase 1. Local engine.**
Scheduler, local content-addressed cache, JDK lock and provisioning, Zinc, JVM compile and test for one exact Scala 3 version.

**Phase 2. Cross-build, versions, CI.**
Scala.js, Scala Native, exact Scala 2.13 versions. Version catalog and the zipx snapshot/release names. Workflow render and drift check. Central plugin, enough for a library to publish without sbt-zipx.

**Phase 3. Protocols and remote cache.**
BSP for Metals. Production MCP. Remote cache for sendable compile actions, opt-in, miss falls back to local.

**Phase 4. Dogfood.**
One hand-written early-effect library. Generated CI. Drop sbt-zipx for the capabilities that library uses. Measure against sbt 2 and Mill.

**Phase 5. The rest of the zipx surface, then execution.**
Docs, docker, deploy, coverage, and the dependency-update bot, as plugins. sbt importer once a second repo repeats the shape. Remote execution client, still off by default, after a test sandbox exists. `LinkStrategy.Api` and MiMa on `histos.api` at 1.0.

## 8. Not yet decided

- The concrete builder surface: a statement block, or a chained builder.
- How the test sandbox works. Tests stay local until it exists.
- Which Node and clang versions a remote worker would carry. JS and Native toolchains are not pinned yet.
- When the workflow renderer implements Layer and Graph fan-out. Aggregate is the default.

---

*This document is a living draft. The decisions in sections 5 through 7 were settled in design review on 2026-10-16.*
