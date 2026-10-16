# Histos Design Document

**Status:** Draft  
**Date:** 2026-10-16  
**Org:** early-effect  
**Repo:** `early-effect/histos`

## 1. Vision

Histos is an effect-oriented, agent-native build tool for the Scala ecosystem. It treats the build as a first-class ZIO program: tasks are `ZIO[R, E, A]`, the module graph is a typed AST, and every capability (compile, test, publish, inspect) is exposed through the same protocol surface to humans, IDEs, and coding agents.

It is designed so that every early-effect project can adopt it directly, with first-class support for cross-platform (JVM, Scala.js, Scala Native) and Scala 2.13 / 3 cross-building from day one. The tool itself and all plugins are written in Scala 3.9+.

The name comes from Ancient Greek ἱστός (*histós*): loom, mast, warp, web. It is the machine that weaves sources, dependencies, and effects into artifacts.

## 2. Goals

- **Effect-native.** Every task is a ZIO effect with typed errors, resource safety, interruption, and structured concurrency. No stringly `TaskKey`s.
- **Agent-drivable.** First-class ACP (Agent Client Protocol) and MCP surfaces so harnesses and agents can drive the build without scraping CLI output.
- **Human-usable.** Same surfaces exposed via a thin CLI and full BSP so Metals works out of the box.
- **Cross-platform from the start.** JVM, Scala.js, and Scala Native are equal citizens in the project model.
- **Cross-Scala support.** Projects can cross-build Scala 2.13 and Scala 3. The tool and all plugins require Scala 3.9+.
- **Provider-agnostic publishing.** Publish to Maven Central, Sonatype, GitHub Packages, or custom repositories through a single typed interface. No hard-coded publisher.
- **Caching that matches or exceeds sbt 2.** Local disk + remote Bazel-compatible (action cache + content-addressable store) caching, with precise input hashing and automatic remote readiness.
- **Extensible via plugins.** Plugins are Scala 3.9+ ZIO services / layers. The build definition can depend on them as ordinary libraries.
- **Direct adoption by early-effect.** Every project in the org (heddle, ascent, mechanoid, saferis, conduit, specular, mermoid, etc.) should be able to migrate with minimal ceremony. The build remains the source of truth for CI.

## 3. Non-Goals

- Supporting arbitrary non-Scala JVM languages as first-class (Java interop is fine; full Kotlin/Groovy is out of scope for v1).
- Backward compatibility with sbt 1.x plugin APIs.
- Replacing Zinc or the Scala compiler. Histos orchestrates them.
- A pure YAML/data-only build format. Build definitions are Scala 3 code that produce a typed AST.

## 4. Core Principles

1. **Typed AST over stringly configuration.** The build definition is a Scala 3 program that constructs a strongly typed graph of projects, platforms, Scala versions, dependencies, and tasks.
2. **One capability surface.** Compile, test, run, publish, inspect, format, and dependency resolution are available to CLI, BSP, MCP, and ACP. Humans and agents use the same model.
3. **Resource-safe by construction.** All file I/O, process management, and network calls are managed by ZIO. Interruption and cleanup are guaranteed.
4. **Hermetic tasks.** Tasks declare their inputs (sources, dependencies, flags, toolchain versions). Cache keys are content hashes of those inputs.
5. **Build is the source of truth.** CI configuration, publishing targets, and cross-build matrices are derived from the module graph (consistent with zipx).

## 5. Architecture

### 5.1 Task Graph and ZIO Model

A Histos build is a value of type roughly:

```scala
type Build = ZIO[BuildEnv, BuildError, ProjectGraph]
```

`ProjectGraph` is an immutable AST containing:

- Projects (named modules)
- Platforms (`Jvm`, `Js`, `Native`)
- Scala versions (for the *projects being built*, not the tool)
- Dependencies (library and inter-project)
- Tasks (compile, test, package, publish, custom)
- Settings (scalac options, resource generators, etc.)

Tasks are ZIO effects. The scheduler is a ZIO-based fiber graph that:

- Tracks declared inputs and outputs.
- Computes content-addressed cache keys.
- Parallelizes independent tasks.
- Propagates interruption and resource cleanup.

### 5.2 Daemon

A long-lived daemon process holds the loaded build graph, Zinc analysis, and local cache. The CLI, BSP, MCP, and ACP clients all talk to the daemon. Cold-start latency is mitigated by a native-image or Graal launcher (same pattern as sbt 2 / Mill).

### 5.3 Protocol Surfaces

| Surface | Purpose | Transport |
|---------|---------|-----------|
| **CLI** | Human and script entry point | Thin client over daemon |
| **BSP** | Metals / IDE integration | Build Server Protocol |
| **MCP** | Agent tools (compile, test, inspect, etc.) | MCP (stdio or HTTP) |
| **ACP** | Coding agent sessions | Agent Client Protocol (JSON-RPC / stdio) |

All surfaces share the same task and project model. Agents can request structured diagnostics, stream progress, cancel, and inspect the graph.

### 5.4 Caching Strategy

Histos implements a hybrid local + remote cache that is Bazel-compatible and at least as capable as sbt 2.

**Local cache**
- Machine-wide disk cache keyed by content hashes.
- Action results and output files stored separately.
- Automatic invalidation based on declared inputs.

**Remote cache**
- Bazel Remote Execution API compatible (action cache + CAS).
- Supports both gRPC and HTTP/1.1 backends (bazel-remote, BuildBuddy, custom, etc.).
- Authentication, TLS, and mTLS supported.
- Read-only, read-write, or disabled per-invocation or per-task.

**Cache key construction**
- Hash of: source files (content), resolved dependency artifacts (or their hashes), compiler flags, Scala version, platform, JDK/toolchain version, and any declared environment variables.
- Tasks that declare outputs are automatically remote-ready.

**Improvements over sbt 2**
- Explicit input declarations in the typed AST make keys more precise and less prone to false misses.
- ZIO resource safety guarantees that partial outputs are never cached.
- Structured diagnostics and logs are first-class cacheable artifacts.
- Optional remote execution (future) can reuse the same action model.

### 5.5 Plugins and Extensions

Plugins are ordinary Scala 3.9+ libraries that provide ZIO layers or task constructors. A build definition depends on them via normal library dependencies. There is no separate plugin classpath or dynamic classloader magic for the core tool.

Example:

```scala
// In the build definition (Scala 3.9+)
import histos.plugin.publishing.sonatype.*

val publish = sonatypePublishTask(/* ... */)
```

All plugins must target Scala 3.9+. The tool refuses to load plugins compiled against earlier versions.

### 5.6 Cross-Platform and Cross-Scala

**Platforms (first-class)**
- `Jvm`, `Js` (Scala.js), `Native` (Scala Native).
- A project can target one or more platforms. Shared sources live in a common directory; platform-specific sources in `jvm/`, `js/`, `native/`.
- Cross-platform dependencies use the standard `%%%` (or equivalent) resolution.

**Scala versions (for projects being built)**
- Projects declare `scalaVersions: Set[ScalaVersion]` (e.g., `2.13.16`, `3.9.0`).
- The tool generates the appropriate cross-build matrix.
- Version-specific sources: `scala-2.13/`, `scala-3/`, or more granular.
- Binary compatibility helpers (e.g., `CrossVersion.for3Use2_13`) are available but discouraged for library authors.

**Tool and plugins**
- Histos itself and every plugin are compiled with Scala 3.9+.
- They never appear on the classpath of the projects being built.

### 5.7 Publishing

Publishing is a first-class task, not a hard-coded Sonatype plugin.

```scala
trait Publisher:
  def publish(artifact: Artifact, credentials: Credentials): ZIO[Scope, PublishError, Published]
```

Implementations for:
- Maven Central / Sonatype Central
- GitHub Packages
- Custom Maven / Ivy repositories
- Local `.m2`

Credentials and signing are injected as ZIO layers. The same publish task works for all platforms and Scala versions. Provider selection is configuration, not code changes.

### 5.8 Build Definition Format

A minimal `histos.scala` (or `build.histos`):

```scala
import histos.*

object Build extends HistosApp:
  def build = for
    core <- project("core")
              .platforms(Jvm, Js, Native)
              .scalaVersions(ScalaVersion.`2.13`, ScalaVersion.`3`)
              .dependencies(/* ... */)
    _ <- core.task.compile
    _ <- core.task.test
    _ <- core.task.publish(sonatype)
  yield ()
```

The definition is compiled by the daemon and produces the typed graph. Agents can introspect the graph without executing tasks.

## 6. Adoption Path for early-effect

Every early-effect project should be able to migrate by:

1. Adding a `histos.scala` that declares the existing modules, platforms, and Scala versions.
2. Pointing CI at `histos test` / `histos publish` (or letting zipx-style generation produce the workflow).
3. Optionally enabling remote cache for CI and local development.

No change to source layout is required for projects that already use standard `src/main/scala`, `src/main/scala-2.13`, etc. Cross-platform projects (ascent, conduit, heddle, etc.) map directly onto the platform model.

sbt-zipx, sbt-dynver-ci, and sbt-splice concepts will be re-implemented as Histos plugins or core tasks so the “build describes CI” and “stable artifact names” properties are preserved.

## 7. Phased Roadmap

**Phase 0 – Skeleton**  
Repo, ZIO App, typed AST stub, daemon, stub ACP `initialize` + MCP tools, CLI.

**Phase 1 – Core Engine**  
Task scheduler, local cache, Zinc integration, basic JVM compile/test for Scala 3.9+.

**Phase 2 – Cross-Platform & Cross-Scala**  
Scala.js, Scala Native, Scala 2.13 cross-building. Provider-agnostic publishing.

**Phase 3 – Protocols**  
Full BSP for Metals. Production MCP and ACP. Remote Bazel cache.

**Phase 4 – Dogfood**  
Migrate 1–2 early-effect libraries. Measure against sbt 2 and Mill. CI generation.

**Phase 5 – Ecosystem**  
Plugin registry, remote execution (optional), importer from sbt/Mill.

## 8. Open Questions

- Exact shape of the public AST / DSL (macros vs. plain Scala vs. hybrid).
- Whether remote execution is in scope for v1 or deferred.
- Integration depth with Metals MCP vs. a native Histos MCP server.
- Handling of non-Scala source generators and external tools (e.g., protobuf, OpenAPI).
- Binary compatibility policy for the Histos plugin API itself.

---

*This document is a living draft. Feedback and concrete adoption requirements from early-effect projects are welcome.*
