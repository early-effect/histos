# Histos implementation stack

The design in `docs/design.md` stands, with the amendments below. The first four are layer 1, so later layers are not implementing a document that still says the opposite. The fifth is the dependency-resolution specification in design section 5.2.

## Amendments

1. The launcher is the frozen `./histos` and `histos.bat`. They download the pinned JVM assembly, verify it, and exec it. There is no Scala Native launcher and no Rust launcher. A native image is bloat for a downloader, and a second language is the same cost. JDK policy lives in the JVM engine. The script only fetches the locked Temurin when no `java` exists, then the engine re-checks the lock and re-execs if needed.
2. Histos itself is built with scala-cli 1.18 (Scala 3.9.0) until it can build itself. No sbt. No zipx. No GitHub Actions on this repo until dogfood.
3. The engine is a JVM process. ZIO, load, Zinc, MCP, and the Scala Native linker (`tools_3`) run there.
4. Scala Native is a target toolchain, not a build plugin. The jars already in `~/.ivy2/local` are `org.scala-native` `0.5.13-SNAPSHOT` (`tools_3`, `nir_3`, `util_3`, `nscplugin_3.9.0`, `nativelib_native0.5_3`, `scala3lib` / `scalalib` at `3.9.0+0.5.13-SNAPSHOT`). That nativelib is the Mac Mini M6 unwind fix (scala-native#5109). The native layer resolves them with `//> using repository ivy2Local` while Histos is built by scala-cli. When that layer of Histos itself fetches them, it adds `IvyLocal` to the repository list for that resolve. `IvyLocal` is not a default repository. When 0.5.13 is on Maven Central, that layer drops `IvyLocal` and the snapshot pin.
5. Library resolution is Coursier linked as a library. The `cs` program is not a resolver. The specification is section 5.2 of the design. The layer sits after Zinc and before the test subprocess, because a classpath of real dependencies has to exist before a test is defined as running on one.

`heddle-mcp` is the MCP server. ACP `initialize` is a small handshake in histos. There is no ACP library.

## Shape

```
api/      pure graph, builder, literals. No ZIO on the main classpath.
engine/   Load, tasks, later scheduler, cache, Zinc, resolution, native link.
app/      CLI, MCP, ACP. One ZIOApp.
```

Each directory is its own scala-cli project (`project.scala` only; source files carry no directives). `engine` and `app` pull sources with `//> using file ../api/src` and `//> using file ../engine/src`. The build-file compiler does not see those sources. It sees the api jar from `scala-cli --power package api --library`.

Tests are ZIO Test (`zio-test` and `zio-test-sbt` as test deps, framework `zio.test.sbt.ZTestFramework`). The api jar does not depend on ZIO. Verify with `scala-cli test <dir>`. There is no sbt, so `testFull` does not apply. No CI cost until dogfood: each layer is proven locally. Do not add required checks before a workflow exists, or the stack cannot merge.

Packages: `histos.*` is the DSL. Engine code is `histos.load`, `histos.task`, and later `histos.cache`, `histos.zinc`, `histos.resolve`, `histos.native`. App code is `histos.cli`, `histos.mcp`, `histos.acp`.

Errors are enums. `BuildError` (pure graph), `LoadError` (compiler and IO), `TaskError` (run and cancel), `ResolveError` (resolution and the dependency lock). No `Either[String, _]`, no `throw`, no partial accessors, no `asInstanceOf`.

ZIO version is whatever published `heddle-mcp` uses. Do not pin a second ZIO. The GitHub README currently shows heddle `0.11.0`; the heddle tree's next ship is `0.13.0`. The MCP layer resolves the Central coordinate and checks that `Mcp.stdio(pipe)` and `Mcp.native` exist. If the published jar is short, publish heddle to ivy2Local the same way as Scala Native and pin that. Do not fork the protocol.

## Types that later layers must not repaint

```scala
def module(name: String, platform: Platform, platforms: Platform*)(
    version: ScalaVersion, versions: ScalaVersion*
): ModuleBuilder

def aggregate(name: String, project: Project, projects: Project*): Aggregate
```

One platform and one Scala version are required by the signature, so an empty list is a compile error of the build file. `scala"..."` and `dep"..."` are inline literals. A bad literal fails compilation of the build file. Duplicate names and cycles are `BuildError` from a pure `Projects.build: Either[BuildError, ProjectGraph]`.

`TaskKind` starts as `Inspect`. Later layers add enum cases (`Compile`, `Test`, `Publish`) and update the matches. A custom-task closure is not a Phase 0 constructor.

```scala
trait Loader:
  def load(repo: Path, apiJar: Path): ZIO[Scope, LoadError, LoadedBuild]

trait Tasks:
  def run(id: TaskId): ZIO[Scope, TaskError, TaskResult]
  def cancel(run: RunId): IO[TaskError, Unit]
```

`LoadedBuild` holds the graph and the build classloader. The `Scope` closes the classloader. Load uses the Scala 3.9.0 compiler in-process (`scala3-compiler`). That compiler is not on the classpath of the project being built. Phase 0 assumes a JDK is already present. Provisioning is the JDK layer.

Phase 0 `run` executes `Inspect`. Any other id is `TaskError.NoRunner`. `cancel` drops a fiber from a `Ref` registry. A test installs a slow task and cancels it. No compile task is pretended.

MCP tools are `graph`, `run`, and `cancel`. They call `Loader` and `Tasks`. The handler has no build logic. `./histos mcp` is the process the client owns. It is not a daemon. `./histos server` and BSP wait for the BSP layer.

## Stacks

GitHub stacks are strictly linear. One closed middle PR blocks everything above it. The order below is the whole roadmap. We open one stack at a time, bottom up, and we do not pre-create the later stacks.

Proposed stack A: `histos` Phase 0  (main <- design <- api <- load <- tasks <- cli <- mcp <- bootstrap)

1. `histos/design-decisions`: Record the four amendments in `docs/design.md`. Depends on: trunk.
   Verify: the native-image, sbt, and zipx-CI sentences now say script launcher, scala-cli, and no CI until dogfood.
2. `histos/api-graph`: Pure `ProjectGraph`, builder, literals, and validation. Depends on: layer 1 for the contract.
   Verify: `scala-cli test api`
3. `histos/load`: In-process compile and load of `histos.scala` against the packaged api jar. Depends on: layer 2 for the builder.
   Verify: `scala-cli test engine`
4. `histos/tasks`: Inspect runner, `NoRunner`, and the cancel registry. Depends on: layer 3 for a loaded graph.
   Verify: `scala-cli test engine`
5. `histos/cli`: Batch `graph`, `run`, and `version`, with exit codes mapped from the error enums. A direct match into `Command`. Depends on: layer 4.
   Verify: `scala-cli test app`
   zio-cli waits. Adopt it when the first real option lands (`cache clean --dry-run`, or `toolchain use --lock` against `--local`), and keep it as a parser into the same `Command` enum. It is 0.8 and would add `zio-streams` and `zio-process` before either is used.
6. `histos/mcp-acp`: heddle-mcp stdio for `graph`, `run`, and `cancel`. ACP answers `initialize`. Depends on: layer 4 for the task API. Independent of the CLI, but the stack is linear, so it sits above layer 5.
   Verify: `scala-cli test app` (in-memory pipe, not a live socket)
7. `histos/bootstrap`: `./histos`, `histos.bat`, and `.histos-version`. The script verifies a fixture assembly and execs `java -jar` when `java` exists. Depends on: layer 5 for the assembly it will eventually exec.
   Verify: `scala-cli test app` plus one POSIX script test against a local fixture. `histos.bat` is reviewed, not executed, until a Windows machine exists.

Not in stack A: scheduler, cache, JDK provisioning, Zinc, workflow render, Scala.js, Scala Native, publishing, BSP, remote cache.

Proposed stack B: Phase 1, opened after A has merged.

8. `histos/scheduler`: One action type, declared inputs, dependency order, interruption. A cache miss is "run locally". No remote path. Depends on: the task API.
   Verify: `scala-cli test engine` with fake runners.
9. `histos/cache`: Content-addressed store under the XDG cache (`~/.cache/histos`). Write to a temp name, verify, then move. `cache path`, `cache list`, `cache clean` (30 days, keep the current project, `--dry-run`). Nothing is deleted during a build. Depends on: layer 8, because keys are action digests. `--dry-run` is the first option, so this layer is where zio-cli replaces the match, parsing into `Command`.
   Verify: `scala-cli test engine`
10. `histos/jdk`: Lock file, the detection order in the design, and private Temurin only for a bare machine. The script downloads the locked JDK only when no `java` exists. The engine owns the policy and re-execs onto the chosen JDK. `toolchain` prints every candidate. Depends on: layer 7 for the script and layer 9 for the cache location.
    Verify: `scala-cli test engine` with fixture directories. No network in the test. Provisioning is tested against a local file URL.
11. `histos/zinc`: JVM compile for Scala 3.9.0 through Zinc. User classpath does not contain `scala3-compiler` or histos. Cache key includes sources, flags, the full Scala version, the platform, and the resolved JDK. Depends on: layers 9 and 10.
    Verify: `scala-cli test engine` compiles a fixture project twice and the second run is a hit. Look up the Zinc coordinate at implementation time; do not invent one.
12. `histos/resolve`: Coursier as a library, per design section 5.2. A `Dep`, a Scala version, and a platform become a classpath and `.histos/dependencies.lock`. Direct revisions are exact. Evictions are values. Compile reads the lock, downloads a missing locked jar, and does not re-solve. `--update` and `--check` are the solve commands. The compile cache key gains the artifact sha256s. JVM fetch works. Js and Native names are tested; their fetch is `UnsupportedPlatform`. Depends on: the graph and `Sha256`. Zinc already accepts the classpath.
    Verify: `scala-cli test engine` against a `file:` Maven repository. No network. One transitive, a rejected range, a checksum mismatch, a lock drift, and a Zinc compile of a source that calls a class from the resolved jar. Look up the Coursier coordinate at implementation time; do not invent one.
13. `histos/test-subprocess`: One subprocess per test task. Fresh work directory, scrubbed environment, classpath from the resolution plus the compiled classes. Interruption kills the process group. Depends on: layer 12 for that classpath and layer 11 for a compiled test.
    Verify: a fixture whose test calls `System.exit`, and a cancel that kills a sleeping test. That test is the proof. Do not add a theory about process groups without it.
14. `histos/ci-render`: Render `.github/workflows/ci.yml` from the graph. Empty split set is Aggregate and is the default. Drift fails the build. On a pull request the test job is the affected set. Depends on: layer 13, because the rendered command is `./histos test`.
    Verify: `scala-cli test engine` against a golden file and a drifted file.

Not in stack B: Scala.js, Scala Native, 2.13, Central, BSP.

Proposed stack C: Phase 2, after B merges.

15. `histos/scala-213`: Exact Scala 2.13 compile and test on the JVM, including `scala-2.13/` sources. Depends on: Zinc, the test subprocess, and the resolver's `_2.13` suffix.
    Verify: `scala-cli test engine`
16. `histos/scala-js`: Js platform. Node major and `.histos/node.lock`, same manager rules as the JDK, and only when a Js task runs. Resolution of a Js cell uses the `_sjs1` suffix from section 5.2. Depends on: layer 15 only for ordering; it needs the platform model from the api and the resolver.
    Verify: `scala-cli test engine` with a fixture Node, no network.
17. `histos/scala-native`: Native platform. Clang major check, no clang provisioning. Link in-process with `tools_3` `0.5.13-SNAPSHOT`. That resolve adds `IvyLocal`. Cache key includes the full clang version that matched. A miss is a typed error. Depends on: the scheduler, the cache, and the resolver.
    Verify: `scala-cli test engine` links a hello-world on this machine. This is the layer that needs the local Scala Native publish. Other machines cannot compile it until they have those jars or the pin moves to Central.
18. `histos/versions`: The zipx version scheme as tasks. Everyday `<version>-ci`, snapshot `<version>-<sha>-SNAPSHOT`, default-branch `<version>-SNAPSHOT` pointer, ship number, registry gate, catalog drift. No second scheme. Depends on: the task API and CI render.
    Verify: `scala-cli test engine` over the version transitions, no network publish.
19. `histos/central`: In-tree Central publisher module that compiles only against `api`. `Publisher` returns `ZIO[Scope, PublishError, Published]`. Credentials and signing are layers. Publish is local, uncached, and never sent to a remote executor. Depends on: layer 18 for the coordinates it publishes.
    Verify: `scala-cli test central` against a local repository fixture.

The general plugin classloader is not this layer. One in-tree module that only sees `api` is the pattern. The loader arrives when a second plugin exists.

Proposed stack D: Phase 3, after C merges.

20. `histos/bsp`: BSP server for Metals on the same task API. `./histos server` is the opt-in daemon. A batch command attaches when the fingerprint and Histos version match, otherwise it runs in-process. Depends on: compile and test.
    Verify: `scala-cli test app` against a BSP client fixture.
21. `histos/mcp-toolchain`: MCP gains `toolchain`. Depends on: the JDK and Node resolvers, and layer 6 for the server.
    Verify: `scala-cli test app`
22. `histos/remote-cache`: Opt-in remote cache for sendable compile actions. Miss runs locally. Read-only is the default. Publish, tests, `Network`, and `Docker` are not uploaded. A custom closure is not uploaded. A plugin runner is sendable only when the plugin says it is hermetic. Depends on: the local cache and Zinc.
    Verify: `scala-cli test engine` against an in-process fake cache. No real remote service.

Not in any of these stacks:

- Dogfood. Moving a library off sbt-zipx is a PR in that library. Stacks cannot span repos. Histos work that dogfood exposes is a new layer on trunk after D, not a guessed layer now.
- Test jail (filesystem and network) before any test is sendable. It is the first layer of the next stack after dogfood shows the subprocess is the one we jail. Remote execution stays off.
- Plugin loader (`ExactBinary`, `Source`, `BinaryOrSource`). `LinkStrategy.Api` stays a typed error until 1.0. After the loader merges, docs, docker, deploy, coverage, and the dependency-update bot are separate PRs. They do not form one stack: they share the loader and nothing else.
- sbt importer. The design holds it until a second repo repeats the shape.
- MiMa on `histos.api`. That is the 1.0 cut, not a 0.x layer.
- Publishing Histos itself. No snapshot and no Central release until you say the feature is complete.
- sbt, sbt-scala-native, a Scala Native launcher, and a Rust launcher.

## After approval

Implement stack A only, bottom up. Each layer is committed and verified with the command above before the next branch is added. Do not open stacks B through D in the same session. Do not merge. You merge.

