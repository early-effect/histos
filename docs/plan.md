# Histos implementation stack

The design in `docs/design.md` stands, with the amendments below. The first four are layer 1, so later layers are not implementing a document that still says the opposite. The fifth is the dependency-resolution specification in design section 5.2.

## Amendments

1. The launcher is the frozen `./histos` and `histos.bat`. They download the pinned JVM assembly, verify it, and exec it. There is no Scala Native launcher and no Rust launcher. A native image is bloat for a downloader, and a second language is the same cost. JDK policy lives in the JVM engine. The script only fetches the locked Temurin when no `java` exists, then the engine re-checks the lock and re-execs if needed.
2. Histos builds itself. `./histos test` and `./histos assembly` are the build. `dist/histos.jar` is local and is not committed. A module is compiled by the compiler it names, fetched with Coursier: any Scala 3 version, any Scala 2.13 patch, and nothing older. No sbt. No zipx. No scala-cli. No GitHub Actions on this repo until dogfood.
3. The engine is a JVM process. ZIO, load, Zinc, MCP, and the Scala Native linker (`tools_3`) run there.
4. Scala Native is a target toolchain, not a build plugin. The jars already in `~/.ivy2/local` are `org.scala-native` `0.5.13-SNAPSHOT` (`tools_3`, `nir_3`, `util_3`, `nscplugin_3.9.0`, `nativelib_native0.5_3`, `scala3lib` / `scalalib` at `3.9.0+0.5.13-SNAPSHOT`). That nativelib is the Mac Mini M6 unwind fix (scala-native#5109). Histos fetches those jars. `IvyLocal` is added for that resolve and is not a default repository. `IvyLocal` is not a default repository. When 0.5.13 is on Maven Central, that layer drops `IvyLocal` and the snapshot pin.
5. Library resolution is Coursier linked as a library. The `cs` program is not a resolver. The specification is section 5.2 of the design. The layer sits after Zinc and before the test subprocess, because a classpath of real dependencies has to exist before a test is defined as running on one.

`heddle-mcp` is the MCP server. ACP `initialize` is a small handshake in histos. There is no ACP library.

## Shape

```
api/      pure graph, builder, literals. No ZIO on the main classpath.
engine/   Load, tasks, later scheduler, cache, Zinc, resolution, native link.
app/      CLI, MCP, ACP. One ZIOApp.
```

`histos.scala` is the build. `api`, `central`, `engine`, and `app` are modules. `dependsOn` is the classpath. There is no `project.scala`.

Tests are ZIO Test (`zio-test` and `zio-test-sbt` as test deps, framework `zio.test.sbt.ZTestFramework`). The api jar does not depend on ZIO. Verify with `./histos test`. There is no sbt, so `testFull` does not apply. No CI cost until dogfood: each layer is proven locally. Do not add required checks before a workflow exists, or the stack cannot merge.

Packages: `histos.*` is the DSL. Engine code is `histos.load`, `histos.task`, and later `histos.cache`, `histos.zinc`, `histos.resolve`, `histos.native`. App code is `histos.cli`, `histos.mcp`, `histos.acp`, and `histos.bsp`.

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

`LoadedBuild` holds the graph and the build classloader. The `Scope` closes the classloader. Load uses the Scala 3.10.0 compiler in-process (`scala3-compiler`). That compiler loads the build file. It is not the compiler that compiles a module, and it is not on the classpath of the project being built. Phase 0 assumes a JDK is already present. Provisioning is the JDK layer.

Phase 0 `run` executes `Inspect`. Any other id is `TaskError.NoRunner`. `cancel` drops a fiber from a `Ref` registry. A test installs a slow task and cancels it. No compile task is pretended.

MCP tools are `graph`, `run`, and `cancel`. They call `Loader` and `Tasks`. The handler has no build logic. `./histos mcp` is the process the client owns. It is not a daemon.

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
18. `histos/versions`: The zipx version scheme as tasks. Everyday `<version>-ci`, snapshot `<version>-<sha>-SNAPSHOT`, default-branch `<version>-SNAPSHOT` pointer, ship number, registry gate, catalog drift. That scheme is the default. `manual("app", manual"1.2.3")` is the other choice: the stated version, with no git and no registry. A catalog does not mix them. Depends on: the task API and CI render.
    Verify: `scala-cli test engine` over the version transitions, no network publish.
19. `histos/central`: In-tree Central publisher module that compiles only against `api`. `Publisher` returns `ZIO[Scope, PublishError, Published]`. Credentials and signing are layers. Publish is local, uncached, and never sent to a remote executor. Depends on: layer 18 for the coordinates it publishes.
    Verify: `scala-cli test central` against a local repository fixture.

The general plugin classloader is not this layer. One in-tree module that only sees `api` is the pattern. The loader arrives when a second plugin exists.

The library loop is in place. `histos compile` compiles JVM cells from the lock and `dependsOn`. `histos publish <repository>` jars those classes and writes a local Maven directory, with the Scala binary suffix and pom dependencies. It does not publish a development coordinate. A second project resolving that directory is the loop. CI does not render a publish job: a file repository is not a release.

`histos test` compiles, then runs the module's test provider in one subprocess per Scala version. ZIO Test is that provider unless the module names another `TestProvider` or opts out with a main. The engine does not load the provider. A module with no test sources reports `no tests`.

Self-build comes before stack D. A published local assembly has to compile, test, and package this repo, so scala-cli stops being the build. That assembly is not a Maven Central release. BSP, the agent toolchain, and the remote cache follow, and they stay in the plan.

20. `histos/test-classpath`: `libraryDependencies` stay the compile classpath and the published pom. `testDependencies` are a second lock cell, `test <module> <scala> <platform>`, resolved together with the library dependencies so one eviction applies. A module with no test dependencies has no test cell. `histos compile` and `histos publish` do not read it. `histos test` does. Depends on: the lock, compile, and test.
    Verify: `scala-cli test api` and `scala-cli test engine`.
21. `histos/zio-test`: Test providers are pluggable. A provider names an sbt framework class and the test-scoped library that supplies it. `TestProvider.Zio` is the default. A named main opts out. The child loads the framework. The engine does not. `no tests` stays the answer when there is nothing to run. Depends on: the test classpath.
    Verify: `scala-cli test engine` runs a ZIO Test suite in the child.
22. `histos/native-version`: A native module may name `nativeVersion`. Unset means `0.5.12`. Modules may name different revisions. Each link runs in a child against that revision's tools. The engine does not load `scala.scalanative`. `HISTOS_NATIVE` is only the origin (`ivy-local` or `file:/path`). `_native0.5` stays the ABI suffix. Scala.js gets the same treatment when its linker lands. Depends on: the native linker.
    Verify: `scala-cli test engine` links with the default, fetches a second revision, and the engine classloader cannot see the linker.
23. `histos/assembly`: Package `app` and its runtime classpath into the one jar `./histos` execs, with main `histos.cli.Main`. Application classes and dependency entries share that jar: application classes win, the first dependency wins, `META-INF/services` entries are concatenated, and signature files and `module-info.class` are omitted. Scala Native tools are not in the jar. The launcher and a JDK re-exec pass `--enable-native-access=ALL-UNNAMED`. Depends on: the class jar.
    Verify: `scala-cli test app` execs that jar far enough to print the version.
24. `histos/self`: A root `histos.scala` for `api`, `central`, `engine`, and `app`, wired with `dependsOn`. `./histos test` runs the suites. `project.scala` is gone. Depends on: layers 20 through 23.
    Verify: the rebuilt assembly prints the same version, and `./histos test` runs these suites.

Stack D, after this repo builds itself:

25. `histos/bsp`: BSP 2.2 over stdio for Metals. `./histos bsp` speaks `Content-Length` frames and answers `build/initialize`, `workspace/buildTargets`, `buildTarget/sources`, `buildTarget/scalacOptions`, and `buildTarget/compile`, then `build/shutdown` and `build/exit`. `.bsp/histos.json` names that command. One JVM target per module and Scala version. Js and Native are omitted. Compile names that cell, not the module's other versions. A missing lock is a protocol error. Compile failure is status 2. The server does not advertise test or run, and it does not publish diagnostics. Depends on: compile.
    Verify: `./histos test app` against a BSP client fixture.
26. `histos/server`: `./histos server` is the opt-in daemon. `compile`, `test`, and `bsp` attach when the Histos version and the fingerprint of `histos.scala` plus the dependency lock match. A missing server, a dead socket, or a stale record runs in-process. Depends on: layer 25, so Metals and the batch command share one process and the on-disk analysis. Zinc 2.0.4's compiler cache is a fresh compiler on every compile.
    Verify: `./histos test app` attaches a second command to a running server and misses a stale fingerprint.
27. `histos/mcp-toolchain`: MCP gains `toolchain`. The tool returns the JDK decision, and the Node or clang decision when the graph needs them. The CLI prints that same decision. Depends on: the JDK and Node resolvers, and layer 6 for the server.
    Verify: `./histos test app`
28. `histos/remote-cache`: Opt-in remote cache for sendable compile actions. Miss runs locally. Read-only is the default. Publish, tests, `Network`, and `Docker` are not uploaded. A custom closure is not uploaded. A plugin runner is sendable only when the plugin says it is hermetic. Depends on: the local cache and Zinc.
    Verify: `./histos test engine` against an in-process fake cache. No real remote service.

Not in any of these stacks:

- Moving a library other than this repo off sbt. Self-build of Histos is layers 20 through 24. Another library is a PR in that library after this repo builds itself.
- Test jail (filesystem and network) before any test is sendable. It is the first layer of the next stack after dogfood shows the subprocess is the one we jail. Remote execution stays off.
- Plugin loader (`ExactBinary`, `Source`, `BinaryOrSource`). `LinkStrategy.Api` stays a typed error until 1.0. After the loader merges, docs, docker, deploy, coverage, and the dependency-update bot are separate PRs. They do not form one stack: they share the loader and nothing else.
- sbt importer. The design holds it until a second repo repeats the shape.
- MiMa on `histos.api`. That is the 1.0 cut, not a 0.x layer.
- Publishing Histos itself. No snapshot and no Central release until you say the feature is complete.
- sbt, sbt-scala-native, a Scala Native launcher, and a Rust launcher.

## After approval

Stacks A through C landed as those layers. Self-build, layers 20 through 24, has merged. The active sequence is stack D. Do not merge. You merge.

